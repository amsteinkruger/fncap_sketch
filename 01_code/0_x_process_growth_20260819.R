# Wrangle FIA and estimate growth models.

#  Data

#   Plots

dat_plot =
  "02_data/1_5_1_FIA/OR_PLOT.csv" %>%
  read_csv %>%
  select(
    INVYR,
    MEASYEAR,
    PLT_CN = CN,
    STATECD,
    COUNTYCD,
    LAT,
    LON
  )

#   Conditions
  
dat_condition =
  "02_data/1_5_1_FIA/OR_COND.csv" %>%
  read_csv %>%
  select(
    INVYR,
    PLT_CN,
    CON_CN = CN,
    CONDID,
    STDAGE,
    SITECLCD,
    FORTYPCD,
    OWNGRPCD
  )

#   Trees

dat_tree =
  "02_data/1_5_1_FIA/OR_TREE.csv" %>%
  read_csv %>% 
  select(
    INVYR,
    PLT_CN,
    CONDID,
    TRE_CN = CN,
    TPA_UNADJ,
    VOLBFNET,
    SPCD)

#   Tree Growth Estimation

dat_growth = 
  "02_data/1_5_1_FIA/OR_TREE_GRM_ESTN.csv" %>% 
  read_csv %>% 
  filter(COMPONENT == "SURVIVOR") %>% 
  filter(ESTIMATE == "VOLBFNET") %>% 
  filter(ANN_NET_GROWTH > 0) %>% 
  mutate(EST_BEGIN_ACRE = EST_BEGIN * TPAGROW_UNADJ,
         EST_END_ACRE = EST_END * TPAGROW_UNADJ,
         ANN_NET_GROWTH_ACRE = ANN_NET_GROWTH * TPAGROW_UNADJ) %>% 
  select(INVYR, PLT_CN, TRE_CN, EST_BEGIN_ACRE, EST_END_ACRE, ANN_NET_GROWTH_ACRE, REMPER)

#   Wrangle

#    Pick age range.

vec_stdage = 1:75

dat_use = 
  # Handle tree data.
  dat_tree %>% 
  filter(!is.na(VOLBFNET)) %>%
  filter(SPCD %in% c(202, 263)) %>%
  mutate(VOLBFNET_ACRE = VOLBFNET * TPA_UNADJ) %>% 
  # Handle growth estimation data.
  left_join(dat_growth) %>% 
  distinct %>% # Why do observations duplicate on join? 
  # Reduce to conditions. 
  group_by(
    INVYR,
    PLT_CN,
    CONDID,
    SPCD
  ) %>% 
  summarize(
    VOLBFNET_ACRE = sum(VOLBFNET_ACRE, na.rm = TRUE),
    EST_BEGIN_ACRE = sum(EST_BEGIN_ACRE, na.rm = TRUE),
    EST_END_ACRE = sum(EST_END_ACRE, na.rm = TRUE),
    ANN_NET_GROWTH_ACRE = sum(ANN_NET_GROWTH_ACRE, na.rm = TRUE),
    REMPER_MEAN = mean(REMPER, na.rm = TRUE)
    ) %>% 
  ungroup %>% 
  # Handle units. 
  mutate(across(c(ends_with("ACRE"), "REMPER_MEAN"), ~ ifelse(.x == 0, NA, .x)),
         across(ends_with("ACRE"), ~ .x / 1000)) %>% # BF to MBF. 
  # Handle remeasurement periods.
  mutate(across(REMPER_MEAN, ~ ifelse(.x == 0, NA, .x)),
         REMPER_ROUND = REMPER_MEAN %>% round(0),
         REMPER_LOW = REMPER_MEAN %>% floor,
         REMPER_HIGH = REMPER_MEAN %>% ceiling) %>% # BF to MBF. 
  # Cut western hemlock for now. 
  filter(SPCD == 202) %>% 
  # Handle condition data.
  left_join(dat_condition) %>% 
  filter(FORTYPCD == 201) %>% # Cuts to Douglas fir stands only. 
  filter(OWNGRPCD == 40) %>% 
  filter(STDAGE %!in% c(NA, 0, 998, 999)) %>% 
  # Handle plot data.
  left_join(dat_plot) %>% 
  # Cut stands older than 75 years for now.
  filter(STDAGE %in% 1:75) %>%
  # Handle outliers.
  filter(ntile(VOLBFNET_ACRE, 100) %in% 2:99) %>% 
  filter(ntile(VOLBFNET_ACRE / STDAGE, 100) %in% 2:99) %>% 
  filter(ntile(EST_BEGIN_ACRE, 100) %in% 2:99 | is.na(EST_BEGIN_ACRE)) %>% 
  filter(ntile(ANN_NET_GROWTH_ACRE, 100) %in% 2:99 | is.na(ANN_NET_GROWTH_ACRE)) %>%
  filter(ntile(ANN_NET_GROWTH_ACRE / VOLBFNET_ACRE, 100) %in% 2:99 | is.na(ANN_NET_GROWTH_ACRE)) # %>% 

# Estimation

#  OLS Demo

library(nloptr) # Remember to kick this into packages.R if it works. 

mod_ols_initial =
  dat_use %>% 
  lm(ANN_NET_GROWTH_ACRE ~ EST_BEGIN_ACRE, data = .) 

par_ols_initial = mod_ols_initial %>% coef

fun_ols_inner =
  function(t, start, par){
    
    result = 
      Reduce(
        f = function(yield, i) yield + yield * par[[2]],
        seq_len(t),
        init = start,
        accumulate = TRUE
      ) %>% 
      subtract(start) %>% 
      sum
    
    return(result)

  }

fun_ols_outer_yield = 
  function(par){
    
    residuals_yield = 
      dat_use %>% # Note global call. 
      drop_na(EST_BEGIN_ACRE) %>% 
      mutate(
        EST_END_ACRE_HAT = 
          map2(
            REMPER_ROUND, 
            EST_BEGIN_ACRE, 
            ~ fun_ols_inner(.x, .y, par_ols_initial)
          )
      ) %>% 
      unnest(EST_END_ACRE_HAT) %>% 
      mutate(EST_END_ACRE_RESIDUAL = EST_END_ACRE_HAT - EST_END_ACRE) %>% 
      pull(EST_END_ACRE_RESIDUAL) %>% 
      raise_to_power(2) %>% 
      divide_by(sd(.)) %>% # Normalization by standard deviation of residuals. 
      divide_by(length(.)) %>% # Weighting by observations. 
      sum(na.rm = TRUE)
    
    return(residuals_yield)
    
  }

mod_ols_nloptr_yield = 
  nloptr(
    par_ols_initial,
    fun_ols_outer_yield,
    opts = list("algorithm" = "NLOPT_LN_COBYLA")
  )

par_ols_yield = mod_ols_nloptr_yield$solution

fun_ols_outer_growth = 
  function(par){
    
    residuals_growth = 
      dat_use %>% # Note global call. 
      drop_na(STDAGE) %>% 
      mutate(VOLBFNET_ACRE_HAT = 
               map2(
                 STDAGE, 
                 par[[1]], 
                 ~ fun_ols_inner(.x, .y, par_ols_initial)
               )
      ) %>% 
      unnest(VOLBFNET_ACRE_HAT) %>% 
      mutate(VOLBFNET_ACRE_RESIDUAL = VOLBFNET_ACRE_HAT - VOLBFNET_ACRE) %>% 
      pull(VOLBFNET_ACRE_RESIDUAL) %>% 
      raise_to_power(2) %>% 
      divide_by(sd(.)) %>% # Normalization by standard deviation of residuals. 
      divide_by(length(.)) %>% # Weighting by observations. 
      sum(na.rm = TRUE)
    
    return(residuals_growth)
    
  }

mod_ols_nloptr_growth = 
  nloptr(
    par_ols_initial,
    fun_ols_outer_growth,
    opts = list("algorithm" = "NLOPT_LN_COBYLA")
  )

par_ols_growth = mod_ols_nloptr_growth$solution

fun_ols_outer_combined = 
  function(par){
    
    residuals_yield = 
      dat_use %>% # Note global call. 
      drop_na(EST_BEGIN_ACRE) %>% 
      mutate(
        EST_END_ACRE_HAT = 
          map2(
            REMPER_ROUND, 
            EST_BEGIN_ACRE, 
            ~ fun_ols_inner(.x, .y, par_ols_initial)
          )
      ) %>% 
      unnest(EST_END_ACRE_HAT) %>% 
      mutate(EST_END_ACRE_RESIDUAL = EST_END_ACRE_HAT - EST_END_ACRE) %>% 
      pull(EST_END_ACRE_RESIDUAL) %>% 
      raise_to_power(2) %>% 
      divide_by(sd(.)) %>% # Normalization by standard deviation of residuals. 
      divide_by(length(.)) %>% # Weighting by observations. 
      sum(na.rm = TRUE)
    
    residuals_growth = 
      dat_use %>% # Note global call. 
      drop_na(STDAGE) %>% 
      mutate(VOLBFNET_ACRE_HAT = 
               map2(
                 STDAGE, 
                 par[[1]], 
                 ~ fun_ols_inner(.x, .y, par_ols_initial)
               )
      ) %>% 
      unnest(VOLBFNET_ACRE_HAT) %>% 
      mutate(VOLBFNET_ACRE_RESIDUAL = VOLBFNET_ACRE_HAT - VOLBFNET_ACRE) %>% 
      pull(VOLBFNET_ACRE_RESIDUAL) %>% 
      raise_to_power(2) %>% 
      divide_by(sd(.)) %>% # Normalization by standard deviation of residuals. 
      divide_by(length(.)) %>% # Weighting by observations. 
      sum(na.rm = TRUE)
      
    residuals_combined = residuals_yield + residuals_growth
    
    return(residuals_combined)
    
  }

mod_ols_nloptr_combined = 
  nloptr(
    par_ols_initial,
    fun_ols_outer_combined,
    opts = list("algorithm" = "NLOPT_LN_COBYLA")
  )

par_ols_combined = mod_ols_nloptr_combined$solution

# OLS Visualization

vis_ols_yield = 
  dat_use %>% 
  drop_na(EST_BEGIN_ACRE) %>% 
  mutate(
    EST_END_ACRE_HAT_YIELD = map2(REMPER_ROUND, EST_BEGIN_ACRE, ~ fun_ols_inner(.x, .y, par_ols_yield)),
    EST_END_ACRE_HAT_GROWTH = map2(REMPER_ROUND, EST_BEGIN_ACRE, ~ fun_ols_inner(.x, .y, par_ols_growth)),
    EST_END_ACRE_HAT_COMBINED = map2(REMPER_ROUND, EST_BEGIN_ACRE, ~ fun_ols_inner(.x, .y, par_ols_combined))
  ) %>% 
  unnest(starts_with("EST_END_ACRE_HAT")) %>% 
  select(EST_BEGIN_ACRE, starts_with("EST_END_ACRE")) %>% 
  pivot_longer(-EST_BEGIN_ACRE) %>% 
  ggplot() + 
  geom_point(aes(x = EST_BEGIN_ACRE,
                 y = value,
                 color = name),
             alpha = 0.33)

vis_ols_growth = 
  dat_use %>% 
  drop_na(STDAGE) %>% 
  mutate(
    VOLBFNET_ACRE_HAT_YIELD = map2(STDAGE, par_ols_yield[[1]], ~ fun_ols_inner(.x, .y, par_ols_yield)),
    VOLBFNET_ACRE_HAT_GROWTH = map2(STDAGE, par_ols_growth[[1]], ~ fun_ols_inner(.x, .y, par_ols_growth)),
    VOLBFNET_ACRE_HAT_COMBINED = map2(STDAGE, par_ols_combined[[1]], ~ fun_ols_inner(.x, .y, par_ols_combined))
  ) %>% 
  unnest(starts_with("VOLBFNET_ACRE_HAT")) %>% 
  select(STDAGE, starts_with("VOLBFNET_ACRE")) %>% 
  pivot_longer(-STDAGE) %>% 
  ggplot() + 
  geom_point(aes(x = STDAGE,
                 y = value,
                 color = name),
             alpha = 0.33)

vis_ols_yield + vis_ols_growth
