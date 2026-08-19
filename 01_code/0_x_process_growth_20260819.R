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

# P-T Implementation

mod_pt_initial =
  dat_use %>% 
  drop_na(EST_BEGIN_ACRE) %>% 
  nls(
    ANN_NET_GROWTH_ACRE ~ a * (1 - (EST_BEGIN_ACRE / b) ^ c),
    data = .,
    # start = list(a = 0.1, b = 0.1, c = 0.500),
    start = list(a = 1, b = 1, c = 1),
    algorithm = "port",
    lower = c(a = 1e-4, b = 1e-4, c = 1e-4),
    nls.control(maxiter = 100)
  )
  
par_pt_initial = mod_pt_initial %>% coef

ggplot(data = dat_use) +
  geom_point(aes(x = EST_BEGIN_ACRE, y = ANN_NET_GROWTH_ACRE)) +
  geom_point(aes(x = EST_BEGIN_ACRE, y = (par_pt_initial[1] * (1 + (EST_BEGIN_ACRE / par_pt_initial[2]) ^ par_pt_initial[3]))), color = "red")

fun_pt_iterate = 
  function(times, par) {
    
    Reduce(
      function(V_0, i) V_0 + par[1] * (1 - (V_0 / par[2]) ^ par[3]),
      seq_len(times),
      init = 4
    )
  }

fun_pt_growth = 
  function(par){
    
    residuals_yield = 
      dat_use %>% # Note global call. 
      pull(EST_BEGIN_ACRE) %>% 
      {par[1] * (1 - (. / par[2]) ^ par[3])} %>% 
      subtract(dat_use$ANN_NET_GROWTH_ACRE) %>% 
      raise_to_power(2) %>% 
      divide_by(length(.)) %>% # Weighting by observations. 
      sum(na.rm = TRUE)
    
    residuals_growth = 
      dat_use %>% 
      pull(STDAGE) %>% 
      map(~ fun_pt_iterate(.x, par)) %>% 
      unlist %>% 
      subtract(dat_use$EST_BEGIN_ACRE) %>% 
      subtract(dat_use$ANN_NET_GROWTH_ACRE) %>% 
      raise_to_power(2) %>% 
      divide_by(length(.)) %>% # Weighting by observations. 
      sum(na.rm = TRUE)
    
    residuals_yield + residuals_growth
    
  }

dat_pt_initial = fun_pt_growth(par_pt_initial)

mod_pt_optimizing = 
  nloptr(
    par_pt_initial,
    fun_pt_growth,
    lb = c(1e-4, 1e-4, 1e-4),
    opts = list("algorithm" = "NLOPT_LN_COBYLA", maxeval = 100)
  )

par_pt_optimized = mod_pt_optimizing$solution

# PT Visualization

vis_pt_yield = 
  dat_use %>% 
  drop_na(ANN_NET_GROWTH_ACRE) %>% 
  mutate(
    ANN_NET_GROWTH_ACRE_HAT_NAIVE = par_pt_initial[1] * (1 - (EST_BEGIN_ACRE / par_pt_initial[2]) ^ par_pt_initial[3]),
    ANN_NET_GROWTH_ACRE_HAT_OPTIMIZED = par_pt_optimized[1] * (1 - (EST_BEGIN_ACRE / par_pt_optimized[2]) ^ par_pt_optimized[3])
  ) %>% 
  select(EST_BEGIN_ACRE, starts_with("ANN_NET_GROWTH_ACRE")) %>% 
  pivot_longer(-EST_BEGIN_ACRE) %>% 
  ggplot() + 
  geom_point(aes(x = EST_BEGIN_ACRE,
                 y = value,
                 color = name),
             alpha = 0.50) +
  labs(x = "EST_BEGIN_ACRE", y = "ANN_NET_GROWTH_ACRE") +
  scale_color_manual(values = c("#000000", "red", "blue")) +
  theme_pubr() +
  theme(legend.position = "bottom",
        legend.direction = "vertical",
        legend.title = element_blank())

vis_pt_growth = 
  dat_use %>% 
  mutate(
    VOLBFNET_ACRE_HAT_NAIVE = STDAGE %>% map(~ fun_pt_iterate(.x, par_pt_initial)),
    VOLBFNET_ACRE_HAT_OPTIMIZED = STDAGE %>% map(~ fun_pt_iterate(.x, par_pt_optimized))
  ) %>%
  unnest(c(VOLBFNET_ACRE_HAT_NAIVE, VOLBFNET_ACRE_HAT_OPTIMIZED)) %>%
  # mutate(
  #   EST_ANNUAL_ACRE = EST_BEGIN_ACRE + ANN_NET_GROWTH_ACRE,
  #   EST_ANNUAL_ACRE_HAT_NAIVE = STDAGE %>% map(~ fun_pt_iterate(.x, par_pt_initial)), 
  #   EST_ANNUAL_ACRE_HAT_OPTIMIZED = STDAGE %>% map(~ fun_pt_iterate(.x, par_pt_optimized))
  # ) %>% 
  # unnest(c(EST_ANNUAL_ACRE_HAT_NAIVE, EST_ANNUAL_ACRE_HAT_OPTIMIZED)) %>% 
  select(STDAGE, starts_with("VOLBFNET_ACRE")) %>% 
  pivot_longer(-STDAGE) %>% 
  ggplot() + 
  geom_point(aes(x = STDAGE,
                 y = value,
                 color = name),
             alpha = 0.50) +
  labs(x = "STDAGE", y = "EST_ANNUAL_ACRE") +
  scale_color_manual(values = c("#000000", "red", "blue")) +
  theme_pubr() +
  theme(legend.position = "bottom",
        legend.direction = "vertical",
        legend.title = element_blank())

vis_pt_yield + vis_pt_growth

ggsave("04_out/Other/vis_combined.png",
       dpi = 300,
       width = 8,
       height = 5)

# Problems:
#  (1) The combined model is dragging the yield curve implausibly far from the best separate fit.  
#  (2) The combined model isn't actually converging in 1000 evaluations. 
#  (3) ???

# Things to try:
#  (1) Split on site class.
#      SITECLCD is not driving the difference in fit. Neither is VOLBFNET_ACRE vs. EST_BEGIN_ACRE + ANN_NET_GROWTH_ACRE (much).
#  (2) Try alternative functional forms. 
#  (3) Try alternative nonlinear optimization programs.


# To generalize over regions, site classes, etc., refer to earlier modeling script. 
