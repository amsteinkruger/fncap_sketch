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
    SPCD,
    BHAGE)

#   Tree Growth Estimation

dat_growth = 
  "02_data/1_5_1_FIA/OR_TREE_GRM_ESTN.csv" %>% 
  read_csv %>% 
  filter(COMPONENT == "SURVIVOR") %>% 
  filter(ESTIMATE == "VOLBFNET") %>% 
  filter(EST_BEGIN < EST_END) %>% 
  mutate(EST_BEGIN_ACRE = EST_BEGIN * TPAGROW_UNADJ,
         EST_END_ACRE = EST_END * TPAGROW_UNADJ) %>% 
  select(INVYR, PLT_CN, TRE_CN, EST_BEGIN_ACRE, EST_END_ACRE, REMPER)

#   Wrangle

dat_use = 
  # Handle tree data.
  dat_tree %>% 
  filter(!is.na(VOLBFNET)) %>%
  # filter(SPCD %in% c(202, 263)) %>% # Filter to Douglas fir and western hemlock. 
  mutate(VOLBFNET_ACRE = VOLBFNET * TPA_UNADJ) %>% 
  # Add growth estimation data.
  left_join(dat_growth) %>% 
  distinct %>% # Why do observations duplicate on join? 
  # Reduce to conditions. 
  group_by(INVYR, PLT_CN, CONDID) %>% 
  # filter(n_distinct(SPCD) == 1 & SPCD == 202) %>% # Filter to single-species Douglas fir conditions. 
  summarize(
    VOLBFNET_ACRE = sum(VOLBFNET_ACRE, na.rm = TRUE),
    EST_BEGIN_ACRE = sum(EST_BEGIN_ACRE, na.rm = TRUE),
    EST_END_ACRE = sum(EST_END_ACRE, na.rm = TRUE),
    REMPER_MEAN = mean(REMPER, na.rm = TRUE)
    ) %>% 
  ungroup %>% 
  # Handle units. 
  mutate(across(c(ends_with("ACRE"), "REMPER_MEAN"), ~ ifelse(.x == 0, NA, .x)),
         across(ends_with("ACRE"), ~ .x / 1000), # BF to MBF. 
         EST_CHANGE_ACRE = EST_END_ACRE - EST_BEGIN_ACRE) %>% 
  relocate(EST_CHANGE_ACRE, .after = EST_END_ACRE) %>% 
  # Handle remeasurement periods.
  mutate(across(REMPER_MEAN, ~ ifelse(.x == 0, NA, .x)),
         REMPER_ROUND = REMPER_MEAN %>% round(0),
         REMPER_LOW = REMPER_MEAN %>% floor,
         REMPER_HIGH = REMPER_MEAN %>% ceiling) %>% 
  # Handle condition data.
  left_join(dat_condition) %>% 
  filter(FORTYPCD == 201) %>% # Filter to Douglas fir conditions.
  filter(OWNGRPCD == 40) %>% # Filter to private conditions.  
  filter(SITECLCD %in% 1:4) %>% # Filter to more productive site classes.
  filter(STDAGE %!in% c(0, 998, 999)) %>% # Filter to nonproblematic stand ages.
  # Handle plot data.
  left_join(dat_plot) %>% 
  # Cut stands older than 75 years for now.
  filter(STDAGE %in% 1:75) %>%
  # Handle outliers.
  filter(ntile(VOLBFNET_ACRE, 100) %in% 2:99) %>% 
  filter(ntile(EST_CHANGE_ACRE, 100) %in% 2:99 | is.na(EST_CHANGE_ACRE)) 

#  Estimation

#   Functions

#    Inner

fun_pt_inner =
  function(t, start, par){
    
    result = 
      Reduce(
        f = function(yield, i) yield + par[2] * yield * (1 - (yield / par[3]) ^ par[4]),
        seq_len(t),
        init = start
      ) %>% 
      subtract(start)
    
    return(result)

  }

#    Outer

fun_pt_outer_initial = 
  function(par){
    
    residuals = 
      dat_vb %>% # Note global call. 
      drop_na(STDAGE) %>% 
      mutate(VOLBFNET_ACRE_HAT = map(STDAGE, ~ fun_pt_inner(.x, par[[1]], par))) %>% 
      unnest(VOLBFNET_ACRE_HAT) %>% 
      mutate(VOLBFNET_ACRE_RESIDUAL = (VOLBFNET_ACRE_HAT - VOLBFNET_ACRE_VB)) %>%  # / VOLBFNET_ACRE_VB) %>% # Normalize to percent deviation.
      pull(VOLBFNET_ACRE_RESIDUAL) %>% 
      raise_to_power(2) %>% 
      sum(na.rm = TRUE)
    
    return(residuals)
    
  }

fun_pt_outer_growth = 
  function(par){
    
    residuals_growth = 
      dat_use %>% # Note global call. 
      drop_na(STDAGE) %>% 
      mutate(VOLBFNET_ACRE_HAT = map(STDAGE, ~ fun_pt_inner(.x, par[[1]], par))) %>% 
      unnest(VOLBFNET_ACRE_HAT) %>% 
      mutate(VOLBFNET_ACRE_RESIDUAL = (VOLBFNET_ACRE_HAT - VOLBFNET_ACRE)) %>% # / VOLBFNET_ACRE) %>% # Normalize to percent deviation.
      pull(VOLBFNET_ACRE_RESIDUAL) %>% 
      raise_to_power(2) %>% 
      sum(na.rm = TRUE)
    
    return(residuals_growth)
    
  }

fun_pt_outer_yield = 
  function(par){
    
    residuals_yield = 
      dat_use %>% # Note global call. 
      drop_na(EST_BEGIN_ACRE) %>% 
      mutate(
        EST_CHANGE_ACRE_HAT = 
          map2(
            REMPER_ROUND, 
            EST_BEGIN_ACRE, 
            ~ fun_pt_inner(.x, .y, par)
          )
      ) %>% 
      unnest(EST_CHANGE_ACRE_HAT) %>% 
      mutate(EST_CHANGE_ACRE_RESIDUAL = (EST_CHANGE_ACRE_HAT - EST_CHANGE_ACRE)) %>% # / EST_CHANGE_ACRE) %>% # Normalize to percent deviation. 
      pull(EST_CHANGE_ACRE_RESIDUAL) %>%
      raise_to_power(2) %>%
      sum(na.rm = TRUE)
    
    return(residuals_yield)
    
  }

fun_pt_outer_combined = 
  function(par){
    
    residuals_yield = 
      dat_use %>% # Note global call. 
      drop_na(EST_BEGIN_ACRE) %>% 
      mutate(
        EST_CHANGE_ACRE_HAT = 
          map2(
            REMPER_ROUND, 
            EST_BEGIN_ACRE, 
            ~ fun_pt_inner(.x, .y, par)
          )
      ) %>% 
      unnest(EST_CHANGE_ACRE_HAT) %>% 
      mutate(EST_CHANGE_ACRE_RESIDUAL = (EST_CHANGE_ACRE_HAT - EST_CHANGE_ACRE)) %>% # / EST_CHANGE_ACRE) %>% # Normalize to percent deviation. 
      pull(EST_CHANGE_ACRE_RESIDUAL) %>%
      raise_to_power(2) %>%
      divide_by(length(.)) %>% # Weight by observations.
      sum(na.rm = TRUE)
    
    residuals_growth = 
      dat_use %>% # Note global call. 
      drop_na(STDAGE) %>% 
      mutate(VOLBFNET_ACRE_HAT = map(STDAGE, ~ fun_pt_inner(.x, par[[1]], par))) %>% 
      unnest(VOLBFNET_ACRE_HAT) %>% 
      mutate(VOLBFNET_ACRE_RESIDUAL = (VOLBFNET_ACRE_HAT - VOLBFNET_ACRE)) %>% # / VOLBFNET_ACRE) %>% # Normalize to percent deviation.
      pull(VOLBFNET_ACRE_RESIDUAL) %>% 
      raise_to_power(2) %>% 
      divide_by(length(.)) %>% # Weighting by observations. 
      sum(na.rm = TRUE)
    
    residuals_combined = residuals_yield + residuals_growth
    
    return(residuals_combined)
    
  }

#   Initialization via VBG. 

mod_vb = 
  nls(
    VOLBFNET_ACRE ~ a * (1 - exp(- b * STDAGE)) ^ 3,
    data = dat_use,
    start = list(a = 50, b = 0.05)
  )

par_vb = mod_vb %>% coef

dat_vb = 
  dat_use %>% 
  mutate(VOLBFNET_ACRE_VB = STDAGE %>% map(~ (par_vb[[1]] * (1 - exp(- par_vb[[2]] * .x)) ^ 3))) %>% 
  unnest(VOLBFNET_ACRE_VB) %>% 
  select(STDAGE, VOLBFNET_ACRE, VOLBFNET_ACRE_VB)

par_pt_guess = c(1, 0.10, 35.00, 1.00) # Put in a guess. 

mod_pt_initial = 
  nloptr(
    par_pt_guess,
    fun_pt_outer_initial,
    lb = rep(1e-4, 4), 
    opts = 
      list(
        "algorithm" = "NLOPT_LN_COBYLA",
        "maxeval" = 1000
      )
  )

par_pt_initial = mod_pt_initial$solution

vis_initial = 
  ggplot() + 
  geom_point(data = dat_vb, aes(x = STDAGE, y = VOLBFNET_ACRE), alpha = 0.10) + 
  geom_line(
    data = dat_vb %>% distinct(STDAGE, VOLBFNET_ACRE_VB) %>% arrange(STDAGE), 
    aes(x = STDAGE, y = VOLBFNET_ACRE_VB),
    color = "red",
    linewidth = 1.25,
    alpha = 0.50
    ) +
  geom_line(
    data = 
      dat_vb %>% 
      mutate(VOLBFNET_ACRE_PT = STDAGE %>% map(~ fun_pt_inner(.x, par_pt_initial[[1]], par_pt_initial))) %>% 
      unnest(VOLBFNET_ACRE_PT),
    aes(x = STDAGE, y = VOLBFNET_ACRE_PT),
    color = "blue",
    linewidth = 1.25,
    alpha = 0.50
  )

#   Optimization

mod_pt_nloptr_growth = 
  nloptr(
    par_pt_initial,
    fun_pt_outer_growth,
    lb = rep(1e-4, 4), 
    opts = 
      list(
        "algorithm" = "NLOPT_LN_COBYLA", 
        "xtol_rel" = 1e-4,
        "maxeval" = 1000)
  )

par_pt_growth = mod_pt_nloptr_growth$solution

mod_pt_nloptr_yield = 
  nloptr(
    par_pt_initial,
    fun_pt_outer_yield,
    lb = rep(1e-4, 4), 
    opts = 
      list(
        "algorithm" = "NLOPT_LN_COBYLA", 
        "xtol_rel" = 1e-4,
        "maxeval" = 1000)
  )

par_pt_yield = mod_pt_nloptr_yield$solution

mod_pt_nloptr_combined = 
  nloptr(
    par_pt_initial,
    fun_pt_outer_combined,
    lb = rep(1e-4, 4), 
    opts = 
      list(
        "algorithm" = "NLOPT_LN_COBYLA", 
        "xtol_rel" = 1e-4,
        "maxeval" = 1000)
  )

par_pt_combined = mod_pt_nloptr_combined$solution

#  Visualization

vis_pt_growth = 
  dat_use %>% 
  mutate(
    VOLBFNET_ACRE_HAT_INITIAL = map(STDAGE, ~ fun_pt_inner(.x, par_pt_initial[[1]], par_pt_initial)),
    VOLBFNET_ACRE_HAT_YIELD = map(STDAGE, ~ fun_pt_inner(.x, par_pt_yield[[1]], par_pt_yield)),
    VOLBFNET_ACRE_HAT_GROWTH = map(STDAGE, ~ fun_pt_inner(.x, par_pt_growth[[1]], par_pt_growth)),
    VOLBFNET_ACRE_HAT_COMBINED = map(STDAGE, ~ fun_pt_inner(.x, par_pt_combined[[1]], par_pt_combined))
  ) %>% 
  unnest(starts_with("VOLBFNET_ACRE_HAT")) %>% 
  select(STDAGE, starts_with("VOLBFNET_ACRE")) %>% 
  pivot_longer(-STDAGE) %>% 
  ggplot() + 
  geom_point(data = . %>% filter(name == "VOLBFNET_ACRE"),
             aes(x = STDAGE, y = value),
             alpha = 0.10) +
  geom_point(data = . %>% filter(name != "VOLBFNET_ACRE"),
             aes(x = STDAGE,
                 y = value,
                 color = name),
             alpha = 0.50) +
  labs(x = "STDAGE", y = "VOLBFNET_ACRE") +
  scale_color_manual(values = c("green", "red", "blue", "purple")) +
  theme_pubr() +
  theme(legend.position = "bottom",
        legend.direction = "vertical",
        legend.title = element_blank())

vis_pt_yield = 
  dat_use %>% 
  drop_na(EST_BEGIN_ACRE) %>% 
  mutate(
    EST_CHANGE_ACRE_HAT_INITIAL = map2(REMPER_ROUND, EST_BEGIN_ACRE, ~ fun_pt_inner(.x, .y, par_pt_initial)),
    EST_CHANGE_ACRE_HAT_YIELD = map2(REMPER_ROUND, EST_BEGIN_ACRE, ~ fun_pt_inner(.x, .y, par_pt_yield)),
    EST_CHANGE_ACRE_HAT_GROWTH = map2(REMPER_ROUND, EST_BEGIN_ACRE, ~ fun_pt_inner(.x, .y, par_pt_growth)),
    EST_CHANGE_ACRE_HAT_COMBINED = map2(REMPER_ROUND, EST_BEGIN_ACRE, ~ fun_pt_inner(.x, .y, par_pt_combined))
  ) %>% 
  select(EST_BEGIN_ACRE, starts_with("EST_CHANGE_ACRE")) %>% 
  unnest(starts_with("EST_CHANGE_ACRE_HAT")) %>% 
  pivot_longer(-EST_BEGIN_ACRE) %>% 
  ggplot() + 
  geom_point(data = . %>% filter(name == "EST_CHANGE_ACRE"),
             aes(x = EST_BEGIN_ACRE, y = value),
             alpha = 0.10) +
  geom_point(data = . %>% filter(name != "EST_CHANGE_ACRE"),
             aes(x = EST_BEGIN_ACRE,
                 y = value,
                 color = name),
             alpha = 0.50) +
  labs(x = "EST_BEGIN_ACRE", y = "EST_CHANGE_ACRE") +
  scale_color_manual(values = c("green", "red", "blue", "purple")) +
  theme_pubr() +
  theme(legend.position = "bottom",
        legend.direction = "vertical",
        legend.title = element_blank())

vis_pt_growth + vis_pt_yield

ggsave("04_out/Other/vis_combined.png",
       dpi = 300,
       width = 8,
       height = 5)

# Note that code for iteration over regions, etc. is in earlier scripts. 
