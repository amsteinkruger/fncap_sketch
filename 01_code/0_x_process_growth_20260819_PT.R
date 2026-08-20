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
         across(ends_with("ACRE"), ~ .x / 1000), # BF to MBF. 
         EST_CHANGE_ACRE = EST_END_ACRE - EST_BEGIN_ACRE) %>% 
  relocate(EST_CHANGE_ACRE, .after = EST_END_ACRE) %>% 
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
  filter(ntile(ANN_NET_GROWTH_ACRE / VOLBFNET_ACRE, 100) %in% 2:99 | is.na(ANN_NET_GROWTH_ACRE)) %>% 
  # Mangle data to check something.
  drop_na(EST_BEGIN_ACRE) # %>% 
  # filter(ntile(EST_CHANGE_ACRE, 100) %!in% 0:50)

# Estimation

library(nloptr) # Remember to kick this into packages.R if it works. 

par_pt_initial = c(1, 0.04, 80.00, 3.00) # Put in a yield-informed guess. 

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
      mutate(EST_CHANGE_ACRE_RESIDUAL = (EST_CHANGE_ACRE_HAT - EST_CHANGE_ACRE) / EST_CHANGE_ACRE) %>% # Normalize to percent deviation. 
      pull(EST_CHANGE_ACRE_RESIDUAL) %>%
      raise_to_power(2) %>%
      divide_by(length(.)) %>% # Weight by observations.
      sum(na.rm = TRUE)
    
    return(residuals_yield)
    
  }

mod_pt_nloptr_yield = 
  nloptr(
    par_pt_yield,
    fun_pt_outer_yield,
    opts = 
      list(
        "algorithm" = "NLOPT_LN_COBYLA", 
        "xtol_rel" = 1e-3,
        "maxeval" = 1000)
  )

par_pt_yield = mod_pt_nloptr_yield$solution

fun_pt_outer_growth = 
  function(par){
    
    residuals_growth = 
      dat_use %>% # Note global call. 
      drop_na(STDAGE) %>% 
      mutate(VOLBFNET_ACRE_HAT = map(STDAGE, ~ fun_pt_inner(.x, par[[1]], par))) %>% 
      unnest(VOLBFNET_ACRE_HAT) %>% 
      mutate(VOLBFNET_ACRE_RESIDUAL = (VOLBFNET_ACRE_HAT - VOLBFNET_ACRE) / VOLBFNET_ACRE) %>% # Normalize to percent deviation.
      pull(VOLBFNET_ACRE_RESIDUAL) %>% 
      raise_to_power(2) %>% 
      divide_by(length(.)) %>% # Weighting by observations. 
      sum(na.rm = TRUE)
    
    return(residuals_growth)
    
  }

mod_pt_nloptr_growth = 
  nloptr(
    par_pt_initial,
    fun_pt_outer_growth,
    opts = 
      list(
        "algorithm" = "NLOPT_LN_COBYLA", 
        "xtol_rel" = 1e-3,
        "maxeval" = 1000)
  )

par_pt_growth = mod_pt_nloptr_growth$solution

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
      mutate(EST_CHANGE_ACRE_RESIDUAL = (EST_CHANGE_ACRE_HAT - EST_CHANGE_ACRE) / EST_CHANGE_ACRE) %>% # Normalize to percent deviation. 
      pull(EST_CHANGE_ACRE_RESIDUAL) %>%
      raise_to_power(2) %>%
      divide_by(length(.)) %>% # Weight by observations.
      sum(na.rm = TRUE)
    
    residuals_growth = 
      dat_use %>% # Note global call. 
      drop_na(STDAGE) %>% 
      mutate(VOLBFNET_ACRE_HAT = map(STDAGE, ~ fun_pt_inner(.x, par[[1]], par))) %>% 
      unnest(VOLBFNET_ACRE_HAT) %>% 
      mutate(VOLBFNET_ACRE_RESIDUAL = (VOLBFNET_ACRE_HAT - VOLBFNET_ACRE) / VOLBFNET_ACRE) %>% # Normalize to percent deviation.
      pull(VOLBFNET_ACRE_RESIDUAL) %>% 
      raise_to_power(2) %>% 
      divide_by(length(.)) %>% # Weighting by observations. 
      sum(na.rm = TRUE)
      
    residuals_combined = residuals_yield + residuals_growth
    
    return(residuals_combined)
    
  }

mod_pt_nloptr_combined = 
  nloptr(
    par_pt_initial,
    fun_pt_outer_combined,
    opts = 
      list(
        "algorithm" = "NLOPT_LN_COBYLA", 
        "xtol_rel" = 1e-3,
        "maxeval" = 1000)
  )

par_pt_combined = mod_pt_nloptr_combined$solution

# PT Visualization

vis_pt_yield = 
  dat_use %>% 
  drop_na(EST_BEGIN_ACRE) %>% 
  mutate(
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
  scale_color_manual(values = c("red", "blue", "green")) +
  theme_pubr() +
  theme(legend.position = "bottom",
        legend.direction = "vertical",
        legend.title = element_blank())

vis_pt_growth = 
  dat_use %>% 
  mutate(
    VOLBFNET_ACRE_HAT_YIELD = map2(STDAGE, par_pt_yield[[1]], ~ fun_pt_inner(.x, .y, par_pt_yield)),
    VOLBFNET_ACRE_HAT_GROWTH = map2(STDAGE, par_pt_growth[[1]], ~ fun_pt_inner(.x, .y, par_pt_growth)),
    VOLBFNET_ACRE_HAT_COMBINED = map2(STDAGE, par_pt_combined[[1]], ~ fun_pt_inner(.x, .y, par_pt_combined))
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
  labs(x = "STDAGE", y = "EST_ANNUAL_ACRE") +
  scale_color_manual(values = c("red", "blue", "green")) +
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
