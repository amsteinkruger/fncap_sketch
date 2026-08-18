# Wrangle FIA and estimate growth models.

# Grab disturbance and treatment variables, then reduce meaningfully. 

#  Data

#   Bounds

# dat_bounds = "03_intermediate/dat_bounds.gdb" %>% vect

#   Pyromes

# dat_pyrome = 
#   "02_data/1_2_2_USFS_Pyromes/Data/Pyromes_CONUS_20200206.shp" %>% 
#   vect %>% 
#   rename(WHICH = NAME) %>% # Band-Aid for a reserved attribute name.
#   filter(WHICH %in% c("Marine Northwest Coast Forest", "Klamath Mountains", "Middle Cascades")) %>% 
#   select(Pyrome = WHICH) %>% 
#   project("EPSG:2992") %>% 
#   crop(dat_bounds)

#  ODF Private Forest Districts

# dat_districts = 
#   "02_data/1_6_7_ODF_Districts/District_Boundaries.geojson" %>%
#   vect %>%
#   select(District = pf_dist) %>%
#   project("EPSG:2992") %>%
#   makeValid(buffer = TRUE) %>%
#   crop(dat_bounds)

#  Counties

# dat_counties = 
#   "02_data/1_6_6_TIGER/TIGER.gdb" %>% 
#   vect(layer = "County") %>% 
#   select(County = NAMELSAD) %>% 
#   project("EPSG:2992") %>%
#   crop(dat_bounds)

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
  select(INVYR, PLT_CN, TRE_CN, EST_BEGIN_ACRE, EST_END_ACRE, ANN_NET_GROWTH_ACRE)

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
    ANN_NET_GROWTH_ACRE = sum(ANN_NET_GROWTH_ACRE, na.rm = TRUE)
    ) %>% 
  ungroup %>% 
  mutate(across(ends_with("ACRE"), ~ ifelse(.x == 0, NA, .x)),
         across(ends_with("ACRE"), ~ .x / 1000)) %>% # BF to MBF. 
  # Cut western hemlock for now. 
  filter(SPCD == 202) %>% 
  # Handle condition data.
  left_join(dat_condition) %>% 
  filter(FORTYPCD == 201) %>% # Cuts to Douglas fir stands only. 
  filter(OWNGRPCD == 40) %>% 
  filter(STDAGE %!in% c(NA, 0, 998, 999)) %>% 
  # Handle plot data.
  left_join(dat_plot) %>% 
  # Explicate spatial data and reduce to region of interest.
  # vect(
  #   geom = c("LON", "LAT"),
  #   crs = "EPSG:4326"
  #   ) %>% 
  # project("EPSG:2992") %>% 
  # crop(dat_bounds) %>% 
  # Match to pyromes, districts, and counties.
  # intersect(dat_pyrome) %>% 
  # intersect(dat_districts) %>% 
  # intersect(dat_counties) %>% 
  # Back to implicit spatial data. 
  as_tibble %>% 
  # Cut stands older than 75 years for now.
  filter(STDAGE %in% 1:75) %>%
  # Handle outliers.
  filter(ntile(VOLBFNET_ACRE, 100) %in% 2:99) %>% 
  filter(ntile(VOLBFNET_ACRE / STDAGE, 100) %in% 2:99) %>% 
  filter(ntile(EST_BEGIN_ACRE, 100) %in% 2:99 | is.na(EST_BEGIN_ACRE)) %>% 
  filter(ntile(ANN_NET_GROWTH_ACRE, 100) %in% 2:99 | is.na(ANN_NET_GROWTH_ACRE)) %>%
  filter(ntile(ANN_NET_GROWTH_ACRE / VOLBFNET_ACRE, 100) %in% 2:99 | is.na(ANN_NET_GROWTH_ACRE))

# growth and yield are backwards here

# 2262 yield observations, 588 growth observations for Douglas fir without age restriction.
# 2192 yield observations, 573 growth observations for Douglas fir with STDAGE < 100. 
# 1555 yield observations, 430 growth observations for Douglas fir with STDAGE < 100 and FORTYPCD == 201. 
# 1472 , 397 with STDAGE <= 75, FORTYPCD == 201. 

# 47 of 1472 observations with MBF > 47. 

# Estimation

#  OLS Demo

library(nloptr) # Stick this in 0_0 if it keeps working. 

mod_ols_initial =
  dat_use %>% 
  # mutate(EST_ANNUAL_ACRE = EST_BEGIN_ACRE + ANN_NET_GROWTH_ACRE) %>% 
  lm(ANN_NET_GROWTH_ACRE ~ 0 + EST_BEGIN_ACRE, data = .) # EST_ANNUAL_ACRE

par_ols_initial = mod_ols_initial$coefficients[[1]]

fun_ols_iterate =
  function(times, par){
    
    Reduce(
      f = function(V_0, dv) V_0 + dv * V_0,
      x = rep(par, times),
      init = 1 
    )
    
  }

fun_ols_growth = 
  function(par){
    
    residuals_yield = 
      dat_use %>% # Note global call. 
      pull(EST_BEGIN_ACRE) %>% 
      multiply_by(par[[1]]) %>% 
      subtract(dat_use$ANN_NET_GROWTH_ACRE) %>% 
      raise_to_power(2) %>% 
      divide_by(length(.)) %>% # Weighting by observations. 
      sum(na.rm = TRUE)
    
    residuals_growth = 
      dat_use %>% 
      pull(STDAGE) %>% 
      map(~ fun_ols_iterate(.x, par)) %>% 
      unlist %>% 
      subtract(dat_use$VOLBFNET_ACRE) %>% 
      raise_to_power(2) %>% 
      divide_by(length(.)) %>% # Weighting by observations. 
      sum(na.rm = TRUE)
      
    residuals_yield + residuals_growth
    
  }

dat_ols_initial = fun_ols_growth(par_ols_initial)

mod_ols_optimizing = 
  nloptr(
    mod_ols_initial$coefficients[[1]],
    fun_ols_growth,
    opts = list("algorithm" = "NLOPT_LN_COBYLA")
  )

par_ols_optimized = mod_ols_optimizing$solution

# OLS Visualization

vis_ols_yield = 
  dat_use %>% 
  drop_na(ANN_NET_GROWTH_ACRE) %>% 
  mutate(
    ANN_NET_GROWTH_ACRE_HAT_NAIVE = EST_BEGIN_ACRE * par_ols_initial,
    ANN_NET_GROWTH_ACRE_HAT_OPTIMIZED = EST_BEGIN_ACRE * par_ols_optimized
  ) %>% 
  select(EST_BEGIN_ACRE, starts_with("ANN_NET_GROWTH_ACRE")) %>% 
  pivot_longer(-EST_BEGIN_ACRE) %>% 
  ggplot() + 
  geom_point(aes(x = EST_BEGIN_ACRE,
                 y = value,
                 color = name),
             alpha = 0.33)

vis_ols_growth = 
  dat_use %>% 
  mutate(
    VOLBFNET_ACRE_HAT_NAIVE = STDAGE %>% map(~ fun_ols_iterate(.x, par_ols_initial)), 
    VOLBFNET_ACRE_HAT_OPTIMIZED = STDAGE %>% map(~ fun_ols_iterate(.x, par_ols_optimized))
  ) %>% 
  unnest(c(VOLBFNET_ACRE_HAT_NAIVE, VOLBFNET_ACRE_HAT_OPTIMIZED)) %>% 
  select(STDAGE, starts_with("VOLBFNET_ACRE")) %>% 
  pivot_longer(-STDAGE) %>% 
  ggplot() + 
  geom_point(aes(x = STDAGE,
                 y = value,
                 color = name),
             alpha = 0.33) +
  scale_y_continuous(limits = c(0, 100))

vis_ols_yield + vis_ols_growth

# P-T Implementation

mod_pt_initial =
  dat_use %>% 
  drop_na(EST_BEGIN_ACRE) %>% 
  nls(# ANN_NET_GROWTH_ACRE ~ a * ANN_NET_GROWTH_ACRE * (1 - (EST_BEGIN_ACRE / b) ^ c),
      ANN_NET_GROWTH_ACRE ~ a * (1 + (EST_BEGIN_ACRE / b) ^ c),
      data = .,
      start = list(a = 0.1, b = 0.1, c = 0.500),
      algorithm = "port",
      lower = c(a = 1e-4, b = 1e-4, c = 1e-4),
      nls.control(maxiter = 100))
  
par_pt_initial = mod_pt_initial %>% coef

ggplot(data = dat_use) +
  geom_point(aes(x = EST_BEGIN_ACRE, y = ANN_NET_GROWTH_ACRE)) +
  geom_point(aes(x = EST_BEGIN_ACRE, y = (par_pt_initial[1] * (1 + (EST_BEGIN_ACRE / par_pt_initial[2]) ^ par_pt_initial[3]))), color = "red")

fun_pt_iterate = 
  function(times, par) {
    
    Reduce(
      function(V_0, i) V_0 + par[1] * (1 + (V_0 / par[2]) ^ par[3]),
      seq_len(times),
      init = 1
    )
  }

fun_pt_growth = 
  function(par){
    
    residuals_yield = 
      dat_use %>% # Note global call. 
      pull(EST_BEGIN_ACRE) %>% 
      {par[1] * (1 + (. / par[2]) ^ par[3])} %>% 
      subtract(dat_use$ANN_NET_GROWTH_ACRE) %>% 
      raise_to_power(2) %>% 
      divide_by(length(.)) %>% # Weighting by observations. 
      sum(na.rm = TRUE)
    
    residuals_growth = 
      dat_use %>% 
      pull(STDAGE) %>% 
      map(~ fun_pt_iterate(.x, par)) %>% 
      unlist %>% 
      subtract(dat_use$VOLBFNET_ACRE) %>% 
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
    ANN_NET_GROWTH_ACRE_HAT_NAIVE = par_pt_initial[1] * (1 + (EST_BEGIN_ACRE / par_pt_initial[2]) ^ par_pt_initial[3]),
    ANN_NET_GROWTH_ACRE_HAT_OPTIMIZED = par_pt_optimized[1] * (1 + (EST_BEGIN_ACRE / par_pt_optimized[2]) ^ par_pt_optimized[3])
  ) %>% 
  select(EST_BEGIN_ACRE, starts_with("ANN_NET_GROWTH_ACRE")) %>% 
  pivot_longer(-EST_BEGIN_ACRE) %>% 
  ggplot() + 
  geom_point(aes(x = EST_BEGIN_ACRE,
                 y = value,
                 color = name),
             alpha = 0.33)

vis_pt_growth = 
  dat_use %>% 
  mutate(
    VOLBFNET_ACRE_HAT_NAIVE = STDAGE %>% map(~ fun_pt_iterate(.x, par_pt_initial)), 
    VOLBFNET_ACRE_HAT_OPTIMIZED = STDAGE %>% map(~ fun_pt_iterate(.x, par_pt_optimized))
  ) %>% 
  unnest(c(VOLBFNET_ACRE_HAT_NAIVE, VOLBFNET_ACRE_HAT_OPTIMIZED)) %>% 
  select(STDAGE, starts_with("VOLBFNET_ACRE")) %>% 
  pivot_longer(-STDAGE) %>% 
  ggplot() + 
  geom_point(aes(x = STDAGE,
                 y = value,
                 color = name),
             alpha = 0.33) +
  scale_y_continuous(limits = c(0, 100))

vis_pt_yield + vis_pt_growth

# Problems:
#  (1) The combined model is dragging the yield curve implausibly far from the best separate fit.  
#  (2) The combined model isn't actually converging in 1000 evaluations. 
#  (3) ???

# Things to try:
#  (1) Split on site class.
#  (2) Try alternative functional forms. 
#  (3) Try alternative nonlinear optimization programs.


# To generalize over regions, site classes, etc., refer to earlier modeling script. 
