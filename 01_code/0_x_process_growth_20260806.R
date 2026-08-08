# Wrangle FIA and estimate growth models.

# Grab disturbance and treatment variables, then reduce meaningfully. 

#  Data

#   Bounds

dat_bounds = "03_intermediate/dat_bounds.gdb" %>% vect

#   Pyromes

dat_pyrome = 
  "02_data/1_2_2_USFS_Pyromes/Data/Pyromes_CONUS_20200206.shp" %>% 
  vect %>% 
  rename(WHICH = NAME) %>% # Band-Aid for a reserved attribute name.
  filter(WHICH %in% c("Marine Northwest Coast Forest", "Klamath Mountains", "Middle Cascades")) %>% 
  select(Pyrome = WHICH) %>% 
  project("EPSG:2992") %>% 
  crop(dat_bounds)

#  ODF Private Forest Districts

dat_districts = 
  "02_data/1_6_7_ODF_Districts/District_Boundaries.geojson" %>%
  vect %>%
  select(District = pf_dist) %>%
  project("EPSG:2992") %>%
  makeValid(buffer = TRUE) %>%
  crop(dat_bounds)

#  Counties

dat_counties = 
  "02_data/1_6_6_TIGER/TIGER.gdb" %>% 
  vect(layer = "County") %>% 
  select(County = NAMELSAD) %>% 
  project("EPSG:2992") %>%
  crop(dat_bounds)

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
  # Handle condition data.
  left_join(dat_condition) %>% 
  filter(FORTYPCD == 201) %>% 
  filter(OWNGRPCD == 40) %>% 
  filter(STDAGE %!in% c(NA, 0, 998, 999)) %>% 
  # Handle plot data.
  left_join(dat_plot) %>% 
  # Explicate spatial data and reduce to region of interest.
  vect(
    geom = c("LON", "LAT"),
    crs = "EPSG:4326"
    ) %>% 
  project("EPSG:2992") %>% 
  crop(dat_bounds) %>% 
  # Match to pyromes, districts, and counties.
  intersect(dat_pyrome) %>% 
  intersect(dat_districts) %>% 
  intersect(dat_counties) %>% 
  # Back to implicit spatial data. 
  as_tibble %>% 
  # Cut western hemlock for now. 
  filter(SPCD == 202) %>%
  # Cut stands older than 75 years for now.
  filter(STDAGE %in% 1:75) %>%
  # Handle outliers.
  filter(ntile(VOLBFNET_ACRE, 100) %in% 2:99) %>% 
  filter(ntile(VOLBFNET_ACRE / STDAGE, 100) %in% 2:99) %>% 
  filter(ntile(EST_BEGIN_ACRE, 100) %in% 2:99 | is.na(EST_BEGIN_ACRE)) %>% 
  filter(ntile(ANN_NET_GROWTH_ACRE, 100) %in% 2:99 | is.na(ANN_NET_GROWTH_ACRE)) %>% 
  filter(ntile(ANN_NET_GROWTH_ACRE / VOLBFNET_ACRE, 100) %in% 2:99 | is.na(ANN_NET_GROWTH_ACRE))

# 2262 yield observations, 588 growth observations for Douglas fir without age restriction.
# 2192 yield observations, 573 growth observations for Douglas fir with STDAGE < 100. 
# 1555 yield observations, 430 growth observations for Douglas fir with STDAGE < 100 and FORTYPCD == 201. 
# 1322 for STDAGE in 20, 79. 

# Visualization

# vis_1 = 
#   dat_use %>%
#   as_tibble %>% 
#   ggplot(aes(x = STDAGE,
#              y = VOLBFNET_ACRE)) + 
#   geom_point(alpha = 0.25) +
#   theme_minimal() 
# 
# vis_2 = 
#   dat_use %>%
#   as_tibble %>% 
#   ggplot(aes(x = EST_BEGIN_ACRE,
#              y = ANN_NET_GROWTH_ACRE)) + 
#   geom_point(alpha = 0.25) +
#   theme_minimal() 
# 
# vis_data = vis_1 + vis_2
# 
# ggsave("04_out/Presentation_20260805/vis_data.png",
#        vis_data,
#        dpi = 300,
#        width = 8.5,
#        height = 4)

# Estimation

#  problem: lots of NaNs from log in CR

dat_estimates = 
  dat_use %>% 
  mutate(Aggregate = "All") %>% 
  pivot_longer(
    c(Aggregate, Pyrome, District, County),
    names_to = "Definition",
    values_to = "Region") %>% 
  group_by(Definition, Region) %>% 
  nest %>% 
  mutate(
    Estimate_Linear = 
      data %>% 
      map(
        ~ lm(
          VOLBFNET_ACRE ~ 0 + STDAGE, 
          data = .x
        )
      ),
    Estimate_VB =
      data %>%
      map(
        ~ tryCatch(
          {
            nls(
              VOLBFNET_ACRE ~ a * (1 - exp(- b * STDAGE)) ^ 3,
              data = .,
              start = list(a = 150, b = 0.01)
            )
          },
          error = function(message){NA}
        )
      ),
    Estimate_CR =
      data %>%
      map(
        ~ tryCatch(
          {
            nls(
              log(VOLBFNET_ACRE) ~ a + p * log(1 - exp(-k * STDAGE)),
              data = .,
              start = list(a = 5, p = 2, k = 0.01)
            )
          },
          error = function(message){NA}
        )
      )
  ) %>% 
  select(-data) %>% 
  pivot_longer(starts_with("Estimate"),
               names_prefix = "Estimate_",
               names_to = "Model",
               values_to = "Estimate")

# Mihiar and Lewis use V-B expressed as: Y(a) = alpha (1 - e ^ (- beta a))
#  Criteria for keeping estimates:
#   (1) N >= 30
#   (2) Convergent
#   (3) beta <= 0.25
#  Failing any criteria led to using state estimates rather than county estimates.

# Keeping Chapman-Richards as an alternative to the sinusoidal Von Bertalanffy in case the enforced cubic is a downside. 

#   Linear

mod_yield_fir_linear = 
  dat_use %>% 
  lm(VOLBFNET_ACRE ~ STDAGE, data = .)

par_yield_fir_linear_a = mod_yield_fir_linear$coefficients[[1]]
par_yield_fir_linear_b = mod_yield_fir_linear$coefficients[[2]]

#   Von Bertalanffy

mod_vb_all = 
  dat_use %>% 
  nls(
    VOLBFNET_ACRE ~ a * (1 - exp(- b * STDAGE)) ^ 3,
    data = .,
    start = list(a = 150, b = 0.01)
  )

par_vb_all_a = mod_vb_all %>% coef %>% magrittr::extract(1)
par_vb_all_b = mod_vb_all %>% coef %>% magrittr::extract(2)

plot(1:75, par_vb_all_a * (1 - exp(- par_vb_all_b * 1:75)) ^ 3)

#   Chapman-Richards

mod_cr_all = 
  dat_use %>% 
  mutate(VOLBFNET_ACRE_LOG = VOLBFNET_ACRE %>% log) %>% 
  nls(
    VOLBFNET_ACRE_LOG ~ a + p * log(1 - exp(-k * STDAGE)),
    data = .,
    start = list(a = 5, p = 2, k = 0.01)
  )

par_cr_all_a = mod_cr_all %>% coef %>% magrittr::extract(1)
par_cr_all_p = mod_cr_all %>% coef %>% magrittr::extract(2)
par_cr_all_k = mod_cr_all %>% coef %>% magrittr::extract(3)

plot(1:75, exp(par_cr_all_a + par_cr_all_p * log(1 - exp(-par_cr_all_k * 1:75))))

# Reference

#  Yield ~ Age

#   Linear

mod_yield_fir_linear = 
  dat_use %>% 
  lm(VOLBFNET_ACRE ~ STDAGE, data = .)

par_yield_fir_linear_a = mod_yield_fir_linear$coefficients[[1]]
par_yield_fir_linear_b = mod_yield_fir_linear$coefficients[[2]]

mod_yield_fir_linear_fast = 
  dat_use %>% 
  filter(SITECLCD %in% vec_fast) %>% 
  lm(VOLBFNET_ACRE ~ STDAGE, data = .)

par_yield_fir_linear_fast_a = mod_yield_fir_linear_fast$coefficients[[1]]
par_yield_fir_linear_fast_b = mod_yield_fir_linear_fast$coefficients[[2]]

mod_yield_fir_linear_slow = 
  dat_use %>% 
  filter(SITECLCD %in% vec_slow) %>% 
  lm(VOLBFNET_ACRE ~ STDAGE, data = .)

par_yield_fir_linear_slow_a = mod_yield_fir_linear_slow$coefficients[[1]]
par_yield_fir_linear_slow_b = mod_yield_fir_linear_slow$coefficients[[2]]

#   Logistic

mod_yield_fir_logistic = 
  dat_use %>% 
  nls(
    VOLBFNET_ACRE ~ k / (1 + a * exp(-b * STDAGE)),
    data = .,
    start = list(k = 60000, a = 50, b = 0.10)
  )

par_yield_fir_logistic_k = mod_yield_fir_logistic %>% coef %>% magrittr::extract(1)
par_yield_fir_logistic_a = mod_yield_fir_logistic %>% coef %>% magrittr::extract(2)
par_yield_fir_logistic_b = mod_yield_fir_logistic %>% coef %>% magrittr::extract(3)

mod_yield_fir_logistic_fast = 
  dat_use %>% 
  filter(SITECLCD %in% vec_fast) %>% 
  nls(
    VOLBFNET_ACRE ~ k / (1 + a * exp(-b * STDAGE)),
    data = .,
    start = list(k = 60000, a = 50, b = 0.10)
  )

par_yield_fir_logistic_fast_k = mod_yield_fir_logistic_fast %>% coef %>% magrittr::extract(1)
par_yield_fir_logistic_fast_a = mod_yield_fir_logistic_fast %>% coef %>% magrittr::extract(2)
par_yield_fir_logistic_fast_b = mod_yield_fir_logistic_fast %>% coef %>% magrittr::extract(3)

mod_yield_fir_logistic_slow = 
  dat_use %>% 
  filter(SITECLCD %in% vec_slow) %>% 
  nls(
    VOLBFNET_ACRE ~ k / (1 + a * exp(-b * STDAGE)),
    data = .,
    start = list(k = 60000, a = 50, b = 0.10)
  )

par_yield_fir_logistic_slow_k = mod_yield_fir_logistic_slow %>% coef %>% magrittr::extract(1)
par_yield_fir_logistic_slow_a = mod_yield_fir_logistic_slow %>% coef %>% magrittr::extract(2)
par_yield_fir_logistic_slow_b = mod_yield_fir_logistic_slow %>% coef %>% magrittr::extract(3)

#   Chang (1984), Hashida and Fenichel (2021)

mod_yield_fir_chang = 
  dat_use %>% 
  mutate(STDAGE_INVERSE = STDAGE ^ -1,
         STDAGE_INVERSE_SQUARE = STDAGE ^ -2,
         VOLBFNET_ACRE_LOG = VOLBFNET_ACRE %>% log) %>% 
  lm(VOLBFNET_ACRE_LOG ~ STDAGE_INVERSE + STDAGE_INVERSE_SQUARE, data = .)

par_yield_fir_chang_0 = mod_yield_fir_chang$coefficients[[1]]
par_yield_fir_chang_1 = mod_yield_fir_chang$coefficients[[2]]
par_yield_fir_chang_2 = mod_yield_fir_chang$coefficients[[3]]

mod_yield_fir_chang_fast = 
  dat_use %>% 
  filter(SITECLCD %in% vec_fast) %>% 
  mutate(STDAGE_INVERSE = STDAGE ^ -1,
         STDAGE_INVERSE_SQUARE = STDAGE ^ -2,
         VOLBFNET_ACRE_LOG = VOLBFNET_ACRE %>% log) %>% 
  lm(VOLBFNET_ACRE_LOG ~ STDAGE_INVERSE + STDAGE_INVERSE_SQUARE, data = .)

par_yield_fir_chang_fast_0 = mod_yield_fir_chang_fast$coefficients[[1]]
par_yield_fir_chang_fast_1 = mod_yield_fir_chang_fast$coefficients[[2]]
par_yield_fir_chang_fast_2 = mod_yield_fir_chang_fast$coefficients[[3]]

mod_yield_fir_chang_slow = 
  dat_use %>% 
  filter(SITECLCD %in% vec_slow) %>% 
  mutate(STDAGE_INVERSE = STDAGE ^ -1,
         STDAGE_INVERSE_SQUARE = STDAGE ^ -2,
         VOLBFNET_ACRE_LOG = VOLBFNET_ACRE %>% log) %>% 
  lm(VOLBFNET_ACRE_LOG ~ STDAGE_INVERSE + STDAGE_INVERSE_SQUARE, data = .)

par_yield_fir_chang_slow_0 = mod_yield_fir_chang_slow$coefficients[[1]]
par_yield_fir_chang_slow_1 = mod_yield_fir_chang_slow$coefficients[[2]]
par_yield_fir_chang_slow_2 = mod_yield_fir_chang_slow$coefficients[[3]]

#   Chapman-Richards
#    Note that without reference to MCC or MAI via FIA, this does not meaningfully follow Chisholm and Gray. 

mod_yield_fir_chapmanrichards = 
  dat_use %>% 
  mutate(VOLBFNET_ACRE_LOG = VOLBFNET_ACRE %>% log) %>% 
  nls(
    VOLBFNET_ACRE_LOG ~ a + p * log(1 - exp(-k * STDAGE)),
    data = .,
    start = list(a = 60000, p = 100, k = 0.01)
  )

par_yield_fir_chapmanrichards_a = mod_yield_fir_chapmanrichards %>% coef %>% magrittr::extract(1)
par_yield_fir_chapmanrichards_p = mod_yield_fir_chapmanrichards %>% coef %>% magrittr::extract(2)
par_yield_fir_chapmanrichards_k = mod_yield_fir_chapmanrichards %>% coef %>% magrittr::extract(3)

mod_yield_fir_chapmanrichards_fast = 
  dat_use %>% 
  filter(SITECLCD %in% vec_fast) %>% 
  mutate(VOLBFNET_ACRE_LOG = VOLBFNET_ACRE %>% log) %>% 
  nls(
    VOLBFNET_ACRE_LOG ~ a + p * log(1 - exp(-k * STDAGE)),
    data = .,
    start = list(a = 60000, p = 100, k = 0.01)
  )

par_yield_fir_chapmanrichards_fast_a = mod_yield_fir_chapmanrichards_fast %>% coef %>% magrittr::extract(1)
par_yield_fir_chapmanrichards_fast_p = mod_yield_fir_chapmanrichards_fast %>% coef %>% magrittr::extract(2)
par_yield_fir_chapmanrichards_fast_k = mod_yield_fir_chapmanrichards_fast %>% coef %>% magrittr::extract(3)

mod_yield_fir_chapmanrichards_slow = 
  dat_use %>% 
  filter(SITECLCD %in% vec_slow) %>% 
  mutate(VOLBFNET_ACRE_LOG = VOLBFNET_ACRE %>% log) %>% 
  nls(
    VOLBFNET_ACRE_LOG ~ a + p * log(1 - exp(-k * STDAGE)),
    data = .,
    start = list(a = 60000, p = 100, k = 0.01)
  )

par_yield_fir_chapmanrichards_slow_a = mod_yield_fir_chapmanrichards_slow %>% coef %>% magrittr::extract(1)
par_yield_fir_chapmanrichards_slow_p = mod_yield_fir_chapmanrichards_slow %>% coef %>% magrittr::extract(2)
par_yield_fir_chapmanrichards_slow_k = mod_yield_fir_chapmanrichards_slow %>% coef %>% magrittr::extract(3)

#  Yield_1 ~ Yield_0

#   Linear

mod_growth_fir_linear = 
  dat_use %>% 
  lm(ANN_NET_GROWTH_ACRE ~ EST_BEGIN_ACRE, data = .)

par_growth_fir_linear_a = mod_growth_fir_linear$coefficients[[1]]
par_growth_fir_linear_b = mod_growth_fir_linear$coefficients[[2]]

mod_growth_fir_linear_fast = 
  dat_use %>% 
  filter(SITECLCD %in% vec_fast) %>% 
  lm(ANN_NET_GROWTH_ACRE ~ EST_BEGIN_ACRE, data = .)

par_growth_fir_linear_fast_a = mod_growth_fir_linear_fast$coefficients[[1]]
par_growth_fir_linear_fast_b = mod_growth_fir_linear_fast$coefficients[[2]]

mod_growth_fir_linear_slow = 
  dat_use %>% 
  filter(SITECLCD %in% vec_slow) %>% 
  lm(ANN_NET_GROWTH_ACRE ~ EST_BEGIN_ACRE, data = .)

par_growth_fir_linear_slow_a = mod_growth_fir_linear_slow$coefficients[[1]]
par_growth_fir_linear_slow_b = mod_growth_fir_linear_slow$coefficients[[2]]

#   Logistic

mod_growth_fir_logistic =
  dat_use %>% 
  nls(
    ANN_NET_GROWTH_ACRE ~ r * EST_BEGIN_ACRE * (1 - EST_BEGIN_ACRE / k),
    data = .,
    start = list(r = 1.10, k = 60000)
  )

par_growth_fir_logistic_r = mod_growth_fir_logistic %>% coef %>% magrittr::extract(1)
par_growth_fir_logistic_k = mod_growth_fir_logistic %>% coef %>% magrittr::extract(2)

mod_growth_fir_logistic_fast =
  dat_use %>% 
  filter(SITECLCD %in% vec_fast) %>% 
  nls(
    ANN_NET_GROWTH_ACRE ~ r * EST_BEGIN_ACRE * (1 - EST_BEGIN_ACRE / k),
    data = .,
    start = list(r = 1.10, k = 60000)
  )

par_growth_fir_logistic_fast_r = mod_growth_fir_logistic_fast %>% coef %>% magrittr::extract(1)
par_growth_fir_logistic_fast_k = mod_growth_fir_logistic_fast %>% coef %>% magrittr::extract(2)

mod_growth_fir_logistic_slow =
  dat_use %>% 
  filter(SITECLCD %in% vec_slow) %>% 
  nls(
    ANN_NET_GROWTH_ACRE ~ r * EST_BEGIN_ACRE * (1 - EST_BEGIN_ACRE / k),
    data = .,
    start = list(r = 1.10, k = 60000)
  )

par_growth_fir_logistic_slow_r = mod_growth_fir_logistic_slow %>% coef %>% magrittr::extract(1)
par_growth_fir_logistic_slow_k = mod_growth_fir_logistic_slow %>% coef %>% magrittr::extract(2)

#   Ricker

mod_growth_fir_ricker = 
  dat_use %>% 
  mutate(MBF_QUOTIENT_LOG = log(ANN_NET_GROWTH_ACRE / EST_BEGIN_ACRE),
         MBF_INITIAL = EST_BEGIN_ACRE) %>% 
  lm(MBF_QUOTIENT_LOG ~ MBF_INITIAL, data = .)

par_growth_fir_ricker_r = mod_growth_fir_ricker$coefficients[[1]]
par_growth_fir_ricker_k = - mod_growth_fir_ricker$coefficients[[1]] / mod_growth_fir_ricker$coefficients[[2]]

mod_growth_fir_ricker_fast = 
  dat_use %>% 
  filter(SITECLCD %in% vec_fast) %>% 
  mutate(MBF_QUOTIENT_LOG = log(ANN_NET_GROWTH_ACRE / EST_BEGIN_ACRE),
         MBF_INITIAL = EST_BEGIN_ACRE) %>% 
  lm(MBF_QUOTIENT_LOG ~ MBF_INITIAL, data = .)

par_growth_fir_ricker_fast_r = mod_growth_fir_ricker_fast$coefficients[[1]]
par_growth_fir_ricker_fast_k = - mod_growth_fir_ricker_fast$coefficients[[1]] / mod_growth_fir_ricker_fast$coefficients[[2]]

mod_growth_fir_ricker_slow = 
  dat_use %>% 
  filter(SITECLCD %in% vec_slow) %>% 
  mutate(MBF_QUOTIENT_LOG = log(ANN_NET_GROWTH_ACRE / EST_BEGIN_ACRE),
         MBF_INITIAL = EST_BEGIN_ACRE) %>% 
  lm(MBF_QUOTIENT_LOG ~ MBF_INITIAL, data = .)

par_growth_fir_ricker_slow_r = mod_growth_fir_ricker_slow$coefficients[[1]]
par_growth_fir_ricker_slow_k = - mod_growth_fir_ricker_slow$coefficients[[1]] / mod_growth_fir_ricker_slow$coefficients[[2]]

#  Beverton-Holt

mod_growth_fir_bevertonholt =
  dat_use %>% 
  nls(
    ANN_NET_GROWTH_ACRE ~ EST_BEGIN_ACRE * (r / (1 + ((r - 1) / k) * EST_BEGIN_ACRE)),
    data = .,
    start = list(r = 1.10, k = 60000)
    )

par_growth_fir_bevertonholt_r = mod_growth_fir_bevertonholt %>% coef %>% magrittr::extract(1)
par_growth_fir_bevertonholt_k = mod_growth_fir_bevertonholt %>% coef %>% magrittr::extract(2)

mod_growth_fir_bevertonholt_fast =
  dat_use %>% 
  filter(SITECLCD %in% vec_fast) %>% 
  nls(
    ANN_NET_GROWTH_ACRE ~ EST_BEGIN_ACRE * (r / (1 + ((r - 1) / k) * EST_BEGIN_ACRE)),
    data = .,
    start = list(r = 1.10, k = 60000)
  )

par_growth_fir_bevertonholt_fast_r = mod_growth_fir_bevertonholt_fast %>% coef %>% magrittr::extract(1)
par_growth_fir_bevertonholt_fast_k = mod_growth_fir_bevertonholt_fast %>% coef %>% magrittr::extract(2)

mod_growth_fir_bevertonholt_slow =
  dat_use %>% 
  filter(SITECLCD %in% vec_slow) %>% 
  nls(
    ANN_NET_GROWTH_ACRE ~ EST_BEGIN_ACRE * (r / (1 + ((r - 1) / k) * EST_BEGIN_ACRE)),
    data = .,
    start = list(r = 1.10, k = 60000)
  )

par_growth_fir_bevertonholt_slow_r = mod_growth_fir_bevertonholt_slow %>% coef %>% magrittr::extract(1)
par_growth_fir_bevertonholt_slow_k = mod_growth_fir_bevertonholt_slow %>% coef %>% magrittr::extract(2)

#  Vary: site class, species, ecoregion, county

# Estimation w/ Stochastic Component


# Visualization

#  Get point estimates. 

#  All

dat_fir_model = 
  dat_use %>% 
  mutate(
    # Yield
    VOLBFNET_ACRE_FIR_LINEAR = par_yield_fir_linear_a + par_yield_fir_linear_b * STDAGE,
    VOLBFNET_ACRE_FIR_LOGISTIC = par_yield_fir_logistic_k / (1 + par_yield_fir_logistic_a * exp(-par_yield_fir_logistic_b * STDAGE)),
    VOLBFNET_ACRE_FIR_CHANG = exp(par_yield_fir_chang_0) * exp(par_yield_fir_chang_1 * STDAGE ^ -1 + par_yield_fir_chang_2 * STDAGE ^ -2),
    VOLBFNET_ACRE_FIR_CHAPMANRICHARDS = exp(par_yield_fir_chapmanrichards_a) * (1 - exp(- par_yield_fir_chapmanrichards_k * STDAGE)) ^ par_yield_fir_chapmanrichards_p,
    # Growth
    ANN_NET_GROWTH_ACRE_FIR_LINEAR = par_growth_fir_linear_a + par_growth_fir_linear_b * EST_BEGIN_ACRE,
    ANN_NET_GROWTH_ACRE_FIR_LOGISTIC = par_growth_fir_logistic_r * EST_BEGIN_ACRE * (1 - EST_BEGIN_ACRE / par_growth_fir_logistic_k),
    ANN_NET_GROWTH_ACRE_FIR_RICKER = EST_BEGIN_ACRE * exp(par_growth_fir_ricker_r * (1 - EST_BEGIN_ACRE / par_growth_fir_ricker_k)),
    ANN_NET_GROWTH_ACRE_FIR_BEVERTONHOLT = EST_BEGIN_ACRE * (par_growth_fir_bevertonholt_r / (1 + ((par_growth_fir_bevertonholt_r - 1) / par_growth_fir_bevertonholt_k) * EST_BEGIN_ACRE)) 
    )

dat_fir_model_yield = 
  dat_fir_model %>% 
  select(VALUE_X = STDAGE, starts_with("VOLBFNET_")) %>% 
  pivot_longer(cols = starts_with("VOLBFNET_"),
               names_prefix = "VOLBFNET_ACRE_",
               names_to = "MODEL",
               values_to = "VALUE_Y") %>% 
  mutate(
    MODEL = 
      MODEL %>% 
      str_remove_all("FIR_") %>% 
      str_replace_all("VOLBFNET_ACRE", "OBSERVED"),
    SUBSET = "ALL",
    VARIABLE = "YIELD"
    )

dat_fir_model_growth = 
  dat_fir_model %>% 
  select(VALUE_X = EST_BEGIN_ACRE, starts_with("ANN_NET_GROWTH_")) %>% 
  drop_na(VALUE_X) %>% 
  pivot_longer(cols = starts_with("ANN_NET_GROWTH_"),
               names_prefix = "ANN_NET_GROWTH_ACRE_",
               names_to = "MODEL",
               values_to = "VALUE_Y") %>% 
  mutate(
    MODEL = 
      MODEL %>% 
      str_remove_all("FIR_") %>% 
      str_replace_all("ANN_NET_GROWTH_ACRE", "OBSERVED"),
    SUBSET = "ALL",
    VARIABLE = "GROWTH"
  )

# Site Class 1-3

dat_fir_model_fast = 
  dat_use %>% 
  filter(SITECLCD %in% vec_fast) %>% 
  mutate(
    # Yield
    VOLBFNET_ACRE_FIR_LINEAR = par_yield_fir_linear_fast_a + par_yield_fir_linear_fast_b * STDAGE,
    VOLBFNET_ACRE_FIR_LOGISTIC = par_yield_fir_logistic_fast_k / (1 + par_yield_fir_logistic_fast_a * exp(-par_yield_fir_logistic_fast_b * STDAGE)),
    VOLBFNET_ACRE_FIR_CHANG = exp(par_yield_fir_chang_fast_0) * exp(par_yield_fir_chang_fast_1 * STDAGE ^ -1 + par_yield_fir_chang_fast_2 * STDAGE ^ -2),
    VOLBFNET_ACRE_FIR_CHAPMANRICHARDS = exp(par_yield_fir_chapmanrichards_fast_a) * (1 - exp(- par_yield_fir_chapmanrichards_fast_k * STDAGE)) ^ par_yield_fir_chapmanrichards_fast_p,
    # Growth
    ANN_NET_GROWTH_ACRE_FIR_LINEAR = par_growth_fir_linear_fast_a + par_growth_fir_linear_fast_b * EST_BEGIN_ACRE,
    ANN_NET_GROWTH_ACRE_FIR_LOGISTIC = par_growth_fir_logistic_fast_r * EST_BEGIN_ACRE * (1 - EST_BEGIN_ACRE / par_growth_fir_logistic_fast_k),
    ANN_NET_GROWTH_ACRE_FIR_RICKER = EST_BEGIN_ACRE * exp(par_growth_fir_ricker_fast_r * (1 - EST_BEGIN_ACRE / par_growth_fir_ricker_fast_k)),
    ANN_NET_GROWTH_ACRE_FIR_BEVERTONHOLT = EST_BEGIN_ACRE * (par_growth_fir_bevertonholt_fast_r / (1 + ((par_growth_fir_bevertonholt_fast_r - 1) / par_growth_fir_bevertonholt_fast_k) * EST_BEGIN_ACRE)) 
  )

dat_fir_model_fast_yield = 
  dat_fir_model_fast %>% 
  select(VALUE_X = STDAGE, starts_with("VOLBFNET_")) %>% 
  pivot_longer(cols = starts_with("VOLBFNET_"),
               names_prefix = "VOLBFNET_ACRE_",
               names_to = "MODEL",
               values_to = "VALUE_Y") %>% 
  mutate(
    MODEL = 
      MODEL %>% 
      str_remove_all("FIR_") %>% 
      str_replace_all("VOLBFNET_ACRE", "OBSERVED"),
    SUBSET = "1-3",
    VARIABLE = "YIELD"
  )

dat_fir_model_fast_growth = 
  dat_fir_model_fast %>% 
  select(VALUE_X = EST_BEGIN_ACRE, starts_with("ANN_NET_GROWTH_")) %>% 
  drop_na(VALUE_X) %>% 
  pivot_longer(cols = starts_with("ANN_NET_GROWTH_"),
               names_prefix = "ANN_NET_GROWTH_ACRE_",
               names_to = "MODEL",
               values_to = "VALUE_Y") %>% 
  mutate(
    MODEL = 
      MODEL %>% 
      str_remove_all("FIR_") %>% 
      str_replace_all("ANN_NET_GROWTH_ACRE", "OBSERVED"),
    SUBSET = "1-3",
    VARIABLE = "GROWTH"
  )

# Site Class 4-7

dat_fir_model_slow = 
  dat_use %>% 
  filter(SITECLCD %in% vec_slow) %>% 
  mutate(
    # Yield
    VOLBFNET_ACRE_FIR_LINEAR = par_yield_fir_linear_slow_a + par_yield_fir_linear_slow_b * STDAGE,
    VOLBFNET_ACRE_FIR_LOGISTIC = par_yield_fir_logistic_slow_k / (1 + par_yield_fir_logistic_slow_a * exp(-par_yield_fir_logistic_slow_b * STDAGE)),
    VOLBFNET_ACRE_FIR_CHANG = exp(par_yield_fir_chang_slow_0) * exp(par_yield_fir_chang_slow_1 * STDAGE ^ -1 + par_yield_fir_chang_slow_2 * STDAGE ^ -2),
    VOLBFNET_ACRE_FIR_CHAPMANRICHARDS = exp(par_yield_fir_chapmanrichards_slow_a) * (1 - exp(- par_yield_fir_chapmanrichards_slow_k * STDAGE)) ^ par_yield_fir_chapmanrichards_slow_p,
    # Growth
    ANN_NET_GROWTH_ACRE_FIR_LINEAR = par_growth_fir_linear_slow_a + par_growth_fir_linear_slow_b * EST_BEGIN_ACRE,
    ANN_NET_GROWTH_ACRE_FIR_LOGISTIC = par_growth_fir_logistic_slow_r * EST_BEGIN_ACRE * (1 - EST_BEGIN_ACRE / par_growth_fir_logistic_slow_k),
    ANN_NET_GROWTH_ACRE_FIR_RICKER = EST_BEGIN_ACRE * exp(par_growth_fir_ricker_slow_r * (1 - EST_BEGIN_ACRE / par_growth_fir_ricker_slow_k)),
    ANN_NET_GROWTH_ACRE_FIR_BEVERTONHOLT = EST_BEGIN_ACRE * (par_growth_fir_bevertonholt_slow_r / (1 + ((par_growth_fir_bevertonholt_slow_r - 1) / par_growth_fir_bevertonholt_slow_k) * EST_BEGIN_ACRE)) 
  )  

dat_fir_model_slow_yield = 
  dat_fir_model_slow %>% 
  select(VALUE_X = STDAGE, starts_with("VOLBFNET_")) %>% 
  pivot_longer(cols = starts_with("VOLBFNET_"),
               names_prefix = "VOLBFNET_ACRE_",
               names_to = "MODEL",
               values_to = "VALUE_Y") %>% 
  mutate(
    MODEL = 
      MODEL %>% 
      str_remove_all("FIR_") %>% 
      str_replace_all("VOLBFNET_ACRE", "OBSERVED"),
    SUBSET = "4-7",
    VARIABLE = "YIELD"
  )

dat_fir_model_slow_growth = 
  dat_fir_model_slow %>% 
  select(VALUE_X = EST_BEGIN_ACRE, starts_with("ANN_NET_GROWTH_")) %>% 
  drop_na(VALUE_X) %>% 
  pivot_longer(cols = starts_with("ANN_NET_GROWTH_"),
               names_prefix = "ANN_NET_GROWTH_ACRE_",
               names_to = "MODEL",
               values_to = "VALUE_Y") %>% 
  mutate(
    MODEL = 
      MODEL %>% 
      str_remove_all("FIR_") %>% 
      str_replace_all("ANN_NET_GROWTH_ACRE", "OBSERVED"),
    SUBSET = "4-7",
    VARIABLE = "GROWTH"
  )

# Combine

dat_fir_models = 
  dat_fir_model_yield %>% 
  bind_rows(dat_fir_model_growth) %>% 
  bind_rows(dat_fir_model_fast_yield) %>% 
  bind_rows(dat_fir_model_fast_growth) %>% 
  bind_rows(dat_fir_model_slow_yield) %>% 
  bind_rows(dat_fir_model_slow_growth) %>% 
  select(VALUE_X, VALUE_Y, VARIABLE, SUBSET, MODEL) %>% 
  arrange(VARIABLE, SUBSET, MODEL, VALUE_X, VALUE_Y) %>% 
  filter(VALUE_Y != max(VALUE_Y))

# Tabulate

modelsummary(
  list("Linear" = mod_yield_fir_linear,
       "Logistic" = mod_yield_fir_logistic,
       "Chang" = mod_yield_fir_chang,
       "Chapman-Richards" = mod_yield_fir_chapmanrichards),
  stars = TRUE, 
  output = "flextable") |> 
  autofit() |> 
  save_as_docx(path = "04_out/Presentation_20260805/Table_Yield_Fir_All.docx")

modelsummary(
  list("Linear" = mod_growth_fir_linear,
       "Logistic" = mod_growth_fir_logistic,
       "Ricker" = mod_growth_fir_ricker,
       "Beverton-Holt" = mod_growth_fir_bevertonholt),
  stars = TRUE, 
  output = "flextable") |> 
  autofit() |> 
  save_as_docx(path = "04_out/Presentation_20260805/Table_Growth_Fir_All.docx")

# Visualize

vis_fir_models_yield = 
  dat_fir_models %>%
  filter(VARIABLE == "YIELD") %>% 
  mutate(
    Model = 
      MODEL %>% 
      str_to_sentence %>% 
      str_replace_all("Chapmanrichards", "Chapman-Richards") %>% 
      factor %>% 
      fct_relevel(
        "Linear",
        "Logistic",
        "Chang",
        "Chapman-Richards"
      ),
    Subset = 
      ifelse(SUBSET == "ALL", "All Site Classes", SUBSET) %>% 
      factor %>% 
      fct_relevel(
        "All Site Classes",
        "1-3",
        "4-7"
      )
  ) %>% 
  ggplot() + 
  geom_point(data = . %>% filter(MODEL == "OBSERVED"),
             aes(x = VALUE_X, y = VALUE_Y / 1000), 
             shape = 21,
             alpha = 0.25,
             fill = NA) + 
  geom_line(data = . %>% filter(MODEL != "OBSERVED"),
            aes(x = VALUE_X, y = VALUE_Y / 1000, group = Model, color = Model),
            linewidth = 1.25) + 
  labs(x = "Stand Age",
       y = "Stand Yield (MBF/Acre)") +
  facet_wrap(~ Subset) +
  theme_pubr() +
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        legend.title = element_blank())

vis_fir_models_growth = 
  dat_fir_models %>%
  filter(VARIABLE == "GROWTH") %>% 
  mutate(
    Model = 
      MODEL %>% 
      str_to_sentence %>% 
      str_replace_all("Bevertonholt", "Beverton-Holt") %>% 
      factor %>% 
      fct_relevel(
        "Linear",
        "Logistic",
        "Ricker",
        "Beverton-Holt"
      ),
    Subset = 
      ifelse(SUBSET == "ALL", "All Site Classes", SUBSET) %>% 
      factor %>% 
      fct_relevel(
        "All Site Classes",
        "1-3",
        "4-7"
      )
  ) %>% 
  ggplot() + 
  geom_point(data = . %>% filter(MODEL == "OBSERVED"),
             aes(x = VALUE_X / 1000, y = VALUE_Y / 1000), 
             shape = 21,
             alpha = 0.25,
             fill = NA) + 
  geom_line(data = . %>% filter(MODEL != "OBSERVED"),
            aes(x = VALUE_X / 1000, y = VALUE_Y / 1000, group = Model, color = Model),
            linewidth = 1.25) + 
  labs(x = "Initial Stand Yield (MBF/Acre)",
       y = "Stand Growth (MBF/Acre/Year)") +
  facet_wrap(~ Subset) +
  theme_pubr() +
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        legend.title = element_blank())

ggsave("04_out/Presentation_20260805/vis_model_yield.png",
       vis_fir_models_yield,
       dpi = 300,
       width = 9,
       height = 4)
  
ggsave("04_out/Presentation_20260805/vis_model_growth.png",
       vis_fir_models_growth,
       dpi = 300,
       width = 9,
       height = 4)
