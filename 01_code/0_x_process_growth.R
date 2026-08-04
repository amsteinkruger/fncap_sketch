# Wrangle FIA and estimate growth models.

# Grab disturbance and treatment variables, then reduce meaningfully. 

#  Data

#   Bounds

dat_bounds = "03_intermediate/dat_bounds.gdb" %>% vect

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
  mutate(across(ends_with("ACRE"), ~ ifelse(.x == 0, NA, .x))) %>% 
  # Handle condition data.
  left_join(dat_condition) %>% 
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
  as_tibble %>% 
  # Cut western hemlock for now. 
  filter(SPCD == 202) %>%
  # Cut stands older than 75 years for now.
  filter(STDAGE <= 100) %>%
  # Handle outliers.
  filter(ntile(VOLBFNET_ACRE, 100) %in% 2:99) %>% 
  filter(ntile(VOLBFNET_ACRE / STDAGE, 100) %in% 2:99) %>% 
  filter(ntile(EST_BEGIN_ACRE, 100) %in% 2:99 | is.na(EST_BEGIN_ACRE)) %>% 
  filter(ntile(ANN_NET_GROWTH_ACRE, 100) %in% 2:99 | is.na(ANN_NET_GROWTH_ACRE)) %>% 
  filter(ntile(ANN_NET_GROWTH_ACRE / VOLBFNET_ACRE, 100) %in% 2:99 | is.na(ANN_NET_GROWTH_ACRE))

# 2262 observations for Douglas fir without age restriction.
# 2192 observations for Douglas fir with STDAGE < 100. 

plot(dat_use$STDAGE, dat_use$VOLBFNET_ACRE)
plot(dat_use$EST_BEGIN_ACRE, dat_use$ANN_NET_GROWTH_ACRE)

# Visualization

vis_1 = 
  dat_use %>%
  as_tibble %>% 
  ggplot(aes(x = STDAGE,
             y = VOLBFNET_ACRE)) + 
  geom_point(alpha = 0.25) +
  facet_wrap(~ SPCD) +
  theme_minimal() 

# Estimation

#  SITECLCD Bins

vec_fast = 1:3
vec_slow = 4:7

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

dat_fir_model_slow = 
  dat_use %>% 
  filter(SITECLCD %!in% vec_slow) %>% 
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




vis_yield_fir_linear = 
  dat_fir_model %>% 
  ggplot() +
  geom_point(aes(x = STDAGE, y = VOLBFNET_ACRE), color = "red", alpha = 0.25) +
  geom_point(aes(x = STDAGE, y = VOLBFNET_ACRE_FIR_LINEAR), color = "blue", alpha = 0.50)

vis_yield_fir_exponential = 
  dat_fir_model %>% 
  ggplot() +
  geom_point(aes(x = STDAGE, y = VOLBFNET_ACRE), color = "red", alpha = 0.25) +
  geom_point(aes(x = STDAGE, y = VOLBFNET_ACRE_FIR_EXPONENTIAL), color = "blue", alpha = 0.50)

vis_yield_fir_logistic = 
  dat_fir_model %>% 
  ggplot() +
  geom_point(aes(x = STDAGE, y = VOLBFNET_ACRE), color = "red", alpha = 0.25) +
  geom_point(aes(x = STDAGE, y = VOLBFNET_ACRE_FIR_LOGISTIC), color = "blue", alpha = 0.50)

vis_yield_fir_chang = 
  dat_fir_model %>% 
  ggplot() +
  geom_point(aes(x = STDAGE, y = VOLBFNET_ACRE), color = "red", alpha = 0.25) +
  geom_point(aes(x = STDAGE, y = VOLBFNET_ACRE_FIR_CHANG), color = "blue", alpha = 0.50)

vis_yield_fir_chapmanrichards = 
  dat_fir_model %>% 
  ggplot() +
  geom_point(aes(x = STDAGE, y = VOLBFNET_ACRE), color = "red", alpha = 0.25) +
  geom_point(aes(x = STDAGE, y = VOLBFNET_ACRE_FIR_CHAPMANRICHARDS), color = "blue", alpha = 0.50)

vis_growth_fir_linear = 
  dat_fir_model %>% 
  ggplot() +
  geom_point(aes(x = EST_BEGIN_ACRE, y = ANN_NET_GROWTH_ACRE), color = "red", alpha = 0.25) +
  geom_point(aes(x = EST_BEGIN_ACRE, y = ANN_NET_GROWTH_ACRE_FIR_LINEAR), color = "blue", alpha = 0.50)

vis_growth_fir_exponential = 
  dat_fir_model %>% 
  ggplot() +
  geom_point(aes(x = EST_BEGIN_ACRE, y = ANN_NET_GROWTH_ACRE), color = "red", alpha = 0.25) +
  geom_point(aes(x = EST_BEGIN_ACRE, y = ANN_NET_GROWTH_ACRE_FIR_EXPONENTIAL), color = "blue", alpha = 0.50)

vis_growth_fir_logistic = 
  dat_fir_model %>% 
  ggplot() +
  geom_point(aes(x = EST_BEGIN_ACRE, y = ANN_NET_GROWTH_ACRE), color = "red", alpha = 0.25) +
  geom_point(aes(x = EST_BEGIN_ACRE, y = ANN_NET_GROWTH_ACRE_FIR_LOGISTIC), color = "blue", alpha = 0.50)

vis_growth_fir_ricker = 
  dat_fir_model %>% 
  ggplot() +
  geom_point(aes(x = EST_BEGIN_ACRE, y = ANN_NET_GROWTH_ACRE), color = "red", alpha = 0.25) +
  geom_point(aes(x = EST_BEGIN_ACRE, y = ANN_NET_GROWTH_ACRE_FIR_RICKER), color = "blue", alpha = 0.50)

vis_growth_fir_bevertonholt = 
  dat_fir_model %>% 
  ggplot() +
  geom_point(aes(x = EST_BEGIN_ACRE, y = ANN_NET_GROWTH_ACRE), color = "red", alpha = 0.25) +
  geom_point(aes(x = EST_BEGIN_ACRE, y = ANN_NET_GROWTH_ACRE_FIR_BEVERTONHOLT), color = "blue", alpha = 0.50)

# Reference Code

# Results Visualization

vis_4 = 
  dat %>% 
  ggplot() + 
  geom_point(aes(x = MBF_0,
                 y = MBF_Annual,
                 color = SITECLCD_Bin %>% factor(labels = c("1-3", "4-6"))),
             shape = 21,
             alpha = 0.50, 
             fill = NA) +
  geom_function(aes(x = MBF_0,
                    color = "1-3"),
                fun = ~ .x * exp(b_0 + b_1 * .x)) +
  geom_function(aes(x = MBF_0,
                    color = "4-6"),
                fun = ~ .x * exp(b_0 + b_1 * .x + b_2)) +
  scale_color_manual(values = c("black", color_beav)) +
  labs(x = "Initial MBF/Acre",
       y = "Annualized Change in MBF/Acre",
       color = "Site Class") +
  theme_minimal()

ggsave("out/vis_4.png",
       vis_4,
       dpi = 300,
       width = 6,
       height = 4.5)
