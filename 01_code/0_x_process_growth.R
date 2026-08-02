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

#   Wrangling

dat_use = 
  # Handle tree data.
  dat_tree %>% 
  filter(!is.na(VOLBFNET)) %>%
  filter(SPCD %in% c(202, 263)) %>%
  mutate(VOLBFNET_ACRE = VOLBFNET * TPA_UNADJ) %>% 
  group_by(
    INVYR,
    PLT_CN,
    CONDID,
    SPCD
  ) %>% 
  summarize(VOLBFNET_ACRE = sum(VOLBFNET_ACRE, na.rm = TRUE)) %>% 
  ungroup %>% 
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
  # Handle older-than-useful conditions.
  filter(ntile(STDAGE, 100) <= 99) %>% 
  # Handle outliers.
  filter(ntile(VOLBFNET_ACRE, 100) %in% 2:99)

# This returns 2356 observations for INVYR 1999-2021, MEASYEAR 1995-2023. 
# 1802 Douglas fir, 554 western hemlock. 
#  Note that this is not the latest FIA release, so updating the data could help.

# For comparison, Chisholm and Gray get 747, 1767 for INVYR 2010-2019 for a larger region. 

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

#  Linear

mod_fir_linear = 
  dat_use %>% 
  as_tibble %>% 
  filter(SPCD == 202) %>% 
  filter(STDAGE < 75) %>% 
  lm(VOLBFNET_ACRE ~ STDAGE, data = .)

par_fir_linear_a = 
  mod_fir_linear %>% 
  coef %>% 
  magrittr::extract(1)

par_fir_linear_b = 
  mod_fir_linear %>% 
  coef %>% 
  magrittr::extract(2)

#  Exponential

mod_fir_exponential = 
  dat_use %>% 
  as_tibble %>% 
  filter(SPCD == 202) %>% 
  filter(STDAGE < 75) %>% 
  mutate(VOLBFNET_ACRE_LOG = VOLBFNET_ACRE %>% log) %>% 
  lm(VOLBFNET_ACRE_LOG ~ STDAGE, data = .)

par_fir_exponential_a = 
  mod_fir_exponential %>% 
  coef %>% 
  magrittr::extract(1) %>% 
  exp

par_fir_exponential_b = 
  mod_fir_exponential %>% 
  coef %>% 
  magrittr::extract(2)

#  Ricker
#  Beverton-Holt
#  Chapman-Richards

#  Varying: site class, species handling?, ecoregion, county

# Estimation w/ Stochastic Component


# Visualization

#  each model with residuals against data separately
#  then all models together with data faded to minimize visual noise
#  residuals in space might be interesting; or residuals on other covariates

dat_fir_model = 
  dat_use %>% 
  as_tibble %>% 
  filter(SPCD == 202) %>% 
  filter(STDAGE < 75) %>% 
  mutate(
    VOLBFNET_ACRE_FIR_LINEAR = par_fir_linear_a + par_fir_linear_b * STDAGE,
    VOLBFNET_ACRE_FIR_EXPONENTIAL = par_fir_exponential_a * exp(par_fir_exponential_b * STDAGE)
    )
  
vis_fir_model = 
  dat_fir_model %>% 
  ggplot() +
  geom_point(aes(x = STDAGE, y = VOLBFNET_ACRE), color = "red", alpha = 0.25) +
  geom_point(aes(x = STDAGE, y = VOLBFNET_ACRE_FIR_LINEAR), color = "blue", alpha = 0.50)


vis_fir_model = 
  dat_fir_model %>% 
  ggplot() +
  geom_point(aes(x = STDAGE, y = VOLBFNET_ACRE), color = "red", alpha = 0.25) +
  geom_point(aes(x = STDAGE, y = VOLBFNET_ACRE_FIR_EXPONENTIAL), color = "blue", alpha = 0.50)
  

# remeasurement check

dat_check = 
  "02_data/1_5_1_FIA/OR_TREE_GRM_ESTN.csv" %>% 
  read_csv %>% 
  filter(COMPONENT == "SURVIVOR") %>% 
  filter(LAND_BASIS %in% c("FORESTLAND", "TIMBERLAND")) %>% 
  filter(ESTIMATE == "VOLBFNET") %>% 
  filter(ANN_NET_GROWTH > 0) %>% 
  select(INVYR, PLT_CN, TRE_CN, REMPER, EST_BEGIN, EST_END, ANN_NET_GROWTH) %>% 
  left_join(dat_tree, .) %>% 
  drop_na(ANN_NET_GROWTH) %>% 
  distinct %>% # Not worth figuring out why the join returns duplicates.
  filter(SPCD %in% c(202, 263))

# takeaway: counting on remeasurement is fine, actually: 5607 fir/hem conditions; 5463 fir; 1378 hem

# Reference Code

mod = 
  dat %>% 
  mutate(Y = log(MBF_Annual / MBF_0),
         X_1 = MBF_0,
         X_2 = SITECLCD_Bin, 
         .keep = "none") %>% 
  lm(Y ~ X_1 + X_2,
     data = .)

b_0 = mod$coefficients[[1]]
b_1 = mod$coefficients[[2]]
b_2 = mod$coefficients[[3]]

stargazer(mod, type = "html")

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
