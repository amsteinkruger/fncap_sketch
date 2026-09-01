# Visualize time series of yield, supply, and time-varying covariates.

#  Try deltas as well as level values. 
#  Try doing something about seasonality. 

# correlations within raw X

# correlations with lags (corr matrices I guess?)

# correlations between Y, X

# so, column goals:
# (x) lags 1:40
# (x) means 1, 4, 8, 12
# ( ) level values less seasonal means
# ( ) change values
# ( ) change values less seasonal means

# maybe need demeaned levels, change, demeaned change before lags, means
# then pivot again to YQ-Metric-Series-Value for lags/means?

#  Data

dat_yield = 
  "03_intermediate/dat_notifications_1_9.csv" %>% 
  read_csv %>% 
  filter(Activity == "Clearcut/Overstory Removal") %>% 
  select(
    Year_Quarter = QuarterCompletion,
    DouglasFir = MBF_Acre_2_DouglasFir,
    WesternHemlock = MBF_Acre_2_WesternHemlock
  ) %>% 
  mutate(Year_Quarter = Year_Quarter %>% str_replace("_", "_Q")) %>% 
  pivot_longer(-Year_Quarter, names_to = "Species", values_to = "Yield")

#   Note that adding supply data to covariates with bind_rows could make more sense. 

dat_supply = 
  "03_intermediate/dat_notifications_1_9.csv" %>% 
  read_csv %>% 
  # filter(Activity == "Clearcut/Overstory Removal") %>% 
  select(
    Year_Quarter = QuarterCompletion,
    DouglasFir = MBF_2_DouglasFir,
    WesternHemlock = MBF_2_WesternHemlock
  ) %>% 
  mutate(Year_Quarter = Year_Quarter %>% str_replace("_", "_Q")) %>% 
  pivot_longer(-Year_Quarter, names_to = "Species", values_to = "Supply") %>% 
  group_by(Year_Quarter, Species) %>% 
  summarize(Supply = sum(Supply)) %>% 
  ungroup %>% 
  arrange(Year_Quarter) %>% 
  group_by(Species) %>% 
  mutate(across(Supply, setNames(lapply(1:40, \(k) ~ lag(.x, k)), paste0("Lag_", 1:40)))) %>% 
  ungroup %>% 
  rename(Supply_Lag_0 = Supply) %>% # Some covariates need this tweak as well. 
  pivot_longer(starts_with("Supply"), names_to = "Series_Lag", values_to = "Value") %>% 
  separate(Series_Lag, into = c("Series", "Lag"), sep = "_Lag_") %>% 
  mutate(Lag = Lag %>% as.numeric %>% replace_na(0)) %>% 
  pivot_wider(names_from = Lag, names_prefix = "Lag_", values_from = Value) %>% 
  group_by(Series) %>% 
  mutate(
    Mean_4 = 
      rowMeans(
        pick(starts_with("Lag_") & ends_with(paste0("_", 1:4))),
        na.rm = TRUE
      ),
    Mean_8 = 
      rowMeans(
        pick(starts_with("Lag_") & ends_with(paste0("_", 1:8))),
        na.rm = TRUE
      ),
    Mean_12 = 
      rowMeans(
        pick(starts_with("Lag_") & ends_with(paste0("_", 1:12))),
        na.rm = TRUE
      )
  ) %>% 
  ungroup

dat_covariates = 
  "03_intermediate/dat_notifications_1_7.csv" %>% 
  read_csv %>% 
  select(-UID, -Year, -Quarter) %>% 
  group_by(Year_Quarter) %>% 
  summarize(across(everything(), ~ mean(.x, na.rm = TRUE))) %>% 
  ungroup %>% 
  pivot_longer(-Year_Quarter, names_to = "Series_Lag", values_to = "Value") %>% 
  separate(Series_Lag, into = c("Series", "Lag"), sep = "_Lag_") %>% 
  mutate(Lag = Lag %>% as.numeric %>% replace_na(0)) %>% 
  pivot_wider(names_from = Lag, names_prefix = "Lag_", values_from = Value) %>% 
  group_by(Series) %>% 
  mutate(
    Mean_4 = 
      rowMeans(
        pick(starts_with("Lag_") & ends_with(paste0("_", 1:4))),
        na.rm = TRUE
      ),
    Mean_8 = 
      rowMeans(
        pick(starts_with("Lag_") & ends_with(paste0("_", 1:8))),
        na.rm = TRUE
      ),
    Mean_12 = 
      rowMeans(
        pick(starts_with("Lag_") & ends_with(paste0("_", 1:12))),
        na.rm = TRUE
      )
  ) %>% 
  ungroup

# Yield | Douglas fir and western hemlock

vis_yield = 
  dat_harvest %>% 
  filter(Yield < 100) %>% 
  ggplot() + 
  geom_boxplot(
    aes(x = Year_Quarter, y = Yield, color = Species),
    outliers = FALSE
  ) +
  facet_wrap(~ Species) +
  theme(legend.position = "none")

#  Supply

vis_supply = 
  dat_supply %>% 
  select(Year_Quarter, Species, Series, Mean_1 = Lag_1, Mean_4, Mean_8, Mean_12) %>% 
  pivot_longer(starts_with("Mean"), names_to = "Metric", values_to = "Value") %>% 
  mutate(Metric = Metric %>% factor %>% fct_relevel("Mean_1", "Mean_4", "Mean_8", "Mean_12")) %>%
  ggplot() +
  geom_line(aes(x = Year_Quarter, y = Value, color = Species, group = Species)) +
  facet_wrap(~ Metric, nrow = 1)

#  Prices | Douglas fir

vis_price_douglasfir = 
  dat_covariates %>% 
  mutate(Keep = Series %>% str_detect("DouglasFir")) %>% 
  filter(Keep) %>% 
  select(Year_Quarter, Series, Mean_1 = Lag_1, Mean_4, Mean_8, Mean_12) %>% 
  pivot_longer(starts_with("Mean"), names_to = "Metric", values_to = "Value") %>% 
  mutate(Series = Series %>% str_remove("Price_") %>% str_remove("_DouglasFir"),
         Metric = Metric %>% factor %>% fct_relevel("Mean_1", "Mean_4", "Mean_8", "Mean_12")) %>%
  ggplot() +
  geom_line(aes(x = Year_Quarter, y = Value, color = Series, group = Series)) +
  facet_wrap(~ Metric, nrow = 1)

#  Prices | Western hemlock

#  Prices | Composite

#  Climate | CWD

#  Climate | VPD

#  Climate | ???

#  Climate | Fire
