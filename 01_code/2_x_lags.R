# Visualize time series of raw and distributed values for time-varying covariates.

#  Try deltas as well as level values. 
#  Try doing something about seasonality. 
#  Note Almon polynomial weights on distributed lags as . . . a thing. 
# probably throw in a big all-all correlation matrix

# raw | 1-y mean | 3-y mean | 5-y mean | 10-y mean

# correlations within raw X | correlations with lags (corr matrices I guess?)

# correlations between Y, X

# probably read in all the data, join and pivot to YQ-Series-Value format, compute lags and means, then filter for each ... thing.

# so, column goals:
# lags 1:40
# means 1, 4, 8, 12
# level values less seasonal means
# change values
# change values less seasonal means

# maybe need demeaned levels, change, demeaned change before lags, means
# then pivot again to YQ-Metric-Series-Value for lags/means?

#  Data

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

# Supply (actually yield)
# Prices
# Climate | CWD, Cumulative Precipitation, VPD?
# Fire

#  Supply | Douglas fir

#  Supply | Western hemlock

#  Prices | Douglas fir

vis_price_douglasfir = 
  dat %>% 
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
