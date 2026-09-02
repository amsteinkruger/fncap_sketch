# Visualize time series of yield, supply, and time-varying covariates.

# correlations within raw X

# correlations with lags (corr matrices I guess?)

# correlations between Y, X

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
  rename(Supply_Lag_0 = Supply) %>% 
  pivot_wider(
    names_from = Species,
    names_glue = "Supply_{Species}_{.value}", # This works but doesn't make any sense. 
    values_from = starts_with("Supply")
  ) %>% 
  rename_with(~ str_remove(.x, "_Supply"))

dat_variables = 
  "03_intermediate/dat_notifications_1_7.csv" %>% 
  read_csv %>% 
  select(-UID, -Year, -Quarter) %>% 
  # Reduce across notifications.
  group_by(Year_Quarter) %>% 
  summarize(across(everything(), ~ mean(.x, na.rm = TRUE))) %>% 
  ungroup %>% 
  # Add supply. 
  left_join(dat_supply) %>% 
  # Switch to long data for operations across series.
  pivot_longer(-Year_Quarter, names_to = "Series_Lag", values_to = "Value") %>% 
  separate(Series_Lag, into = c("Series", "Lag"), sep = "_Lag_") %>% 
  mutate(Lag = Lag %>% as.numeric %>% replace_na(0)) %>% 
  filter(str_sub(Series, 1, 4) != "Fire") %>% # This is arbitrary. Note the same problem comes with CWD. 
  # Compute first order differences.
  rename(Value_Level = Value) %>% # For cleaner references. 
  group_by(Series) %>% 
  mutate(Value_FD = Value_Level - lag(Value_Level)) %>% 
  ungroup %>% 
  # Compute quarters for each lag. 
  mutate(
    Quarter = 
      Year_Quarter %>% 
      str_sub(-1, -1) %>% 
      as.numeric %>% 
      add(3) %>% 
      subtract(((Lag - 1) %% 4))
  ) %>% 
  # Compute seasonal (quarterly) means by series for (un)differenced values.
  group_by(Series, Quarter) %>% 
  mutate(
    Value_Level_Quarter_Mean = mean(Value_Level, na.rm = TRUE),
    Value_FD_Quarter_Mean = mean(Value_FD, na.rm = TRUE)
  ) %>% 
  ungroup %>% 
  # Compute demeaned level and change values. 
  mutate(
    Value_Level_SD = Value_Level - Value_Level_Quarter_Mean,
    Value_FD_SD = Value_FD - Value_FD_Quarter_Mean
  ) %>% 
  # Switch to even longer (!) data.
  select(
    Year_Quarter, 
    Series, 
    Lag, 
    Value_Level, 
    Value_Level_SD, 
    Value_FD, 
    Value_FD_SD
  ) %>% 
  pivot_longer(
    starts_with("Value"), 
    names_prefix = "Value_", 
    names_to = "Measure",
    values_to = "Value"
  ) %>% 
  # Switch to wider data to compute (multi-)annual means. 
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
  dat_variables %>% 
  filter(Series %>% str_detect("Supply")) %>% 
  select(Year_Quarter, Series, Measure, Mean_1 = Lag_1, Mean_4, Mean_8, Mean_12) %>% 
  pivot_longer(starts_with("Mean"), names_to = "Metric", values_to = "Value") %>% 
  mutate(Series = Series %>% str_remove("Price_") %>% str_remove("_DouglasFir"),
         Metric = Metric %>% factor %>% fct_relevel("Mean_1", "Mean_4", "Mean_8", "Mean_12")) %>%
  ggplot() +
  geom_line(aes(x = Year_Quarter, y = Value, color = Series, group = Series)) +
  facet_grid(Measure ~ Metric)

#  Prices | Douglas fir

vis_price_douglasfir = 
  dat_variables %>% 
  filter(Series %>% str_detect("DouglasFir") & Series %>% str_detect("Price")) %>% 
  select(Year_Quarter, Series, Measure, Mean_1 = Lag_1, Mean_4, Mean_8, Mean_12) %>% 
  pivot_longer(starts_with("Mean"), names_to = "Metric", values_to = "Value") %>% 
  mutate(Series = Series %>% str_remove("Price_") %>% str_remove("_DouglasFir"),
         Metric = Metric %>% factor %>% fct_relevel("Mean_1", "Mean_4", "Mean_8", "Mean_12")) %>%
  ggplot() +
  geom_line(aes(x = Year_Quarter, y = Value, color = Series, group = Series)) +
  facet_grid(Measure ~ Metric)

#  Prices | Western hemlock

#  Prices | Composite



#  Climate | CWD

#  Climate | VPD

#  Climate | ???

#  Climate | Fire
