# Visualize time series of yield, supply, and time-varying covariates.

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
  group_by(Series, Lag) %>% 
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
  # Compute seasonal (quarterly) mean deviations from the overall mean by series.
  #  Overall means
  group_by(Series) %>% 
  mutate(
    Value_Level_Total_Mean = mean(Value_Level, na.rm = TRUE),
    Value_FD_Total_Mean = mean(Value_FD, na.rm = TRUE)
  ) %>% 
  ungroup %>% 
  #  Seasonal (quarterly) means
  group_by(Series, Quarter) %>% 
  mutate(
    Value_Level_Quarter_Mean = mean(Value_Level, na.rm = TRUE),
    Value_FD_Quarter_Mean = mean(Value_FD, na.rm = TRUE)
  ) %>% 
  ungroup %>% 
  # Compute demeaned level and change values. 
  mutate(
    Value_Level_SD = Value_Level - (Value_Level_Quarter_Mean - Value_Level_Total_Mean),
    Value_FD_SD = Value_FD - (Value_FD_Quarter_Mean - Value_FD_Total_Mean)
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
  dat_yield %>% 
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
  mutate(
    Series = Series %>% str_remove("Supply_"),
    Measure = 
      Measure %>% 
      factor %>% 
      fct_relevel("Level", "Level_SD", "FD", "FD_SD"),
    Metric = 
      Metric %>% 
      factor %>% 
      fct_relevel("Mean_1", "Mean_4", "Mean_8", "Mean_12")
  ) %>%
  mutate(Value = Value / 1000) %>% 
  ggplot() +
  geom_vline(xintercept = "2020_Q1", linetype = "dashed", color = "gray50") +
  geom_line(aes(x = Year_Quarter, y = Value, color = Series, group = Series)) +
  scale_x_discrete(breaks = c("2015_Q1", "2020_Q1", "2024_Q1")) +
  labs(x = NULL, y = "MMBF") +
  facet_grid(
    Measure ~ Metric,
    scales = "free_y"
  ) +
  theme_pubr() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#  Prices | Douglas fir

vis_price_douglasfir = 
  dat_variables %>% 
  filter(Series %>% str_detect("DouglasFir") & Series %>% str_detect("Price")) %>% 
  select(Year_Quarter, Series, Measure, Mean_1 = Lag_1, Mean_4, Mean_8, Mean_12) %>% 
  pivot_longer(starts_with("Mean"), names_to = "Metric", values_to = "Value") %>% 
  mutate(
    Series = Series %>% str_remove("Price_") %>% str_remove("_DouglasFir"),
    Measure = 
      Measure %>% 
      factor %>% 
      fct_relevel("Level", "Level_SD", "FD", "FD_SD"),
    Metric = 
      Metric %>% 
      factor %>% 
      fct_relevel("Mean_1", "Mean_4", "Mean_8", "Mean_12")
  ) %>%
  ggplot() +
  geom_vline(xintercept = "2020_Q1", linetype = "dashed", color = "gray50") +
  geom_line(aes(x = Year_Quarter, y = Value, color = Series, group = Series)) +
  scale_x_discrete(breaks = c("2015_Q1", "2020_Q1", "2024_Q1")) +
  labs(x = NULL, y = "2024 USD / MBF") +
  facet_grid(
    Measure ~ Metric,
    scales = "free_y"
  ) +
  theme_pubr() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#  Prices | Western hemlock

vis_price_westernhemlock = 
  dat_variables %>% 
  filter((Series %>% str_detect("WesternHemlock") | Series %>% str_detect("HemFir")) & Series %>% str_detect("Price")) %>% 
  select(Year_Quarter, Series, Measure, Mean_1 = Lag_1, Mean_4, Mean_8, Mean_12) %>% 
  pivot_longer(starts_with("Mean"), names_to = "Metric", values_to = "Value") %>% 
  mutate(
    Series = Series %>% str_remove("Price_") %>% str_remove("_WesternHemlock") %>% str_remove("_HemFir"),
    Measure = 
      Measure %>% 
      factor %>% 
      fct_relevel("Level", "Level_SD", "FD", "FD_SD"),
    Metric = 
      Metric %>% 
      factor %>% 
      fct_relevel("Mean_1", "Mean_4", "Mean_8", "Mean_12")
  ) %>%
  ggplot() +
  geom_vline(xintercept = "2020_Q1", linetype = "dashed", color = "gray50") +
  geom_line(aes(x = Year_Quarter, y = Value, color = Series, group = Series)) +
  scale_x_discrete(breaks = c("2015_Q1", "2020_Q1", "2024_Q1")) +
  labs(x = NULL, y = "2024 USD / MBF") +
  facet_grid(
    Measure ~ Metric,
    scales = "free_y"
  ) +
  theme_pubr() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#  Prices | Composite

vis_price_composite = 
  dat_variables %>% 
  filter(Series == "Price_Composite") %>% 
  select(Year_Quarter, Series, Measure, Mean_1 = Lag_1, Mean_4, Mean_8, Mean_12) %>% 
  pivot_longer(starts_with("Mean"), names_to = "Metric", values_to = "Value") %>% 
  mutate(
    Measure = 
      Measure %>% 
      factor %>% 
      fct_relevel("Level", "Level_SD", "FD", "FD_SD"),
    Metric = 
      Metric %>% 
      factor %>% 
      fct_relevel("Mean_1", "Mean_4", "Mean_8", "Mean_12")
  ) %>%
  ggplot() +
  geom_vline(xintercept = "2020_Q1", linetype = "dashed", color = "gray50") +
  geom_line(aes(x = Year_Quarter, y = Value, color = Series, group = Series)) +
  scale_x_discrete(breaks = c("2015_Q1", "2020_Q1", "2024_Q1")) +
  labs(x = NULL, y = "2024 USD / MBF") +
  facet_grid(
    Measure ~ Metric,
    scales = "free_y"
  ) +
  theme_pubr() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#  Federal Funds Rate

vis_rate = 
  dat_variables %>% 
  filter(Series == "Rate") %>% 
  select(Year_Quarter, Series, Measure, Mean_1 = Lag_1, Mean_4, Mean_8, Mean_12) %>% 
  pivot_longer(starts_with("Mean"), names_to = "Metric", values_to = "Value") %>% 
  mutate(
    Measure = 
      Measure %>% 
      factor %>% 
      fct_relevel("Level", "Level_SD", "FD", "FD_SD"),
    Metric = 
      Metric %>% 
      factor %>% 
      fct_relevel("Mean_1", "Mean_4", "Mean_8", "Mean_12")
  ) %>%
  ggplot() +
  geom_vline(xintercept = "2020_Q1", linetype = "dashed", color = "gray50") +
  geom_line(aes(x = Year_Quarter, y = Value, color = Series, group = Series)) +
  scale_x_discrete(breaks = c("2015_Q1", "2020_Q1", "2024_Q1")) +
  labs(x = NULL, y = "Federal Funds Rate") +
  facet_grid(
    Measure ~ Metric,
    scales = "free_y"
  ) +
  theme_pubr() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#  Climate | CWD
#   Does this work at all without subannual CWD?

#  Climate | VPD

vis_vpd = 
  dat_variables %>% 
  filter(Series == "VPD") %>% 
  select(Year_Quarter, Series, Measure, Mean_1 = Lag_1, Mean_4, Mean_8, Mean_12) %>% 
  pivot_longer(starts_with("Mean"), names_to = "Metric", values_to = "Value") %>% 
  mutate(
    Measure = 
      Measure %>% 
      factor %>% 
      fct_relevel("Level", "Level_SD", "FD", "FD_SD"),
    Metric = 
      Metric %>% 
      factor %>% 
      fct_relevel("Mean_1", "Mean_4", "Mean_8", "Mean_12")
  ) %>%
  ggplot() +
  geom_vline(xintercept = "2020_Q1", linetype = "dashed", color = "gray50") +
  geom_line(aes(x = Year_Quarter, y = Value, color = Series, group = Series)) +
  scale_x_discrete(breaks = c("2015_Q1", "2020_Q1", "2024_Q1")) +
  labs(x = NULL, y = "Vapor Pressure Deficit") +
  facet_grid(
    Measure ~ Metric,
    scales = "free_y"
  ) +
  theme_pubr() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#  Climate | ???

#  Climate | Fire
#   Note that this is nonsense without resolving MTBS to some subannual scale. 

#  Correlations

#   note that correlations between measures (e.g. FD vs FD SD) are hard to interpret

#   Demo

dat_variables %>% 
  filter(Series == "Rate" & Measure == "Level") %>% 
  select(starts_with("Lag")) %>% 
  drop_na %>% 
  cor

#   Auto

# supply-supply
# stumpage-stumpage
# kiln-kiln

#   X-X

# kiln and stumpage

#   Y-X 

# stumpage and supply
# kiln and supply

# extend to yield, rate, climate, fire
