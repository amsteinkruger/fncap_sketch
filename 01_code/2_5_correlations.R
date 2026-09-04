# Visualize time series of yield, supply, and time-varying covariates.

#  This script uses year-quarter-polygons from active notifications.
#  That's probably the wrong decision unit, or at best one that's hard to interpret.
#  Aggregating over all timberland or all mature timberland might make more sense. 

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

vis_cwd = 
  dat_variables %>% 
  filter(Series == "CWD") %>% 
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

#  Climate | Fire

vis_fire = 
  dat_variables %>% 
  filter(Series %>% str_detect("Fire")) %>% 
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
  labs(x = NULL, y = "Large Wildfire Count") +
  facet_grid(
    Measure ~ Metric,
    scales = "free_y"
  ) +
  theme_pubr() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#  Correlations

#   Auto

#    Supply, Douglas Fir

#     Level

cor_auto_supply_level =
  dat_variables %>% 
  filter(Series == "Supply_DouglasFir" & Measure == "Level") %>% 
  select(starts_with("Lag")) %>% 
  select(1:21) %>% 
  drop_na(Lag_1) %>% 
  cor(use = "na.or.complete") %>% 
  as_tibble %>% 
  mutate(Lag_X = paste0("Lag_", seq(0, 20))) %>% 
  relocate(Lag_X) %>% 
  pivot_longer(-Lag_X, names_to = "Lag_Y", values_to = "Correlation") %>% 
  mutate(across(starts_with("Lag"), ~ as.numeric(str_remove(.x, "Lag_")))) %>% 
  ggplot() +
  geom_tile(aes(x = Lag_X, y = Lag_Y, fill = Correlation)) +
  labs(x = "Supply (Quarter Lags)", y = "Supply (Quarter Lags)") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_gradientn(
    colors = c("purple", "white", "orange"),
    limits = c(-1, 1),
    breaks = c(-1, 0, 1)
  ) +
  theme(legend.ticks = element_blank())

#     Level, Season-Adjusted

cor_auto_supply_level_sd =
  dat_variables %>% 
  filter(Series == "Supply_DouglasFir" & Measure == "Level_SD") %>% 
  select(starts_with("Lag")) %>% 
  select(1:21) %>% 
  drop_na(Lag_1) %>% 
  cor(use = "na.or.complete") %>% 
  as_tibble %>% 
  mutate(Lag_X = paste0("Lag_", seq(0, 20))) %>% 
  relocate(Lag_X) %>% 
  pivot_longer(-Lag_X, names_to = "Lag_Y", values_to = "Correlation") %>% 
  mutate(across(starts_with("Lag"), ~ as.numeric(str_remove(.x, "Lag_")))) %>% 
  ggplot() +
  geom_tile(aes(x = Lag_X, y = Lag_Y, fill = Correlation)) +
  labs(x = "Supply (Season-Adjusted) (Quarter Lags)", y = "Supply (Season-Adjusted) (Quarter Lags)") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_gradientn(
    colors = c("purple", "white", "orange"),
    limits = c(-1, 1),
    breaks = c(-1, 0, 1)
  ) +
  theme(legend.ticks = element_blank())

#     First Differences

cor_auto_supply_fd_sd =
  dat_variables %>% 
  filter(Series == "Supply_DouglasFir" & Measure == "FD_SD") %>% 
  select(starts_with("Lag")) %>% 
  select(1:21) %>% 
  drop_na(Lag_1) %>% 
  cor(use = "na.or.complete") %>% 
  as_tibble %>% 
  mutate(Lag_X = paste0("Lag_", seq(0, 20))) %>% 
  relocate(Lag_X) %>% 
  pivot_longer(-Lag_X, names_to = "Lag_Y", values_to = "Correlation") %>% 
  mutate(across(starts_with("Lag"), ~ as.numeric(str_remove(.x, "Lag_")))) %>% 
  ggplot() +
  geom_tile(aes(x = Lag_X, y = Lag_Y, fill = Correlation)) +
  labs(x = "Supply (Season-Adjusted First Differences) (Quarter Lags)", y = "Supply (Season-Adjusted First Differences) (Quarter Lags)") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_gradientn(
    colors = c("purple", "white", "orange"),
    limits = c(-1, 1),
    breaks = c(-1, 0, 1)
  ) +
  theme(legend.ticks = element_blank())

#    Stumpage, Douglas Fir

#     Level

cor_auto_stumpage_level =
  dat_variables %>% 
  filter(Series == "Price_Stumpage_DouglasFir" & Measure == "Level") %>% 
  select(starts_with("Lag")) %>% 
  cor(use = "na.or.complete") %>% 
  as_tibble %>% 
  mutate(Lag_X = paste0("Lag_", seq(0, 40))) %>% 
  relocate(Lag_X) %>% 
  pivot_longer(-Lag_X, names_to = "Lag_Y", values_to = "Correlation") %>% 
  mutate(across(starts_with("Lag"), ~ as.numeric(str_remove(.x, "Lag_")))) %>% 
  ggplot() +
  geom_tile(aes(x = Lag_X, y = Lag_Y, fill = Correlation)) +
  labs(x = "Stumpage (Quarter Lags)", y = "Stumpage (Quarter Lags)") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_gradientn(
    colors = c("purple", "white", "orange"),
    limits = c(-1, 1),
    breaks = c(-1, 0, 1)
  ) +
  theme(legend.ticks = element_blank())

#     First Differences

cor_auto_stumpage_fd =
  dat_variables %>% 
  filter(Series == "Price_Stumpage_DouglasFir" & Measure == "FD") %>% 
  select(starts_with("Lag")) %>% 
  cor(use = "na.or.complete") %>% 
  as_tibble %>% 
  mutate(Lag_X = paste0("Lag_", seq(0, 40))) %>% 
  relocate(Lag_X) %>% 
  pivot_longer(-Lag_X, names_to = "Lag_Y", values_to = "Correlation") %>% 
  mutate(across(starts_with("Lag"), ~ as.numeric(str_remove(.x, "Lag_")))) %>% 
  ggplot() +
  geom_tile(aes(x = Lag_X, y = Lag_Y, fill = Correlation)) +
  labs(x = "Stumpage (First Differences) (Quarter Lags)", y = "Stumpage (First Differences) (Quarter Lags)") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_gradientn(
    colors = c("purple", "white", "orange"),
    limits = c(-1, 1),
    breaks = c(-1, 0, 1)
  ) +
  theme(legend.ticks = element_blank())

#    Kiln, Douglas Fir

#     Level

cor_auto_kiln_level =
  dat_variables %>% 
  filter(Series == "Price_Lumber_DouglasFir_Kiln_RL" & Measure == "Level") %>% 
  select(starts_with("Lag")) %>% 
  cor(use = "na.or.complete") %>% 
  as_tibble %>% 
  mutate(Lag_X = paste0("Lag_", seq(0, 40))) %>% 
  relocate(Lag_X) %>% 
  pivot_longer(-Lag_X, names_to = "Lag_Y", values_to = "Correlation") %>% 
  mutate(across(starts_with("Lag"), ~ as.numeric(str_remove(.x, "Lag_")))) %>% 
  ggplot() +
  geom_tile(aes(x = Lag_X, y = Lag_Y, fill = Correlation)) +
  labs(x = "Kiln (Quarter Lags)", y = "Kiln (Quarter Lags)") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_gradientn(
    colors = c("purple", "white", "orange"),
    limits = c(-1, 1),
    breaks = c(-1, 0, 1)
  ) +
  theme(legend.ticks = element_blank())

#     First Differences

cor_auto_kiln_fd =
  dat_variables %>% 
  filter(Series == "Price_Lumber_DouglasFir_Kiln_RL" & Measure == "FD") %>% 
  select(starts_with("Lag")) %>% 
  cor(use = "na.or.complete") %>% 
  as_tibble %>% 
  mutate(Lag_X = paste0("Lag_", seq(0, 40))) %>% 
  relocate(Lag_X) %>% 
  pivot_longer(-Lag_X, names_to = "Lag_Y", values_to = "Correlation") %>% 
  mutate(across(starts_with("Lag"), ~ as.numeric(str_remove(.x, "Lag_")))) %>% 
  ggplot() +
  geom_tile(aes(x = Lag_X, y = Lag_Y, fill = Correlation)) +
  labs(x = "Kiln (First Differences) (Quarter Lags)", y = "Kiln (First Differences) (Quarter Lags)") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_gradientn(
    colors = c("purple", "white", "orange"),
    limits = c(-1, 1),
    breaks = c(-1, 0, 1)
  ) +
  theme(legend.ticks = element_blank())

#   X-X

#    Stumpage (Douglas Fir, Western Hemlock)

#     Level

cor_xx_stumpage_level =
  dat_variables %>% 
  filter(Series %in% c("Price_Stumpage_DouglasFir", "Price_Stumpage_WesternHemlock")) %>% 
  filter(Measure == "Level") %>% 
  mutate(Series = Series %>% str_remove("Price_Stumpage_")) %>% 
  select(-starts_with("Mean")) %>% 
  pivot_wider(names_from = Series, values_from = starts_with("Lag")) %>% 
  select(-Year_Quarter, -Measure) %>% 
  {
    x <- select(., ends_with("DouglasFir"))
    y <- select(., ends_with("WesternHemlock"))
    cor(x, y, use = "na.or.complete")
  } %>% 
  as_tibble %>% 
  mutate(Lag_DouglasFir = paste0("Lag_", seq(0, 40), "_DouglasFir")) %>% 
  relocate(Lag_DouglasFir) %>% 
  pivot_longer(
    -Lag_DouglasFir, 
    names_to = "Lag_WesternHemlock", 
    values_to = "Correlation"
  ) %>% 
  mutate(
    across(starts_with("Lag"), 
           ~ .x %>% 
             str_remove("Lag_") %>% 
             str_remove("_DouglasFir") %>% 
             str_remove("_WesternHemlock") %>% 
             as.numeric
    )
  ) %>% 
  ggplot() +
  geom_tile(aes(x = Lag_DouglasFir, y = Lag_WesternHemlock, fill = Correlation)) +
  labs(
    x = "Douglas Fir Stumpage (Quarter Lags)", 
    y = "Western Hemlock Stumpage (Quarter Lags)"
  ) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_gradientn(
    colors = c("purple", "white", "orange"),
    limits = c(-1, 1),
    breaks = c(-1, 0, 1)
  ) +
  theme(legend.ticks = element_blank())

#    Kiln (Douglas Fir, Western Hemlock)

#     Level

cor_xx_kiln_level =
  dat_variables %>% 
  filter(Series %in% c("Price_Lumber_DouglasFir_Kiln_RL", "Price_Lumber_HemFir_Kiln_RL")) %>% 
  filter(Measure == "Level") %>% 
  mutate(Series = Series %>% str_remove("Price_Lumber_") %>% str_remove("_Kiln_RL")) %>% 
  select(-starts_with("Mean")) %>% 
  pivot_wider(names_from = Series, values_from = starts_with("Lag")) %>% 
  select(-Year_Quarter, -Measure) %>% 
  {
    x <- select(., ends_with("DouglasFir"))
    y <- select(., ends_with("HemFir"))
    cor(x, y, use = "na.or.complete")
  } %>% 
  as_tibble %>% 
  mutate(Lag_DouglasFir = paste0("Lag_", seq(0, 40), "_DouglasFir")) %>% 
  relocate(Lag_DouglasFir) %>% 
  pivot_longer(
    -Lag_DouglasFir, 
    names_to = "Lag_HemFir", 
    values_to = "Correlation"
  ) %>% 
  mutate(
    across(starts_with("Lag"), 
           ~ .x %>% 
             str_remove("Lag_") %>% 
             str_remove("_DouglasFir") %>% 
             str_remove("_HemFir") %>% 
             as.numeric
    )
  ) %>% 
  ggplot() +
  geom_tile(aes(x = Lag_DouglasFir, y = Lag_HemFir, fill = Correlation)) +
  labs(
    x = "Douglas Fir Kiln (Quarter Lags)", 
    y = "Western Hemlock Kiln (Quarter Lags)"
  ) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_gradientn(
    colors = c("purple", "white", "orange"),
    limits = c(-1, 1),
    breaks = c(-1, 0, 1)
  ) +
  theme(legend.ticks = element_blank())

#    Stumpage and Kiln (Douglas Fir)

#     Level

cor_xx_prices_level =
  dat_variables %>% 
  filter(Series %in% c("Price_Stumpage_DouglasFir", "Price_Lumber_DouglasFir_Kiln_RL")) %>% 
  filter(Measure == "Level") %>% 
  mutate(
    Series = 
      Series %>% 
      str_remove("Price_") %>% 
      str_remove("_DouglasFir") %>% 
      str_remove("_Kiln_RL")) %>% 
  select(-starts_with("Mean")) %>% 
  pivot_wider(names_from = Series, values_from = starts_with("Lag")) %>% 
  select(-Year_Quarter, -Measure) %>% 
  {
    x <- select(., ends_with("Stumpage"))
    y <- select(., ends_with("Lumber"))
    cor(x, y, use = "na.or.complete")
  } %>% 
  as_tibble %>% 
  mutate(Lag_Stumpage = paste0("Lag_", seq(0, 40), "_Stumpage")) %>% 
  relocate(Lag_Stumpage) %>% 
  pivot_longer(
    -Lag_Stumpage, 
    names_to = "Lag_Lumber", 
    values_to = "Correlation"
  ) %>% 
  mutate(
    across(starts_with("Lag"), 
           ~ .x %>% 
             str_remove("Lag_") %>% 
             str_remove("_Stumpage") %>% 
             str_remove("_Lumber") %>% 
             as.numeric
    )
  ) %>% 
  ggplot() +
  geom_tile(aes(x = Lag_Stumpage, y = Lag_Lumber, fill = Correlation)) +
  labs(
    x = "Douglas Fir Stumpage (Quarter Lags)",
    y = "Douglas Fir Kiln (Quarter Lags)"
  ) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_gradientn(
    colors = c("purple", "white", "orange"),
    limits = c(-1, 1),
    breaks = c(-1, 0, 1)
  ) +
  theme(legend.ticks = element_blank())

#    Rate and Price

#     Level

cor_xx_rate_interest_level =
  dat_variables %>% 
  filter(Series %in% c("Price_Stumpage_DouglasFir", "Rate")) %>% 
  filter(Measure == "Level") %>% 
  mutate(
    Series = 
      Series %>% 
      str_remove("Price_") %>% 
      str_remove("_DouglasFir")
  ) %>% 
  select(-starts_with("Mean")) %>% 
  pivot_wider(names_from = Series, values_from = starts_with("Lag")) %>% 
  select(-Year_Quarter, -Measure) %>% 
  {
    x <- select(., ends_with("Stumpage"))
    y <- select(., ends_with("Rate"))
    cor(x, y, use = "na.or.complete")
  } %>% 
  as_tibble %>% 
  mutate(Lag_Stumpage = paste0("Lag_", seq(0, 40), "_Stumpage")) %>% 
  relocate(Lag_Stumpage) %>% 
  pivot_longer(
    -Lag_Stumpage, 
    names_to = "Lag_Rate", 
    values_to = "Correlation"
  ) %>% 
  mutate(
    across(starts_with("Lag"), 
           ~ .x %>% 
             str_remove("Lag_") %>% 
             str_remove("_Stumpage") %>% 
             str_remove("_Rate") %>% 
             as.numeric
    )
  ) %>% 
  ggplot() +
  geom_tile(aes(x = Lag_Stumpage, y = Lag_Rate, fill = Correlation)) +
  labs(
    x = "Douglas Fir Stumpage (Quarter Lags)",
    y = "Federal Funds Rate (Quarter Lags)"
  ) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_gradientn(
    colors = c("purple", "white", "orange"),
    limits = c(-1, 1),
    breaks = c(-1, 0, 1)
  ) +
  theme(legend.ticks = element_blank())

#   VPD, CWD

#    Level

cor_xx_vpd_cwd_level =
  dat_variables %>% 
  filter(Series %in% c("VPD", "CWD")) %>% 
  filter(Measure == "Level") %>% 
  select(-starts_with("Mean")) %>% 
  pivot_wider(names_from = Series, values_from = starts_with("Lag")) %>% 
  select(-Year_Quarter, -Measure) %>% 
  {
    x <- select(., ends_with("VPD"))
    y <- select(., ends_with("CWD"))
    cor(x, y, use = "na.or.complete")
  } %>% 
  as_tibble %>% 
  mutate(Lag_VPD = paste0("Lag_", seq(0, 40), "_VPD")) %>% 
  relocate(Lag_VPD) %>% 
  pivot_longer(
    -Lag_VPD, 
    names_to = "Lag_CWD", 
    values_to = "Correlation"
  ) %>% 
  mutate(
    across(starts_with("Lag"), 
           ~ .x %>% 
             str_remove("Lag_") %>% 
             str_remove("_VPD") %>% 
             str_remove("_CWD") %>% 
             as.numeric
    )
  ) %>% 
  ggplot() +
  geom_tile(aes(x = Lag_VPD, y = Lag_CWD, fill = Correlation)) +
  labs(
    x = "VPD (Quarter Lags)",
    y = "CWD (Quarter Lags)"
  ) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_gradientn(
    colors = c("purple", "white", "orange"),
    limits = c(-1, 1),
    breaks = c(-1, 0, 1)
  ) +
  theme(legend.ticks = element_blank())

# CWD and Fire

cor_xx_cwd_fire_level =
  dat_variables %>% 
  filter(Series %in% c("CWD", "Fire_30")) %>% 
  filter(Measure == "Level") %>% 
  select(-starts_with("Mean")) %>% 
  pivot_wider(names_from = Series, values_from = starts_with("Lag")) %>% 
  select(-Year_Quarter, -Measure) %>% 
  {
    x <- select(., ends_with("CWD"))
    y <- select(., ends_with("Fire_30"))
    cor(x, y, use = "na.or.complete")
  } %>% 
  as_tibble %>% 
  mutate(Lag_CWD = paste0("Lag_", seq(0, 40), "_CWD")) %>% 
  relocate(Lag_CWD) %>% 
  pivot_longer(
    -Lag_CWD, 
    names_to = "Lag_Fire_30", 
    values_to = "Correlation"
  ) %>% 
  mutate(
    across(starts_with("Lag"), 
           ~ .x %>% 
             str_remove("Lag_") %>% 
             str_remove("_CWD") %>% 
             str_remove("_Fire_30") %>% 
             as.numeric
    )
  ) %>% 
  ggplot() +
  geom_tile(aes(x = Lag_CWD, y = Lag_Fire_30, fill = Correlation)) +
  labs(
    x = "CWD (Quarter Lags)",
    y = "Fires (30km) (Quarter Lags)"
  ) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_gradientn(
    colors = c("purple", "white", "orange"),
    limits = c(-1, 1),
    breaks = c(-1, 0, 1)
  ) +
  theme(legend.ticks = element_blank())

#   X-Y 

#    This is pretty vile as-is. 

cor_xy =
  dat_variables %>% 
  filter(Year_Quarter > "2014_Q4") %>% 
  filter(Year_Quarter < "2025_Q1") %>% 
  filter(
    Series %in% 
      c(
        "Price_Stumpage_DouglasFir", 
        "Price_Lumber_DouglasFir_Kiln_RL",
        "Rate",
        "Fire_30",
        "VPD",
        "CWD",
        "Supply_DouglasFir"
        )
  ) %>% 
  # filter((Series == "Supply_DouglasFir" & Measure == "Level") | Series != "Supply_DouglasFir") %>% 
  select(-starts_with("Mean")) %>% 
  mutate(
    Series = 
      Series %>% 
      str_remove("Price_") %>% 
      str_remove("_DouglasFir") %>% 
      str_remove("_Kiln_RL") %>% 
      str_remove("_30")
  ) %>% 
  mutate(Measure = Measure %>% str_remove_all("_")) %>% 
  pivot_wider(names_from = c(Measure, Series), values_from = starts_with("Lag")) %>% 
  pivot_longer(ends_with("_Supply"), names_to = "Supply_Lag", values_to = "Supply") %>% 
  mutate(Supply_Measure = Supply_Lag %>% str_split_i("_", 3)) %>% 
  relocate(Supply_Lag, Supply_Measure, Supply) %>% 
  filter((Supply_Lag  %>% str_sub(1, 6)) == "Lag_0_") %>% 
  drop_na(Supply) %>% 
  select(-Supply_Lag) %>% 
  pivot_wider(names_from = Supply_Measure, values_from = Supply) %>% 
  relocate("Level", "LevelSD", "FD", "FDSD") %>% 
  rename(
    Level_Supply = Level,
    LevelSD_Supply = LevelSD,
    FD_Supply = FD,
    FDSD_Supply = FDSD
  ) %>% 
  select(-Year_Quarter) %>% 
  {
    x <- select(., ends_with("Supply"))
    y <- select(., -ends_with("Supply"))
    cor(x, y, use = "na.or.complete")
  } %>% 
  as.data.frame %>% 
  rownames_to_column("Measure_Supply") %>% 
  as_tibble %>% 
  pivot_longer(
    -Measure_Supply, 
    names_to = "Lag_Measure_Series", 
    values_to = "Correlation"
  ) %>% 
  mutate(
    Lag = Lag_Measure_Series %>% str_split_i("_", 2) %>% as.numeric,
    Measure_Covariate = Lag_Measure_Series %>% str_split_i("_", 3),
    Series = Lag_Measure_Series %>% str_split_i("_", 4)
  ) %>% 
  select(-Lag_Measure_Series) %>% 
  mutate(
    Measure_Supply = 
      case_when(
        Measure_Supply == "Level_Supply" ~ "Level",
        Measure_Supply == "LevelSD_Supply" ~ "Level (Season-Adjusted)",
        Measure_Supply == "FD_Supply" ~ "First Differences",
        Measure_Supply == "FDSD_Supply" ~ "First Differences (Season-Adjusted)"
      ) %>% 
      factor %>% 
      fct_relevel(
        "Level", 
        "Level (Season-Adjusted)",
        "First Differences",
        "First Differences (Season-Adjusted)"
      ),
    Measure_Covariate = 
      case_when(
        Measure_Covariate == "Level" ~ "Level",
        Measure_Covariate == "LevelSD" ~ "Level (Season-Adjusted)",
        Measure_Covariate == "FD" ~ "First Differences",
        Measure_Covariate == "FDSD" ~ "First Differences (Season-Adjusted)"
      ) %>% 
      factor %>% 
      fct_relevel(
        "Level", 
        "Level (Season-Adjusted)",
        "First Differences",
        "First Differences (Season-Adjusted)"
      ),
    Series = 
      Series %>% 
      factor %>% 
      fct_relevel(
        "Stumpage",
        "Lumber",
        "Rate",
        "Fire",
        "VPD",
        "CWD"
      ) %>% 
      fct_rev
  ) %>% 
  ggplot() +
  geom_tile(aes(x = Lag, y = Series, fill = Correlation)) +
  facet_grid(Measure_Supply ~ Measure_Covariate) +
  labs(
    x = "Quarter Lags",
    y = "Covariate"
  ) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  scale_fill_gradientn(
    colors = c("purple", "white", "orange"),
    limits = c(-1, 1),
    breaks = c(-1, 0, 1)
  ) +
  theme(legend.ticks = element_blank())

# Exports

#  Line Plots

ggsave(
  "04_out/Correlations/vis_supply.png",
  vis_supply,
  dpi = 300,
  width = 8, 
  height = 8
)

ggsave(
  "04_out/Correlations/vis_price_douglasfir.png",
  vis_price_douglasfir,
  dpi = 300,
  width = 8, 
  height = 8
)

ggsave(
  "04_out/Correlations/vis_price_westernhemlock.png",
  vis_price_westernhemlock,
  dpi = 300,
  width = 8, 
  height = 8
)

ggsave(
  "04_out/Correlations/vis_rate.png",
  vis_rate,
  dpi = 300,
  width = 8, 
  height = 8
)

ggsave(
  "04_out/Correlations/vis_fire.png",
  vis_fire,
  dpi = 300,
  width = 8, 
  height = 8
)

ggsave(
  "04_out/Correlations/vis_vpd.png",
  vis_vpd,
  dpi = 300,
  width = 8, 
  height = 8
)

ggsave(
  "04_out/Correlations/vis_cwd.png",
  vis_cwd,
  dpi = 300,
  width = 8, 
  height = 8
)

#  Autocorrelation Plots

ggsave(
  "04_out/Correlations/vis_cor_auto_supply_level.png",
  cor_auto_supply_level,
  dpi = 300,
  width = 5, 
  height = 4
)

ggsave(
  "04_out/Correlations/vis_cor_auto_supply_level_sd.png",
  cor_auto_supply_level_sd,
  dpi = 300,
  width = 5, 
  height = 4
)

ggsave(
  "04_out/Correlations/vis_cor_auto_stumpage_level.png",
  cor_auto_stumpage_level,
  dpi = 300,
  width = 5, 
  height = 4
)

ggsave(
  "04_out/Correlations/vis_cor_auto_kiln_level.png",
  cor_auto_kiln_level,
  dpi = 300,
  width = 5, 
  height = 4
)

#  Cross-Correlation Plots

ggsave(
  "04_out/Correlations/vis_cor_xx_stumpage_level.png",
  cor_xx_stumpage_level,
  dpi = 300,
  width = 5, 
  height = 4
)

ggsave(
  "04_out/Correlations/vis_cor_xx_kiln_level.png",
  cor_xx_kiln_level,
  dpi = 300,
  width = 5, 
  height = 4
)

ggsave(
  "04_out/Correlations/vis_cor_xx_prices_level.png",
  cor_xx_prices_level,
  dpi = 300,
  width = 5, 
  height = 4
)

ggsave(
  "04_out/Correlations/vis_cor_xx_cwd_fire_level.png",
  cor_xx_cwd_fire_level,
  dpi = 300,
  width = 5, 
  height = 4
)

ggsave(
  "04_out/Correlations/vis_cor_xx_vpd_cwd_level.png",
  cor_xx_vpd_cwd_level,
  dpi = 300,
  width = 5, 
  height = 4
)

#  Cross-Correlations (Supply ~ Covariates)

ggsave(
  "04_out/Correlations/vis_cor_xy.png",
  cor_xy,
  dpi = 300,
  width = 9, 
  height = 7
)
