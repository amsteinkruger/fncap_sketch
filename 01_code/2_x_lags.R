# Visualize time series of raw and distributed values for time-varying covariates.

#  Try deltas as well as level values. 
#  Try doing something about seasonality. 
#  Note Almon polynomial weights on distributed lags as . . . a thing. 
# probably throw in a big all-all correlation matrix

# raw | 1-y mean | 3-y mean | 5-y mean | 10-y mean

# correlations within raw X | correlations with lags (corr matrices I guess?)

# correlations between Y, X

#  Supply | Douglas fir

#  Supply | Western hemlock

#  Prices | Douglas fir

vis_price_douglasfir = 
  left_join(
    "03_intermediate/data_stumpage.csv" %>% read_csv,
    "03_intermediate/data_lumber.csv" %>% read_csv
  ) %>% 
  pivot_longer(
    -Year_Quarter,
    names_to = "Series",
    values_to = "Value") %>% 
  filter(str_detect(Series, "DouglasFir")) %>% 
  mutate(Series = Series %>% str_remove("Price_") %>% str_remove("_DouglasFir")) %>% 
  # This is where distributed lags come in, with some lag-to-facet-variable manipulation. 
  ggplot() +
  geom_line(aes(x = Year_Quarter, y = Value, color = Series, group = Series))

  
#  Prices | Western hemlock

#  Prices | Composite

#  Climate | CWD

#  Climate | Precipitation?

#  Climate | Fire (30km)



# Reference code, I guess?

# Reference Plot

vis_price_test = 
  dat_price_stumpage %>% 
  left_join(dat_price_lumber) %>% 
  pivot_longer(-Year_Quarter) %>% 
  mutate(DouglasFir = ifelse(str_detect(name, "DouglasFir"), "Douglas Fir", "Western hemlock")) %>% 
  ggplot() +
  geom_line(aes(x = Year_Quarter,
                y = value,
                group = name,
                color = name)) +
  facet_wrap(~ DouglasFir)

# vis_price_test = 
#   dat_price_test %>% 
#   filter(Commodity %in% c("Logs", "Lumber/Sawn Timber")) %>% 
#   ggplot() + 
#   geom_vline(xintercept = "2008",
#              color = "red",
#              linetype = "dashed") +
#   geom_vline(xintercept = "2020", 
#              color = "red",
#              linetype = "dashed") +
#   geom_boxplot(aes(x = Year %>% factor,
#                    y = Price %>% log,
#                    color = Commodity),
#                alpha = 0.75) +
#   scale_x_discrete(breaks = c("2000", "2010", "2020")) +
#   scale_color_manual(values = c("gray40", "gray20")) +
#   labs(x = "Year",
#        y = "Price (Nominal) (Log.)") +
#   facet_wrap(~ Commodity) +
#   theme_minimal() +
#   theme(legend.position = "none")

# ggsave("04_out/vis_price_20260401.png",
#        vis_price_test,
#        dpi = 300,
#        width = 6,
#        height = 4)

#  Prices

vis_prices =
  # "03_intermediate/dat_notifications_1_9.csv" %>% 
  # read_csv %>% 
  dat_use_really %>% 
  select(QuarterCompletion, contains("Composite") & !contains("Green")) %>% # Keeping things simple to start. 
  select(QuarterCompletion, ends_with(c("Lag_1", "Mean_1Y", "Mean_5Y", "Mean_10Y"))) %>% 
  pivot_longer(-QuarterCompletion) %>%
  arrange(QuarterCompletion) %>% 
  group_by(QuarterCompletion, name) %>% 
  summarize(value = mean(value, na.rm = TRUE)) %>% 
  ungroup %>% 
  mutate(name = name %>% str_remove_all("Price_Composite_")) %>% 
  ggplot(aes(x = QuarterCompletion %>% factor,
             y = value,
             color = name,
             group = name)) +
  geom_line()

#  Interest

vis_interest = 
  dat_use_really %>% 
  select(QuarterCompletion,
         contains("Rate") & ends_with(c("Lag_1", "Mean_1Y", "Mean_5Y", "Mean_10Y"))) %>% 
  pivot_longer(-QuarterCompletion) %>%
  arrange(QuarterCompletion) %>% 
  group_by(QuarterCompletion, name) %>% 
  summarize(value = mean(value, na.rm = TRUE)) %>% 
  ungroup %>% 
  mutate(name = name %>% str_remove_all("Rate")) %>% 
  ggplot(aes(x = QuarterCompletion %>% factor,
             y = value,
             color = name,
             group = name)) +
  geom_line()

#  Climate and Weather



