# Visualize lags and means of lags for time-varying covariates.

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



