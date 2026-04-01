# Wrangle prices. 

# Timber PPI

dat_ppi = 
  "02_data/BLS/data_ppi_timber.csv" %>% 
  read_csv %>% 
  mutate(Year = observation_date %>% year,
         Month = observation_date %>% month,
         Factor_PPI_Timber_2024 = max(WPU085) / WPU085) %>% 
  select(Year, Month, Factor_PPI_Timber_2024)

dat_ppi_timber = 
  "02_data/BLS/data_annual_WPU08510502.csv" %>% 
  read_csv %>% 
  rename(Date = 1, PPI_Timber = 2) %>% 
  mutate(Year = Date %>% year,
         PPI_Timber = ifelse(is.na(PPI_Timber), lag(PPI_Timber), PPI_Timber),
         Factor_PPI_Timber = max(PPI_Timber) / PPI_Timber) %>% 
  select(Year, PPI_Timber, Factor_PPI_Timber)

dat_ppi_lumber = 
  "02_data/BLS/data_annual_WPU0811.csv" %>% 
  read_csv %>% 
  rename(Date = 1, PPI_Lumber = 2) %>% 
  mutate(Year = Date %>% year,
         PPI_Lumber = ifelse(is.na(PPI_Lumber), lag(PPI_Lumber), PPI_Lumber),
         Factor_PPI_Lumber = max(PPI_Lumber) / PPI_Lumber) %>% 
  select(Year, PPI_Lumber, Factor_PPI_Lumber)

# Prices

#  Stumpage, LogLines/FastMarkets

dat_price_stumpage = 
  "02_data/Prices_FastMarkets/data_stumpage.csv" %>% 
  read_csv %>% 
  rename(Stumpage_Nominal = 2) %>% 
  mutate(Year = Quarter %>% str_sub(1, 4) %>% as.numeric) %>% 
  group_by(Year) %>% 
  summarize(Stumpage_Nominal = Stumpage_Nominal %>% mean(na.rm = TRUE)) %>% 
  ungroup %>% 
  left_join(dat_ppi_timber) %>% 
  mutate(Stumpage_Real = Stumpage_Nominal * Factor_PPI_Timber) %>% 
  filter(Year %in% 2014:2026) %>% 
  select(Year, starts_with("Stumpage"))

#   Delivered Logs, FastMarkets

dat_price_delivered = 
  "02_data/Prices_FastMarkets/data_pull_filter.csv" %>% 
  read_csv %>% 
  select(1, 3) %>% 
  rename(Delivered_Nominal = 2) %>% 
  group_by(Year) %>% 
  summarize(Delivered_Nominal = Delivered_Nominal %>% mean(na.rm = TRUE)) %>% 
  ungroup %>% 
  left_join(dat_ppi_timber) %>% 
  mutate(Delivered_Real = Delivered_Nominal * Factor_PPI_Timber) %>% 
  filter(Year %in% 2014:2026) %>% 
  select(Year, starts_with("Delivered"))

#   Lumber, FastMarkets

dat_price_test = 
  "02_data/Prices_FastMarkets/data_pull.csv" %>% 
  read_csv %>% 
  select(-c(`Start Date`, `Fame Name`)) %>% 
  mutate(across(!1:13, ~ as.numeric(.x))) %>% 
  pivot_longer(cols = !1:13,
               names_to = "Month-Year",
               values_to = "Price") %>% 
  drop_na(Price) %>% 
  mutate(Year_Stub = `Month-Year` %>% str_sub(-2, -1),
         Year = 
           ifelse(Year_Stub < 50, 
                  paste0("20", Year_Stub), 
                  paste0("19", Year_Stub))) %>% 
  group_by(across(c(1:13, Year))) %>% 
  summarize(Price = Price %>% mean(na.rm = TRUE)) %>% 
  ungroup

#   Forecast, Forisk

#   Quick Plot

vis_price_test = 
  dat_price_test %>% 
  filter(Commodity %in% c("Logs", "Lumber/Sawn Timber")) %>% 
  ggplot() + 
  geom_vline(xintercept = "2008",
             color = "red",
             linetype = "dashed") +
  geom_vline(xintercept = "2020", 
             color = "red",
             linetype = "dashed") +
  geom_boxplot(aes(x = Year %>% factor,
                   y = Price %>% log,
                   color = Commodity),
               alpha = 0.75) +
  scale_x_discrete(breaks = c("2000", "2010", "2020")) +
  scale_color_manual(values = c("gray40", "gray20")) +
  labs(x = "Year",
       y = "Price (Nominal) (Log.)") +
  facet_wrap(~ Commodity) +
  theme_minimal() +
  theme(legend.position = "none")

ggsave("04_out/vis_price_20260401.png",
       vis_price_test,
       dpi = 300,
       width = 6,
       height = 4)
