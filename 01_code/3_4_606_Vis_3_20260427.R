# Visualize something for 2YP.

# Data

dat = 
  "03_intermediate/dat_notifications_1_6.csv" %>% 
  read_csv %>% 
  mutate(Year = DateStart %>% year,
         Month = DateStart %>% month,
         Quarter = Month %>% multiply_by(1 / 3) %>% ceiling,
         Year_Quarter = paste0(Year, "_Q", Quarter)) %>% 
  filter(DateStart %>% year > 2014 & DateEnd %>% year < 2025) %>% 
  select(Year_Quarter, Stumpage_Real, Rate_Fed) %>% 
  group_by(Year_Quarter) %>% 
  summarize(Price = Stumpage_Real %>% mean,
            Rate = Rate_Fed %>% mean) %>% 
  ungroup

# Time Series

vis_price = 
  dat %>% 
  ggplot() + 
  geom_line(aes(x = Year_Quarter,
                y = Price,
                group = 1)) +
  labs(x = NULL, y = "Price (Stumpage, 2024 Q4 USD)") +
  scale_x_discrete(breaks = paste0(2015:2024, "_Q1")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

vis_rate = 
  dat %>% 
  ggplot() + 
  geom_line(aes(x = Year_Quarter,
                y = Rate,
                group = 1)) +
  labs(x = NULL, y = "Effective Federal Funds Rate") +
  scale_x_discrete(breaks = paste0(2015:2024, "_Q1")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Combine/Export

vis = vis_price + vis_rate

ggsave("04_out/vis_3_20260428.png",
       vis,
       dpi = 300,
       height = 3.25,
       width = 6.5)
