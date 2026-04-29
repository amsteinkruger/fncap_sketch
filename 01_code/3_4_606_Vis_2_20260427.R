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
  filter(ProportionDouglasFir > 0.50) %>% 
  filter(MBF > quantile(MBF, 0.01) & MBF < quantile(MBF, 0.99) &
           Acres > quantile(Acres, 0.01) & Acres < quantile(Acres, 0.99) &
           MBF_Acre > quantile(MBF_Acre, 0.01) & MBF_Acre < quantile(MBF_Acre, 0.99)) %>% 
  select(Year, MBF, MBF_Acre, Landowner) %>% 
  group_by(Year, Landowner) %>% 
  summarize(MBF_Log = sum(MBF) %>% log,
            MBF_Acre_Log = mean(MBF_Acre) %>% log) %>% 
  ungroup # %>% 
  # add_row(Year_Quarter = "2016_Q2",
  #         Landowner = NA,
  #         MBF_Log = NA,
  #         MBF_Acre_Log = NA)

# Time Series

pal_time_production = RColorBrewer::brewer.pal(9, "Greens")[7]
pal_time_yield = RColorBrewer::brewer.pal(9, "Blues")[7]
pal_time_companies = RColorBrewer::brewer.pal(9, "Oranges")[7]

#  Production

vis_time_production = 
  dat %>% 
  ggplot() +
  geom_boxplot(aes(x = Year %>% factor,
                   y = MBF_Log),
               outlier.shape = 21,
               color = pal_time_production,
               fill = "white") +
  labs(x = NULL, y = "Log(MBF)") +
  # scale_x_discrete(breaks = paste0(2015:2024, "_Q1")) +
  # scale_y_continuous(expand = c(0, 0)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#  Yield

vis_time_yield = 
  dat %>% 
  ggplot() +
  geom_boxplot(aes(x = Year %>% factor,
                   y = MBF_Acre_Log),
               outlier.shape = 21,
               color = pal_time_yield,
               fill = "white") + 
  labs(x = NULL, y = "Log(MBF/Acre)") +
  # scale_x_discrete(breaks = paste0(2015:2024, "_Q1")) +
  # scale_y_continuous(expand = c(0, 0)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#  Firms

vis_time_companies = 
  dat %>% 
  group_by(Year) %>% 
  summarize(Companies = n_distinct(Landowner)) %>% 
  ungroup %>% 
  ggplot() +
  geom_col(aes(x = Year %>% factor,
               y = Companies),
           fill = pal_time_companies,
           color = NA) +
  labs(x = NULL, y = "Landowners with Clearcuts") +
  scale_x_discrete(# breaks = paste0(2015:2024, "_Q1"),
                   expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Combine/Export

vis_time = vis_time_production + vis_time_yield + vis_time_companies

ggsave("04_out/vis_2_20260428.png",
       vis_time,
       dpi = 300,
       height = 2.75,
       width = 6.5)
