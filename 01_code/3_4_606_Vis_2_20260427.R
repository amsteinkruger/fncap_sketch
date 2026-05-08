# Visualize something for 2YP.

# Data

dat = 
  "03_intermediate/dat_notifications_1_8.csv" %>% 
  read_csv %>% 
  mutate(Year = QuarterCompletion %>% str_sub(1, 4)) %>% 
  mutate(Restrict_MBF_Lower = MBF_2_DouglasFir > quantile(MBF_2_DouglasFir, 0.01),
         Restrict_MBF_Upper = MBF_2_DouglasFir < quantile(MBF_2_DouglasFir, 0.99),
         Restrict_Acres_Lower = Acres_1 > quantile(Acres_1, 0.01),
         Restrict_Acres_Upper = Acres_1 < quantile(Acres_1, 0.99),
         Restrict_MBFAcre_Lower = MBF_Acre_2_DouglasFir > quantile(MBF_Acre_2_DouglasFir, 0.01),
         Restrict_MBFAcre_Upper = MBF_Acre_2_DouglasFir < quantile(MBF_Acre_2_DouglasFir, 0.99)) %>% 
  filter(if_all(starts_with("Restrict"), ~ .x == TRUE)) %>% 
  select(Year, MBF_2_DouglasFir, MBF_Acre_2_DouglasFir, Landowner) %>% 
  group_by(Year, Landowner) %>% 
  summarize(MBF_Log = sum(MBF_2_DouglasFir) %>% log,
            MBF_Acre_Log = mean(MBF_Acre_2_DouglasFir) %>% log) %>% 
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
