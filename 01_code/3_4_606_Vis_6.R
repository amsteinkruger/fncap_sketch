# Visualize something for 2YP.

#  Clear the environment.

rm(list = ls())

# Data

dat = 
  "03_intermediate/dat_notifications_1_8.csv" %>% 
  read_csv %>% 
  mutate(Restrict_MBF_Lower = MBF_2_DouglasFir > quantile(MBF_2_DouglasFir, 0.01),
         Restrict_MBF_Upper = MBF_2_DouglasFir < quantile(MBF_2_DouglasFir, 0.99),
         Restrict_Acres_Lower = Acres_1 > quantile(Acres_1, 0.01),
         Restrict_Acres_Upper = Acres_1 < quantile(Acres_1, 0.99),
         Restrict_MBFAcre_Lower = MBF_Acre_2_DouglasFir > quantile(MBF_Acre_2_DouglasFir, 0.01),
         Restrict_MBFAcre_Upper = MBF_Acre_2_DouglasFir < quantile(MBF_Acre_2_DouglasFir, 0.99)) %>% 
  filter(if_all(starts_with("Restrict"), ~ .x == TRUE)) %>% 
  group_by(Landowner, QuarterCompletion) %>% 
  summarize(MBF_Firm = MBF_2_DouglasFir %>% sum(na.rm = TRUE),
            Acres = Acres_1 %>% sum(na.rm = TRUE),
            across(starts_with("Lumber"), 
                   ~ weighted.mean(.x, na.rm = TRUE, w = MBF_2_DouglasFir)),
            across(starts_with("Stumpage"), 
                   ~ weighted.mean(.x, na.rm = TRUE, w = MBF_2_DouglasFir)),
            across(starts_with("Fire"), 
                   ~ weighted.mean(.x, na.rm = TRUE, w = MBF_2_DouglasFir)),
            across(starts_with("VPD"), 
                   ~ weighted.mean(.x, na.rm = TRUE, w = MBF_2_DouglasFir)),
            across(starts_with("Rate"), 
                   ~ weighted.mean(.x, na.rm = TRUE, w = MBF_2_DouglasFir))) %>% 
  ungroup %>% 
  mutate(Quarter = QuarterCompletion %>% str_split_i("_", 2) %>% as.numeric) %>% 
  rename(MBF = MBF_Firm)

# Production and Prices

vis_stumpage = 
  dat %>% 
  mutate(MBF_Log = MBF %>% log,
         Stumpage_Log = Stumpage_Lag_1 %>% log) %>% 
  ggplot() + 
  geom_point(aes(x = Stumpage_Log,
                 y = MBF_Log),
             shape = 21,
             fill = NA,
             alpha = 0.50) +
  labs(x = "Log(Price) (Stumpage, 2024 USD)", y = "Log(MBF)") +
  theme_minimal()

vis_lumber = 
  dat %>% 
  mutate(MBF_Log = MBF %>% log,
         Lumber_Log = Lumber_Lag_1 %>% log) %>% 
  ggplot() + 
  geom_point(aes(x = Lumber_Log,
                 y = MBF_Log),
             shape = 21,
             fill = NA,
             alpha = 0.50) +
  labs(x = "Log(Price) (Lumber, 2024 USD)", y = "Log(MBF)") +
  theme_minimal()

vis = vis_stumpage + vis_lumber

ggsave("04_out/vis_6.png",
       vis,
       dpi = 300,
       height = 3.25,
       width = 6.5)

# Production and Wildfire

vis_fire_15 = 
  dat %>% 
  mutate(MBF_Log = MBF %>% log) %>% 
  ggplot() + 
  geom_point(aes(x = Fire_15_Doughnut,
                 y = MBF_Log),
             shape = 21,
             fill = NA,
             alpha = 0.50) +
  labs(x = "Wildfires, (0, 15] km", y = "Log(MBF)") +
  theme_minimal()

vis_fire_30 = 
  dat %>% 
  mutate(MBF_Log = MBF %>% log) %>% 
  ggplot() + 
  geom_point(aes(x = Fire_30_Doughnut,
                 y = MBF_Log),
             shape = 21,
             fill = NA,
             alpha = 0.50) +
  labs(x = "Wildfires, (15, 30] km", y = "Log(MBF)") +
  theme_minimal()

vis = vis_fire_15 + vis_fire_30

ggsave("04_out/vis_7.png",
       vis,
       dpi = 300,
       height = 3.25,
       width = 6.5)

