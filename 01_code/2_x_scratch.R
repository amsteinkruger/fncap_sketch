# Try out descriptive plots and tables.

# Packages

library(ggpubr)

# Data

dat = 
  "03_intermediate/dat_notifications_1_6.gdb" %>% 
  vect

# Firms

dat_firms = 
  dat %>% 
  as_tibble %>% 
  mutate(Year = DateSubmit %>% year) %>% 
  group_by(Year) %>% 
  summarize(Firms = n_distinct(Landowner)) %>% 
  ungroup

vis_firms = 
  dat_firms %>% 
  ggplot() +
  geom_col(aes(x = Year %>% factor,
               y = Firms),
           width = 0.75) +
  labs(x = "Year (Notification Submission)") +
  theme_pubr()

ggsave("04_out/Out_20260527/vis_firms.png",
       vis_firms,
       dpi = 300,
       width = 6,
       height = 4)

# Counts by Mode

dat_counts_mode = 
  dat %>% 
  as_tibble %>% 
  mutate(Year = DateSubmit %>% year) %>% 
  select(Year, Activity) %>% 
  group_by(Year, Activity) %>% 
  summarize(Count = n()) %>% 
  ungroup 

vis_counts_mode = 
  dat_counts_mode %>% 
  ggplot() +
  geom_col(aes(x = Year %>% factor,
               y = Count,
               fill = Activity),
           width = 0.75) +
  labs(x = "Year (Notification Submission)") +
  theme_pubr() +
  theme(legend.title = element_blank())

ggsave("04_out/Out_20260527/vis_counts_mode.png",
       vis_counts_mode,
       dpi = 300,
       width = 6,
       height = 4)

# Acres by Mode

dat_acres_mode = 
  dat %>% 
  as_tibble %>% 
  mutate(Year = DateSubmit %>% year) %>% 
  select(Year, Acres_1, Activity) %>% 
  group_by(Year, Activity) %>% 
  summarize(Acres = Acres_1 %>% sum) %>% 
  ungroup 

vis_acres_mode = 
  dat_acres_mode %>% 
  ggplot() +
  geom_col(aes(x = Year %>% factor,
               y = Acres,
               fill = Activity),
           width = 0.75) +
  labs(x = "Year (Notification Submission)") +
  theme_pubr() +
  theme(legend.title = element_blank())

ggsave("04_out/Out_20260527/vis_acres_mode.png",
       vis_acres_mode,
       dpi = 300,
       width = 6,
       height = 4)

# MBF by Mode  

dat_mbf_mode = 
  dat %>% 
  as_tibble %>% 
  mutate(Year = DateSubmit %>% year,
         MBF = MBF_2_DouglasFir + MBF_2_WesternHemlock) %>% 
  select(Year, MBF, Activity) %>% 
  group_by(Year, Activity) %>% 
  summarize(MBF = MBF %>% sum) %>% 
  ungroup 

vis_mbf_mode = 
  dat_mbf_mode %>% 
  ggplot() +
  geom_col(aes(x = Year %>% factor,
               y = MBF,
               fill = Activity),
           width = 0.75) +
  labs(x = "Year (Notification Submission)") +
  theme_pubr() +
  theme(legend.title = element_blank())

ggsave("04_out/Out_20260527/vis_mbf_mode.png",
       vis_mbf_mode,
       dpi = 300,
       width = 6,
       height = 4)

# Modes over Space

dat_bounds = "03_intermediate/dat_bounds.gdb" %>% vect

dat_space_mode = 
  dat %>% 
  select(Activity) %>% 
  centroids

vis_space_mode = 
  ggplot() +
  geom_spatvector(data = dat_bounds) +
  geom_spatvector(data = dat_space_mode,
                  aes(color = Activity),
                  shape = 21,
                  fill = NA,
                  size = 0.50,
                  alpha = 0.50) + 
  facet_wrap(~ Activity,
             nrow = 1) +
  theme_void() +
  theme(legend.position = "none")

ggsave("04_out/Out_20260527/vis_space_mode.png",
       vis_space_mode,
       dpi = 300,
       width = 6,
       height = 4)

# Site Class (Mode)

dat_class_space = 
  dat %>% 
  select(SiteClassMode) %>% 
  mutate(SiteClassMode = SiteClassMode %>% factor) %>% 
  centroids

vis_class_space = 
  ggplot() +
  geom_spatvector(data = dat_bounds) +
  geom_spatvector(data = dat_class_space,
                  aes(color = SiteClassMode),
                  shape = 21,
                  fill = NA,
                  size = 0.50,
                  alpha = 0.50) + 
  facet_wrap(~ SiteClassMode, nrow = 1) +
  theme_void() +
  theme(legend.position = "none")

ggsave("04_out/Out_20260527/vis_class_space.png",
       vis_class_space,
       dpi = 300,
       width = 6,
       height = 4)
