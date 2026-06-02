# Try out descriptive plots and tables.

# Dimensions

width = 7.5
height = 7.5 / 1.618

# Data

dat = 
  "03_intermediate/dat_notifications_1_9.gdb" %>% 
  vect

# Firms, Time (Year)

vis_firms_year = 
  dat %>% 
  as_tibble %>% 
  group_by(YearCompletion) %>% 
  summarize(Firms = n_distinct(Landowner)) %>% 
  ungroup %>% 
  ggplot() +
  geom_col(aes(x = YearCompletion %>% factor,
               y = Firms),
           width = 0.75) +
  labs(x = "Year (Estimated Completion)") +
  theme_pubr()

ggsave("04_out/Smorgasbord/vis_firms_year.png",
       vis_firms_year,
       dpi = 300,
       width = width,
       height = height)

# Firms, Time (Year-Quarter)

vis_firms_yearquarter = 
  dat %>% 
  as_tibble %>% 
  group_by(QuarterCompletion, Quarter) %>% 
  summarize(Firms = n_distinct(Landowner)) %>% 
  ungroup %>% 
  ggplot() +
  geom_col(aes(x = QuarterCompletion %>% factor,
               y = Firms),
           width = 0.75) +
  labs(x = "Year-Quarter (Estimated Completion)") +
  theme_pubr() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.50, hjust = 1))

ggsave("04_out/Smorgasbord/vis_firms_yearquarter.png",
       vis_firms_yearquarter,
       dpi = 300,
       width = width,
       height = height)

# Counts by Mode

vis_counts_mode = 
  dat %>% 
  as_tibble %>% 
  group_by(YearCompletion, Activity) %>% 
  summarize(Count = n()) %>% 
  ungroup %>% 
  ggplot() +
  geom_col(aes(x = YearCompletion %>% factor,
               y = Count,
               fill = Activity),
           width = 0.75) +
  labs(x = "Year (Estimated Completion)") +
  theme_pubr() +
  theme(legend.title = element_blank())

ggsave("04_out/Smorgasbord/vis_counts_mode.png",
       vis_counts_mode,
       dpi = 300,
       width = width,
       height = height)

# Acres by Mode

vis_acres_mode = 
  dat %>% 
  as_tibble %>% 
  group_by(YearCompletion, Activity) %>% 
  summarize(Acres = Acres_1 %>% sum) %>% 
  ungroup %>% 
  ggplot() +
  geom_col(aes(x = YearCompletion %>% factor,
               y = Acres,
               fill = Activity),
           width = 0.75) +
  labs(x = "Year (Estimated Completion)") +
  theme_pubr() +
  theme(legend.title = element_blank())

ggsave("04_out/Smorgasbord/vis_acres_mode.png",
       vis_acres_mode,
       dpi = 300,
       width = width,
       height = height)

# MBF by Mode  

vis_mbf_mode = 
  dat %>% 
  as_tibble %>% 
  mutate(MBF_Both = MBF_2_DouglasFir + MBF_2_WesternHemlock) %>% 
  group_by(YearCompletion, Activity) %>% 
  summarize(MBF_Both = MBF_Both %>% sum) %>% 
  ungroup %>% 
  ggplot() +
  geom_col(aes(x = YearCompletion %>% factor,
               y = MBF_Both,
               fill = Activity),
           width = 0.75) +
  labs(x = "Year (Estimated Completion)") +
  theme_pubr() +
  theme(legend.title = element_blank())

ggsave("04_out/Smorgasbord/vis_mbf_mode.png",
       vis_mbf_mode,
       dpi = 300,
       width = width,
       height = height)

# Modes over Space

dat_bounds = "03_intermediate/dat_bounds.gdb" %>% vect

vis_space_mode = 
  dat %>% 
  select(Activity) %>% 
  centroids %>% 
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

ggsave("04_out/Smorgasbord/vis_space_mode.png",
       vis_space_mode,
       dpi = 300,
       width = width,
       height = height)

# Site Class (Mode)

vis_class_space = 
  dat %>% 
  select(SiteClassMode) %>% 
  mutate(SiteClassMode = SiteClassMode %>% factor) %>% 
  centroids %>% 
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

ggsave("04_out/Smorgasbord/vis_class_space.png",
       vis_class_space,
       dpi = 300,
       width = width,
       height = height)
