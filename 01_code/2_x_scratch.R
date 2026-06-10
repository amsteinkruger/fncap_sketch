# Try out descriptive plots and tables.

#  TOC

#   Dimensions
#   Data
#   Bounds
#   Firms, Time (Year)
#   Firms, Time (Year-Quarter)
#   Counts over Time by Mode
#   Acres over Time by Mode
#   MBF over Time by Mode 
#   Modes over Space
#   Site Class over Space by Mode

#   Species over Time
#   Species over Space
#   Prices, Interest over Time
#   Fire over Time
#   Fire over Space

# Dimensions

width = 7.5
height = 7.5 / 1.618

# Data

dat = "03_intermediate/dat_notifications_1_9.gdb" %>% vect

# Bounds

dat_bounds = "03_intermediate/dat_bounds.gdb" %>% vect

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

#   Species over Time

vis_species_time = 
  dat %>% 
  as_tibble %>% 
  select(YearCompletion,
         ProportionDouglasFirTree,
         ProportionWesternHemlockTree) %>% 
  pivot_longer(starts_with("Proportion"),
               names_to = "Species",
               names_prefix = "Proportion",
               values_to = "Proportion") %>% 
  mutate(Species = ifelse(Species == "DouglasFirTree", "Douglas fir", "Western hemlock")) %>% 
  ggplot() +
  geom_boxplot(aes(x = YearCompletion %>% factor,
                   y = Proportion,
                   color = Species),
               shape = 21,
               fill = NA) +
  scale_x_discrete(breaks = c(2015, 2018, 2021, 2024)) +
  labs(x = "Year (Estimated Completion)") +
  facet_wrap(~ Species) +
  theme_pubr() +
  theme(legend.position = "none")

ggsave("04_out/Smorgasbord/vis_species_time.png",
       vis_species_time,
       dpi = 300,
       width = width,
       height = height)

#   Species over Space

dat_species_space = 
  dat %>% 
  select(ProportionDouglasFirTree, ProportionWesternHemlockTree) %>% 
  pivot_longer(everything(),
               names_to = "Species",
               names_prefix = "Proportion",
               values_to = "Proportion") %>% 
  mutate(Species = ifelse(Species == "DouglasFirTree", "Proportion Douglas fir", "Proportion Western hemlock")) %>% 
  centroids 

vis_species_space = 
  ggplot() +
  geom_spatvector(data = dat_bounds) +
  geom_spatvector(data = dat_species_space,
                  aes(color = Proportion),
                  shape = 21,
                  fill = NA,
                  size = 0.50,
                  alpha = 0.50) + 
  facet_wrap(~ Species) +
  scale_color_viridis(option = "F",
                      limits = c(0, 1.00),
                      breaks = c(0, 0.50, 1.00)) +
  theme_void() +
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        legend.key.height = unit(0.50, "lines"),
        legend.key.width = unit(5.00, "lines"),
        legend.title = element_blank(),
        legend.ticks = element_blank())

ggsave("04_out/Smorgasbord/vis_species_space.png",
       vis_species_space,
       dpi = 300,
       width = width,
       height = height)

#   Prices, Interest over Time

vis_prices = 
  dat %>% 
  as_tibble %>% 
  select(QuarterCompletion, Stumpage, Lumber, Rate) %>% 
  pivot_longer(c(Stumpage, Lumber, Rate),
               names_to = "Which",
               values_to = "Value") %>% 
  mutate(Facet = 
           ifelse(Which == "Rate", "Effective Federal Funds Rate", "Price (USD2024Q4)") %>% 
           factor %>% 
           fct_relevel("Price (USD2024Q4)", "Effective Federal Funds Rate"),
         Which = 
           Which %>% 
           factor %>% 
           fct_relevel("Stumpage", "Lumber", "Rate")) %>% 
  arrange(QuarterCompletion) %>% 
  mutate(QuarterCompletion = QuarterCompletion %>% factor) %>% 
  ggplot() + 
  geom_line(aes(x = QuarterCompletion,
                y = Value,
                color = Which,
                group = Which)) +
  scale_x_discrete(breaks = c("2015_1", "2018_1", "2021_1", "2024_1")) +
  facet_wrap(~ Facet,
             scales = "free_y") +
  theme_pubr() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.50),
        axis.title = element_blank(),
        legend.title = element_blank())

ggsave("04_out/Smorgasbord/vis_prices.png",
       vis_prices,
       dpi = 300,
       width = width,
       height = height)

#   Fire over Time
#   Fire over Space
