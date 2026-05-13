# Count notifications by proportional species assignments.  

# Data

dat_counties = 
  "02_data/1_6_6_TIGER/TIGER.gdb" %>% 
  vect(layer = "County") %>% 
  select(County = NAMELSAD) %>% 
  project("EPSG:2992")

dat_bounds = 
  "03_intermediate/dat_bounds.gdb" %>% 
  vect

dat_pyrome = 
  "02_data/1_2_2_USFS_Pyromes/Data/Pyromes_CONUS_20200206.shp" %>% 
  vect %>% 
  rename(WHICH = NAME) %>% # Band-Aid for a reserved attribute name.
  filter(WHICH %in% c("Marine Northwest Coast Forest", "Klamath Mountains", "Middle Cascades")) %>% 
  select(Pyrome = WHICH) %>% 
  project("EPSG:2992") %>% 
  crop(dat_bounds)

dat_notifications = 
  "03_intermediate/dat_notifications_1_6.csv" %>% 
  read_csv

# Tabulate descriptive statistics on proportional species presence.

dat_tab = 
  dat_notifications %>% 
  mutate(ZerosDouglasFirTree = ifelse(ProportionDouglasFirTree == 0, 1, 0),
         ZerosWesternHemlockTree = ifelse(ProportionWesternHemlockTree == 0, 1, 0)) %>% 
  summarize(ProportionDouglasFirTree_Mean = ProportionDouglasFirTree %>% mean,
            ProportionWesternHemlockTree_Mean = ProportionWesternHemlockTree %>% mean,
            ProportionDouglasFirTree_SD = ProportionDouglasFirTree %>% sd,
            ProportionWesternHemlockTree_SD = ProportionWesternHemlockTree %>% sd,
            ZerosDouglasFirTree_Mean = ZerosDouglasFirTree %>% mean,
            ZerosWesternHemlockTree_Mean = ZerosWesternHemlockTree %>% mean) %>% 
  pivot_longer(everything()) %>% 
  mutate(Statistic = str_split_i(name, "_", 2),
         Measure = str_split_i(name, "_", 1)) %>% 
  select(-name) %>% 
  mutate(value = value %>% round(3)) %>% 
  pivot_wider(names_from = Statistic,
              values_from = value) %T>% 
  write_csv("04_out/tab_species.csv")

# Visualize proportional species presence.

vis_spp = 
  dat_notifications %>% 
  select(ProportionDouglasFirTree,
         ProportionWesternHemlockTree) %>% 
  pivot_longer(everything()) %>% 
  mutate(name = ifelse(name == "ProportionDouglasFirTree", "Douglas Fir", "Western Hemlock")) %>% 
  ggplot() + 
  geom_histogram(aes(x = value,
                     fill = name),
                 color = "black",
                 alpha = 0.50) +
  labs(x = "Proportion of Sawlog Timber in Species",
       y = "Count of Notifications") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        legend.title = element_blank())

ggsave("04_out/vis_species.png",
       vis_spp,
       dpi = 300,
       width = 4.25,
       height = 4.00)
