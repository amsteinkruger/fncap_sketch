# Count notifications by county and pyrome. 

library(tidyverse)
library(terra)
library(tidyterra)
library(viridis)
library(patchwork)

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

dat_counties_more = 
  "03_intermediate/dat_notifications_1_6.gdb" %>% 
  vect %>% 
  as_tibble %>% 
  select(County, MBF_1) %>% 
  group_by(County) %>% 
  summarize(MBF_County = sum(MBF_1) / 10,
            Count_County = n() / 10) %>% 
  ungroup %>% 
  left_join(dat_counties, .) %>% 
  drop_na

dat_pyromes_more = 
  "03_intermediate/dat_notifications_1_6.gdb" %>% 
  vect %>% 
  as_tibble %>% 
  select(Pyrome, MBF_1) %>% 
  group_by(Pyrome) %>% 
  summarize(MBF_Pyrome = sum(MBF_1) / 10,
            Count_Pyrome = n() / 10) %>% 
  ungroup %>% 
  left_join(dat_pyrome, .)

# Visualization

#  Counties

vis_counties_count = 
  dat_counties_more %>% 
  mutate(County_Less = str_split_i(County, " ", 1),
         Count_County_Less = Count_County %>% round(0),
         Label = paste0(County_Less, ", ", Count_County_Less)) %>% 
  filter(County != "Klamath County") %>% 
  ggplot() + 
  geom_spatvector(aes(fill = Count_County_Less)) +
  geom_spatvector_text(aes(label = Label)) +
  labs(title = "Mean Notification Counts") + 
  theme_void() +
  theme(legend.position = "none")

vis_counties_mbf = 
  dat_counties_more %>% 
  mutate(County_Less = str_split_i(County, " ", 1),
         MBF_County_Less = (MBF_County / 1000) %>% round(0),
         Label = paste0(County_Less, ", ", MBF_County_Less)) %>% 
  filter(County != "Klamath County") %>% 
  ggplot() + 
  geom_spatvector(aes(fill = MBF_County_Less)) +
  geom_spatvector_text(aes(label = Label)) +
  labs(title = "Mean MMBF") + 
  theme_void() +
  theme(legend.position = "none")

vis_counties = vis_counties_count + vis_counties_mbf

ggsave("04_out/vis_counties_20260506.png",
       vis_counties,
       dpi = 300,
       width = 11,
       height = 8.5)


#  Pyromes

vis_pyromes_count = 
  dat_pyromes_more %>% 
  mutate(Count_Pyrome_Less = Count_Pyrome %>% round(0),
         Label = paste0(Pyrome, ", ", Count_Pyrome_Less)) %>% 
  ggplot() + 
  geom_spatvector(aes(fill = Count_Pyrome_Less)) +
  geom_spatvector_text(aes(label = Label)) +
  labs(title = "Mean Notification Counts") + 
  theme_void() +
  theme(legend.position = "none")

vis_pyromes_mbf = 
  dat_pyromes_more %>% 
  mutate(MBF_Pyrome_Less = (MBF_Pyrome / 1000) %>% round(0),
         Label = paste0(Pyrome, ", ", MBF_Pyrome_Less)) %>% 
  ggplot() + 
  geom_spatvector(aes(fill = MBF_Pyrome_Less)) +
  geom_spatvector_text(aes(label = Label)) +
  labs(title = "Mean MMBF") + 
  theme_void() +
  theme(legend.position = "none")

vis_pyromes = vis_pyromes_count + vis_pyromes_mbf

ggsave("04_out/vis_pyromes_20260506.png",
       vis_pyromes,
       dpi = 300,
       width = 11,
       height = 8.5)
