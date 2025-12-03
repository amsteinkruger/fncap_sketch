# Add other geodata to processed notification data.

# Note choice to drop spatial information from each "join" SpatVector after operations.

# (!!!) Note that some "join" SpatVectors have incorrect names from numeric indexing in rename().

# TOC:
#  Packages
#  Bounds
#  Notifications
#  (FIA Clone?)
#  Elevation
#  Slope
#  (PRISM?)
#  Pyromes
#  Fires
#  (EVT)
#  TreeMap (Species, Site Class, . . .)
#  TCC
#  Distances (Mills, Cities, Roads, Rivers/Riparian Zones)
#  Ownership
#  Prices
#  Join

# time_start = Sys.time()

# Packages

library(tidyverse)
library(terra)
library(tidyterra)
library(readxl)

# Ratio

phi = (1 + 5 ^ (1 / 2)) / 2

# Bounds

#  OR

dat_bounds_or = 
  "data/cb_2023_us_state_500k" %>% 
  vect %>% 
  filter(STUSPS == "OR") %>% 
  project("EPSG:2992")

#  Pyromes

dat_bounds_pyromes = 
  "data/USFS Pyromes/Data/Pyromes_CONUS_20200206.shp" %>% 
  vect %>% 
  rename(WHICH = NAME) %>% # Band-Aid for a reserved attribute name.
  filter(WHICH %in% c("Marine Northwest Coast Forest", "Klamath Mountains", "Middle Cascades")) %>% 
  select(Pyrome = WHICH) %>% 
  summarize(Pyrome = "All Pyromes") %>% # This is not great.
  fillHoles %>% 
  project("EPSG:2992")

#  Intersection

dat_bounds = 
  intersect(dat_bounds_or, dat_bounds_pyromes) %>% 
  # Handle island polygons. These are not real islands.
  disagg %>% 
  cbind(., expanse(., unit = "ha")) %>% 
  filter(y == max(y)) %>% 
  select(-y)

# Notifications

dat_notifications = 
  "data/dat_notifications_polygons.gdb" %>% # Note that the folder is wrong.
  vect %>% 
  filter(ActivityType == "Clearcut/Overstory Removal") %>% 
  filter(ActivityUnit == "MBF") %>% 
  filter(LandOwnerType == "Partnership/Corporate Forestland Ownership") %>% 
  mutate(Year = year(DateStart_Left),
         Month = month(DateStart_Left),
         YearMonth = paste0(Year, ifelse(str_length(Month) < 2, "0", ""), Month),
         MBF = ActivityQuantity %>% as.numeric) %>%
  arrange(desc(Year), desc(Month), LandOwnerType, LandOwnerName_Right, desc(MBF), desc(Acres)) %>% 
  select(Landowner = LandOwnerName_Right,
         Year, 
         Month, 
         YearMonth, 
         MBF, 
         Acres) %>% 
  project("EPSG:2992") %>% 
  # Check whether polygons are valid.
  cbind(., is.valid(.)) %>% 
  rename(Valid_0 = y) %>% 
  # Try fixing any invalid polygons.
  makeValid %>% 
  # Check again.
  cbind(., is.valid(.)) %>% 
  rename(Valid_1 = y) %>%
  # Drop columns. This would be a nice spot to return counts of valid, invalid, and fixed polygons as a side effect.
  select(-starts_with("Valid")) %>% 
  # Subset for testing.
  # slice_sample(n = 1000) %>%
  # Crop.
  crop(dat_bounds) %>% 
  # Swap unique ID assignment to this step for convenience.
  mutate(UID = row_number())

dat_notifications_mask = 
  dat_notifications %>% 
  summarize(ID = "Combined")

# Visualize region.

vis_quick_1 = 
  ggplot() + 
  geom_spatvector(data = dat_bounds_or, fill = NA) +
  theme_void()

ggsave("output/vis_quick_1.png",
       vis_quick_1,
       dpi = 300,
       width = 8)

vis_quick_2 = 
  ggplot() + 
  geom_spatvector(data = dat_bounds_or, fill = NA) +
  geom_spatvector(data = dat_bounds, color = NA, fill = "#4A773C") +
  theme_void()

ggsave("output/vis_quick_2.png",
       vis_quick_2,
       dpi = 300,
       width = 8)

dat_notifications_less = dat_notifications %>% filter(Year %in% 2015:2024) %>% mutate(Year = Year %>% factor)

library(ggpubr)
library(RColorBrewer)

pal = colorRampPalette(brewer.pal('Greys', n = 9))(10)

vis_quick_3 = 
  ggplot() + 
  geom_spatvector(data = dat_bounds_or, fill = NA) +
  geom_spatvector(data = dat_bounds, color = NA, fill = "#4A773C") +
  geom_spatvector(data = dat_notifications_less, color = NA, aes(fill = Year)) +
  scale_fill_manual(values = pal, guide = guide_legend(reverse = TRUE)) +
  theme_void() +
  theme(legend.position = "right",
        legend.direction = "vertical",
        legend.key.width = unit(0.5, "lines"),
        legend.key.height = unit(2, "lines"),
        legend.title = element_blank())

ggsave("output/vis_quick_3.png",
       vis_quick_3,
       dpi = 300,
       width = 9)

# Wildfire!

dat_mtbs = 
  "data/mtbs_perimeter_data" %>% 
  vect %>% 
  filter(substr(Event_ID, 1, 2) == "OR") %>% 
  mutate(Year = year(Ig_Date)) %>% 
  filter(Year %in% 2015:2024) %>% 
  mutate(Year = Year %>% factor) %>% 
  project("EPSG:2992") %>% 
  crop(dat_bounds)

pal = colorRampPalette(brewer.pal('Reds', n = 9))(10)

vis_quick_4 = 
  ggplot() + 
  geom_spatvector(data = dat_bounds_or, fill = NA) +
  geom_spatvector(data = dat_bounds, color = NA, fill = "#4A773C") +
  geom_spatvector(data = dat_mtbs, color = NA, aes(fill = Year)) +
  scale_fill_manual(values = pal, guide = guide_legend(reverse = TRUE)) +
  theme_void() +
  theme(legend.position = "right",
        legend.direction = "vertical",
        legend.key.width = unit(0.5, "lines"),
        legend.key.height = unit(2, "lines"),
        legend.title = element_blank())

ggsave("output/vis_quick_4.png",
       vis_quick_4,
       dpi = 300,
       width = 7.25)
