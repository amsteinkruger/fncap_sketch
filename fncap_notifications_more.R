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
#  Pyromes
#  Fires
#  Class
#  EVT
#  TCC
#  Soil (LCC)
#  Distances (Mills, Cities, Roads)
#  Ownership
#  Prices
#  Join

# time_start = Sys.time()

# Packages

library(tidyverse)
library(terra)
library(tidyterra)
library(readxl)

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
  "output/dat_notifications_polygons.gdb" %>% 
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

# Try filtering on counts of vertices.

# dat_vertices = 
#   dat_notifications %>% 
#   geom %>% 
#   as_tibble %>% 
#   group_by(geom) %>% 
#   summarize(count = n()) %>% 
#   arrange(count) %>% 
#   mutate(rank = percent_rank(count)) %>% 
#   filter(rank < 0.5)

# Use the subset of spatial objects with fewer vertices for a filtering join on notifications.

# dat_notifications =
#   dat_notifications %>%
#   semi_join(dat_vertices, by = c("UID" = "geom"))

dat_notifications_mask = 
  dat_notifications %>% 
  summarize(ID = "Combined")

# Elevation (Data)

# Remember to swap in alternative OR DEM.

dat_elevation = "data/OR_DEM_10M.gdb.zip" %>% rast

# Slope (Data/Processing)

if (file.exists("output/dat_slope.tif")) {
  
  dat_slope = "output/dat_slope.tif" %>% rast
  
} else {
  
  dat_slope = dat_elevation %>% terrain(v = "slope")
  writeRaster(dat_slope, "output/dat_slope.tif", overwrite = TRUE)
  
}

# Elevation (Processing)

dat_join_elevation =
  dat_notifications %>%
  extract(x = dat_elevation,
          y = .,
          fun = mean,
          ID = FALSE,
          bind = TRUE) %>%
  rename(Elevation = 2) %>%
  as_tibble

# Slope (Processing)

dat_join_slope =
  dat_notifications %>%
  extract(x = dat_slope,
          y = .,
          fun = mean,
          ID = FALSE,
          bind = TRUE,
          na.rm = TRUE) %>%
  rename(Slope = 2) %>%
  as_tibble

#  Note warning about units and projection in terra documentation.

# Pyromes

dat_pyrome = 
  "data/USFS Pyromes/Data/Pyromes_CONUS_20200206.shp" %>% 
  vect %>% 
  rename(WHICH = NAME) %>% # Band-Aid for a reserved attribute name.
  filter(WHICH %in% c("Marine Northwest Coast Forest", "Klamath Mountains", "Middle Cascades")) %>% 
  select(Pyrome = WHICH) %>% 
  project("EPSG:2992")

dat_join_pyrome = 
  dat_notifications %>% 
  intersect(dat_pyrome) %>% 
  as_tibble %>% 
  select(UID, Pyrome)

# Note that this does not account for notifications falling across pyromes.

# MTBS

# Consider reorganizing for (1) better memory management around buffers (2) better memory management around which MTBS perimeters are worth intersecting

dat_mtbs = 
  "data/mtbs_perimeter_data" %>% 
  vect %>% 
  filter(substr(Event_ID, 1, 2) == "OR") %>% 
  project("EPSG:2992")

# ggplot() +
#   geom_spatvector(data = dat_mtbs, fill = "red", color = NA, alpha = 0.50) + 
#   geom_spatvector(data = dat_notifications, fill = "blue", color = NA, alpha = 0.50) +
#   theme_void()
# 
# ggsave("figures/vis_mtbs.png", dpi = 300, height = 8.5, width = 11)

# 1. No Buffer

dat_join_mtbs_1 = 
  dat_notifications %>% 
  intersect(dat_mtbs) %>% 
  as_tibble %>% 
  mutate(Year_MTBS = Ig_Date %>% year) %>% 
  filter(Year >= Year_MTBS & Year - 20 < Year_MTBS) %>% 
  group_by(UID) %>% 
  summarize(MTBS_1 = n()) %>% 
  ungroup

# 2. 15km Buffer, Difference
#  Note switch back to subset of notifications.

dat_join_mtbs_2 = 
  dat_notifications %>% 
  buffer(width = 15 * 3280.84) %>% # Oregon GIC Lambert is in feet, so convert kilometer buffer width into feet.
  erase(dat_notifications) %>% 
  intersect(dat_mtbs) %>% 
  as_tibble %>% 
  mutate(Year_MTBS = Ig_Date %>% year) %>% 
  filter(Year >= Year_MTBS & Year - 20 < Year_MTBS) %>% 
  group_by(UID) %>% 
  summarize(MTBS_2 = n()) %>% 
  ungroup

# 3. 30km Buffer, Difference

dat_join_mtbs_3 = 
  dat_notifications %>% 
  buffer(width = 30 * 3280.84) %>% 
  erase(dat_notifications %>% buffer(width = 15 * 3280.84)) %>% 
  intersect(dat_mtbs) %>% 
  as_tibble %>% 
  mutate(Year_MTBS = Ig_Date %>% year) %>% 
  filter(Year >= Year_MTBS & Year - 20 < Year_MTBS) %>% 
  group_by(UID) %>% 
  summarize(MTBS_3 = n()) %>% 
  ungroup

# 4. 15km Buffer, Union

dat_join_mtbs_4 = 
  dat_notifications %>% 
  buffer(width = 15 * 3280.84) %>% 
  intersect(dat_mtbs) %>% 
  as_tibble %>% 
  mutate(Year_MTBS = Ig_Date %>% year) %>% 
  filter(Year >= Year_MTBS & Year - 20 < Year_MTBS) %>% 
  group_by(UID) %>% 
  summarize(MTBS_4 = n()) %>% 
  ungroup

# 5. 30km Buffer, Union

dat_join_mtbs_5 = 
  dat_notifications %>% 
  buffer(width = 30 * 3280.84) %>% 
  intersect(dat_mtbs) %>% 
  as_tibble %>% 
  mutate(Year_MTBS = Ig_Date %>% year) %>% 
  filter(Year >= Year_MTBS & Year - 20 < Year_MTBS) %>% 
  group_by(UID) %>% 
  summarize(MTBS_5 = n()) %>% 
  ungroup

# 6. Combine

dat_join_mtbs = 
  dat_join_mtbs_1 %>% 
  full_join(dat_join_mtbs_2, by = "UID") %>% 
  full_join(dat_join_mtbs_3, by = "UID") %>% 
  full_join(dat_join_mtbs_4, by = "UID") %>% 
  full_join(dat_join_mtbs_5, by = "UID") %>%
  select(UID,
         Fire_0 = MTBS_1,
         Fire_15_Difference = MTBS_2,
         Fire_30_Difference = MTBS_3,
         Fire_15_Union = MTBS_4,
         Fire_30_Union = MTBS_5)

# Site Class (Latta)

# Get FIA. Sketch interpolation.

# Get temperature.
# Get precipitation.
# Get CMI?

# EVT

dat_evt_2016 = 
  "data/LF2016_EVT_200_CONUS/Tif/LC16_EVT_200.tif" %>% 
  rast %>% 
  crop(dat_bounds %>% project("EPSG:5070"), mask = TRUE) %>% # Reprojecting to account for relative object sizes.
  project("EPSG:2992") %>% 
  crop(dat_notifications_mask, mask = TRUE) %>% 
  droplevels

dat_evt_2016_cats = 
  dat_evt_2016 %>% 
  cats %>% 
  magrittr::extract2(1) %>% 
  as_tibble

# Repeat for 2024.

dat_evt_2024 = 
  "data/LF2024_EVT_250_CONUS/Tif/LC24_EVT_250.tif" %>% 
  rast %>% 
  crop(dat_bounds %>% project("EPSG:5070"), mask = TRUE) %>% # Reprojecting to account for relative object sizes.
  project("EPSG:2992") %>% 
  crop(dat_notifications_mask, mask = TRUE) %>% 
  droplevels

dat_evt_2024_cats = 
  dat_evt_2024 %>% 
  cats %>% 
  magrittr::extract2(1) %>% 
  as_tibble

# Export categories.

dat_evt_cats = 
  bind_rows(dat_evt_2016_cats %>% select(Value, EVT_NAME, EVT_GP, EVT_GP_N, EVT_ORDER, EVT_CLASS), 
            dat_evt_2024_cats %>% select(Value, EVT_NAME, EVT_GP, EVT_GP_N, EVT_ORDER, EVT_CLASS)) %>% 
  distinct %>% 
  write_csv("output/dat_evt_cats.csv")

# Process categories.

vec_evt_in_ceiling = 
  dat_evt_cats %>% 
  filter(EVT_ORDER == "Tree-dominated") %>% 
  pull(Value)

vec_evt_in_hand =
  read_csv("output/dat_evt_cats_hand.csv") %>%
  filter(Keep == 1) %>% # This is a choice.
  pull(Value)

# Pick an option.

vec_evt_in = vec_evt_in_hand

# Filter rasters.

dat_evt_2016_binary <- dat_evt_2016 %in% vec_evt_in

dat_evt_2016_binary %>% plot

dat_evt_2024_binary <- dat_evt_2024 %in% vec_evt_in

dat_evt_2024_binary %>% plot

dat_evt_binary <- dat_evt_2016_binary * dat_evt_2024_binary

# Extract means of binary results onto notifications.

dat_join_evt =
  dat_notifications %>%
  extract(x = dat_evt_binary,
          y = .,
          fun = mean,
          ID = FALSE,
          bind = TRUE) %>%
  rename(EVT = EVT_NAME) %>%
  as_tibble

# TCC
#  Eats 62 GB for 2014-2023 as of 2025/10/29.

crs_tcc = 
  "data/TCC_Science_2014/science_tcc_conus_wgs84_v2023-5_20140101_20141231.tif" %>% # Replace w/ 2024.
  rast %>% 
  crs

dat_bounds_tcc = dat_bounds %>% project(crs_tcc)

dat_tcc = 
  list.files("data") %>% 
  tibble(file = .) %>% 
  filter(file %>% str_sub(1, 7) == "TCC_Sci") %>% 
  mutate(year = file %>% str_sub(-4, -1) %>% as.numeric) %>% 
  # filter(year %in% 2020:2022) %>% # Band-Aid.
  rename(folder = file) %>% # Fiddling around.
  mutate(file = paste0("data/", folder, "/science_tcc_conus_wgs84_v2023-5_", year, "0101_", year, "1231.tif")) %>% # Fragile!
  mutate(data_0 = 
           file %>% 
           map(rast) %>% 
           map(as.numeric) %>% 
           map(crop,
               dat_bounds_tcc,
               mask = TRUE) %>% 
           map(project,
               "EPSG:2992"),
         data_1 = data_0 %>% lag) %>% 
  filter(year > min(year)) %>% # Avoid a frustrating problem with NULL.
  mutate(data_difference = map2(data_0,
                                data_1,
                                ~ .x - .y)) %>% # ifelse(is.null(.y), "This is a null!", ~ .x - .y)
  select(year, starts_with("data"))

dat_join_tcc = 
  dat_notifications %>% 
  pull(Year) %>% 
  unique %>% 
  sort %>% 
  tibble(year = .) %>% 
  mutate(notifications = 
           year %>% 
           map(~ filter(dat_notifications, Year == .x))) %>% 
  inner_join(dat_tcc) %>% 
  mutate(notifications_0 = 
           map2(data_0,
                notifications,
                extract,
                fun = mean,
                ID = FALSE,
                bind = TRUE) %>% 
           map(rename,
               TCC_0 = category) %>% 
           map(as_tibble) %>% 
           map(select, UID, TCC_0),
         notifications_1 = 
           map2(data_1,
                notifications,
                extract,
                fun = mean,
                ID = FALSE,
                bind = TRUE) %>% 
           map(rename,
               TCC_1 = category) %>% 
           map(as_tibble) %>% 
           map(select, UID, TCC_1),
         notifications_d = 
           map2(data_difference,
                notifications,
                extract,
                fun = mean,
                ID = FALSE,
                bind = TRUE) %>% 
           map(rename,
               TCC_D = category) %>% 
           map(as_tibble) %>% 
           map(select, UID, TCC_D)) %>% 
  select(year, starts_with("notifications_")) %>% 
  mutate(notifications = map2(notifications_0, notifications_1, full_join),
         notifications = map2(notifications, notifications_d, full_join)) %>% 
  select(year, notifications) %>% 
  unnest(notifications)

# Ad hoc check

dat_join_tcc %>% 
  mutate(TCC_Bin = ifelse(TCC_D < 0, 1, 0)) %>% 
  ggplot() +
  geom_jitter(aes(x = year %>% factor,
                  y = TCC_D,
                  color = TCC_Bin %>% factor(labels = c("+", "-"))),
              alpha = 0.10) +
  labs(x = NULL, y = "Difference in TCC",
       color = "Sign") +
  theme_minimal()

ggsave("output/vis_counts_20251029.png",
       dpi = 300,
       width = 5,
       height = 5)

dat_join_tcc %>% 
  mutate(TCC_Bin = ifelse(TCC_D < 0, 1, 0)) %>% 
  group_by(year) %>% 
  summarize(TCC_Proportion = sum(TCC_Bin) / n())

# The following snippet is pretty bad.

dat_notifications %>% 
  as_tibble %>% 
  group_by(Year) %>% 
  summarize(count = n()) %>% 
  rename(year = Year) %>% 
  left_join(dat_join_tcc %>% 
              mutate(TCC_Bin = ifelse(TCC_D < 0, 1, 0)) %>% 
              group_by(year) %>% 
              summarize(TCC_Proportion = sum(TCC_Bin) / n())) %>% 
  filter(!is.na(TCC_Proportion)) %>% 
  mutate(Proportion_2 = 1 - TCC_Proportion) %>% 
  mutate(Count_2 = count * (1 - TCC_Proportion)) %>% 
  rename(Year = year,
         Count_All = count,
         Proportion_Negative = TCC_Proportion,
         Proportion_Positive = Proportion_2,
         Count_Positive = Count_2) %>% 
  write_csv("output/data_counts_20251029.csv")

# LCC (Soil)

dat_lcc = 
  "data/gSSURGO_OR.gdb" %>% 
  rast

# Problem: what's the tidiest approach to pulling in look-up tables without ESRI?

# Distances to Mills

#  Note that (1) this is a placeholder and (2) this dataset features mills across the western US.

dat_mills = 
  "data/Data_Mills_MS_20250916.xlsx" %>% 
  read_xlsx %>% 
  vect(geom = c("Long", "Lat"),
       crs = "EPSG:4326") %>% # This could be wrong!
  project("EPSG:2992")

dat_join_mills = 
  dat_mills # %>% 
# distance calculation goes here.

#  Remember to pull this into the "finale" section.

# Distances to Cities
# Distances to Major Roads

# Prices

# Note that this is a placeholder, pending decisions around species, price bins, and regional definitions.

#  Get prices.

dat_prices_19802018 = 
  "data/Prices_ODF/Historic Timber Price Data.xlsx" %>% 
  read_xlsx(sheet = 1) %>% 
  filter(Quarter == 1) %>% 
  select(Year, Region, Price = `DF 2S`)

dat_prices_20192022 =
  "data/Prices_ODF/Historic Timber Price Data.xlsx" %>% 
  read_xlsx(sheet = 2,
            skip = 1) %>% 
  select(YearQuarter = 1, 
         Price_1 = 2, 
         Price_2 = 4,
         Price_3 = 6,
         Price_4 = 8) %>% 
  mutate(Year = YearQuarter %>% str_sub(1, 4),
         Quarter = YearQuarter %>% str_sub(-1, -1)) %>% 
  filter(Quarter == 1) %>% 
  select(Year, starts_with("Price")) %>% 
  pivot_longer(cols = starts_with("Price"),
               names_to = "Region",
               names_prefix = "Price_",
               values_to = "Price") %>% 
  mutate(Year = Year %>% as.numeric,
         Region = Region %>% as.numeric)

dat_prices = bind_rows(dat_prices_19802018, dat_prices_20192022)

#  Get price regions.

dat_prices_regions = 
  "data/Boundaries_ODF/StateForestDistricts" %>% 
  vect %>% 
  mutate(Region = 
           case_when(DISTRICT %in% c("Astoria", "Tillamook", "Forest Grove", "North Cascade", "West Oregon", "Western Lane") ~ 1,
                     DISTRICT %in% c("Coos Bay") ~ 2,
                     DISTRICT %in% c("Grants Pass") ~ 4,
                     TRUE ~ NA),
         .keep = "none") %>% 
  drop_na(Region)

#  Map regions to notifications, then add prices. 

dat_join_prices = 
  dat_notifications %>% 
  intersect(dat_prices_regions) %>% 
  as_tibble %>% 
  left_join(dat_prices, 
            by = c("Year", "Region")) %>% 
  select(UID, Price)

# Finale

dat_notifications_out = 
  dat_notifications %>% 
  left_join(dat_join_pyrome, by = "UID") %>% 
  left_join(dat_join_elevation %>% select(UID, Elevation = OR_DEM_10M.gdb), by = "UID") %>% # Kick this up to the elevation block.
  left_join(dat_join_slope %>% select(UID, Slope = slope), by = "UID") %>% # Ditto.
  left_join(dat_join_mtbs, by = "UID") %>% 
  mutate(across(starts_with("Fire"), ~ replace_na(.x, 0))) %>% # (Fix!) Accounting for observations dropped in MTBS intersect/filter steps.
  left_join(dat_join_prices, by = "UID")

# Check reason for additional observations from start to finish.

writeVector(dat_notifications_out, "output/data_notifications_demo_20250824.gdb", overwrite = TRUE)

write_csv(dat_notifications_out %>% as_tibble, "output/data_notifications_demo_20250824.csv")

time_end = Sys.time()

time_end - time_start
