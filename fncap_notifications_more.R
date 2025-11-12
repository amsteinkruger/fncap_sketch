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

time_start = Sys.time()

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

dat_notifications_range = 
  "output/dat_polygons_20250812.gdb" %>% 
  vect %>% 
  as_tibble %>% 
  filter(ActivityType == "Clearcut/Overstory Removal") %>% 
  filter(ActivityUnit == "MBF") %>% 
  filter(LandOwnerType == "Partnership/Corporate Forestland Ownership") %>% 
  select(starts_with("Date")) %>% 
  mutate(Date_Check = DateEnd_Right - DateStart_Right) %>% 
  # mutate(Date_Check = ifelse(!is.na(DateContinuationStart), DateEnd_Right - DateStart_Right, DateContinuationEnd - DateStart_Right)) %>% 
  select(Date_Check) %>% 
  mutate(Date_Check_Num = Date_Check %>% as.numeric %>% `/` (86400))

vis_notifications_range = 
  dat_notifications_range %>% 
  filter(quantile(Date_Check_Num, 0.99) > Date_Check_Num) %>% 
  group_by(Date_Check_Num) %>% 
  summarize(Count = n()) %>% 
  ungroup %>% 
  ggplot() +
  geom_col(aes(x = Date_Check_Num,
               y = Count)) +
  labs(x = "Days in Notification Period",
       y = "Count of Notifications")

ggsave("output/vis_notifications_range.png",
       dpi = 300,
       width = 5,
       height = 5)

dat_notifications = 
  "output/dat_polygons_20250812.gdb" %>% 
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
#  Note fncap_landfire.R. Remember to fold that in if this all remains in one script.

# After conversation on 10/15, remember to reverse polygon-raster interaction to get all EVT groups of interest.
# And try out the "ceilling" raster that's now written to disk.

dat_evt = "output/dat_evt_binary.tif" %>% rast

dat_join_evt = 
  dat_notifications %>% 
  extract(x = dat_evt,
          y = .,
          fun = mean,
          ID = FALSE,
          bind = TRUE) %>%
  rename(EVT = EVT_NAME) %>%
  as_tibble

# TCC

crs_tcc = 
  "data/NLCD_TCC_2023/nlcd_tcc_conus_wgs84_v2023-5_20230101_20231231.tif" %>% 
  rast %>% 
  crs

dat_tcc = 
  "data/NLCD_TCC_2023/nlcd_tcc_conus_wgs84_v2023-5_20230101_20231231.tif" %>% 
  rast %>% 
  as.numeric %>% 
  crop(dat_bounds %>% project(crs_tcc),
       mask = TRUE) %>% 
  project("EPSG:2992")

dat_join_tcc = 
  dat_join_tcc %>% 
  extract(x = .,
          y = dat_notifications,
          fun = mean,
          ID = FALSE,
          bind = TRUE) %>% 
  rename(TCC = category)

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
