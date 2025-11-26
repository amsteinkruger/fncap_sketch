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
#  (Roughness?)
#  (PRISM?)
#  Pyromes
#  Fires
#  TreeMap
#  TCC
#  Distances (Mills, Cities, Roads, Rivers/Riparian Zones)
#  Protected Areas
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

# dat_notifications_range = 
#   "output/dat_notifications_polygons.gdb" %>% 
#   vect %>% 
#   as_tibble %>% 
#   filter(ActivityType == "Clearcut/Overstory Removal") %>% 
#   filter(ActivityUnit == "MBF") %>% 
#   filter(LandOwnerType == "Partnership/Corporate Forestland Ownership") %>% 
#   select(starts_with("Date")) %>% 
#   mutate(Date_Check = DateEnd_Right - DateStart_Right) %>% 
#   # mutate(Date_Check = ifelse(!is.na(DateContinuationStart), DateEnd_Right - DateStart_Right, DateContinuationEnd - DateStart_Right)) %>% 
#   select(Date_Check) %>% 
#   mutate(Date_Check_Num = Date_Check %>% as.numeric %>% `/` (86400))
# 
# vis_notifications_range = 
#   dat_notifications_range %>% 
#   filter(quantile(Date_Check_Num, 0.99) > Date_Check_Num) %>% 
#   group_by(Date_Check_Num) %>% 
#   summarize(Count = n()) %>% 
#   ungroup %>% 
#   ggplot() +
#   geom_col(aes(x = Date_Check_Num,
#                y = Count)) +
#   labs(x = "Days in Notification Period",
#        y = "Count of Notifications")
# 
# ggsave("output/vis_notifications_range.png",
#        dpi = 300,
#        width = 5,
#        height = 5)

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

# TreeMap

#  Get FIA data for reference.

dat_fia_plot =
  "data/FIA/OR_PLOT.csv" %>%
  read_csv

dat_fia_cond = 
  "data/FIA/OR_COND.csv" %>% 
  read_csv

#  Get TreeMap metadata and data.

#   Handle initial data.

crs_treemap = 
  "data/TreeMap_2014/national_c2014_tree_list.tif" %>% 
  rast %>% 
  crs

dat_bounds_treemap = dat_bounds %>% project(crs_treemap)

dat_treemap = 
  "data/TreeMap_2014/national_c2014_tree_list.tif" %>% 
  rast %>% 
  crop(dat_bounds_treemap, mask = TRUE) %>% 
  project("EPSG:2992") %>% 
  as.numeric # This matters, but it runs for ~20 minutes (2025/11/25).

vec_treemap = dat_treemap %>% as.vector %>% na.omit %>% unique

#  Get FIA data by way of TreeMap's look-up table. This avoids a confusing raster operation.

dat_treemap_lookup = 
  "data/TreeMap_2014/TL_CN_Lookup.txt" %>% 
  read_delim %>% 
  rename(TL_ID = tl_id) %>% 
  select(TL_ID, CN)

dat_treemap_join =
  vec_treemap %>%
  tibble(TL_ID = .) %>%
  left_join(dat_treemap_lookup) %>% 
  left_join(dat_fia_cond %>% select(CN = PLT_CN, INVYR, FORTYPCD, SITECLCD) %>% mutate(Join = 1))

#  Reclassify Treemap into (1) binary forest types (Douglas Fir / Not) and (2) site class (1-7).

dat_treemap_fortypcd = 
  dat_treemap_join %>% 
  mutate(FORTYPCD_BIN = ifelse(FORTYPCD %in% 201:203, 1, ifelse(!is.na(Join) | !is.na(FORTYPCD), 0, NA))) %>% 
  select(tl_id = TL_ID, FORTYPCD_BIN) %>% 
  as.matrix %>% 
  classify(dat_treemap, .)

dat_treemap_siteclcd = 
  dat_treemap_join %>% 
  select(tl_id = TL_ID, SITECLCD) %>% 
  rename(from = 1, to = 2) %>% 
  as.matrix %>% 
  classify(dat_treemap, .)

#  Extract both results onto notifications for later joins.

dat_join_treemap_fortypcd = 
  dat_notifications %>% 
  extract(dat_treemap_fortypcd, ., fun = "mean", na.rm = TRUE) %>% 
  select(UID = ID, ProportionDouglasFir = tl_id) %>% 
  left_join(dat_notifications, .)

dat_join_treemap_siteclcd_min = 
  dat_notifications %>% 
  extract(dat_treemap_siteclcd, ., fun = "min", na.rm = TRUE) %>% 
  select(UID = ID, SiteClass_Min = tl_id)

dat_join_treemap_siteclcd_max = 
  dat_notifications %>% 
  extract(dat_treemap_siteclcd, ., fun = "max", na.rm = TRUE) %>% 
  select(UID = ID, SiteClass_Max = tl_id)

dat_join_treemap_siteclcd_med = 
  dat_notifications %>% 
  extract(dat_treemap_siteclcd, ., fun = "median", na.rm = TRUE) %>% 
  select(UID = ID, SiteClass_Med = tl_id)

dat_join_treemap_siteclcd = 
  dat_join_treemap_fortypcd %>% 
  left_join(dat_join_treemap_siteclcd_min) %>% 
  left_join(dat_join_treemap_siteclcd_max) %>% 
  left_join(dat_join_treemap_siteclcd_med)

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

# Annual histograms of TCC change for each year for the full study area (inclusive of notifications)

dat_tcc %>% 
  # filter(year %in% 2015:2017) %>% 
  mutate(data_difference_out = 
           data_difference %>% 
           map(as.vector) %>% 
           map(as_tibble) %>% 
           map(drop_na)) %>% 
  select(year, data_difference_out) %>% 
  unnest(data_difference_out) %>% 
  rename(tcc = value) %>% 
  group_by(year) %>% 
  mutate(tcc_decile = tcc %>% ntile(100)) %>% 
  ungroup %>% 
  filter(tcc_decile %in% 6:96) %>% 
  mutate(bin = cut_interval(tcc, n = 20)) %>% # cut_width(tcc, width = 25.5, boundary = -255)
  group_by(year, bin) %>% 
  summarize(count = n()) %>% 
  ggplot() +
  geom_col(aes(x = count,
               y = bin)) +
  facet_wrap(~ year) +
  labs(x = "Pixels", 
       y = "Binned Interannual Change in TCC") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

ggsave("output/vis_tcc_histogram_all_20251030.png",
       dpi = 300,
       width = 7.5,
       height = 7.5)

# Annual histograms of TCC change for each year for each notification

dat_join_tcc %>% 
  rename(tcc = TCC_D) %>% 
  group_by(year) %>% 
  mutate(tcc_decile = tcc %>% ntile(100)) %>% 
  ungroup %>% 
  filter(tcc_decile %in% 6:96) %>% 
  mutate(bin = cut_interval(tcc, n = 20)) %>% # cut_width(tcc, width = 25.5, boundary = -255)
  group_by(year, bin) %>% 
  summarize(count = n()) %>% 
  ggplot() +
  geom_col(aes(x = count,
               y = bin)) +
  facet_wrap(~ year) +
  labs(x = "Notifications", 
       y = "Binned Interannual Change in TCC") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

ggsave("output/vis_tcc_histogram_not_20251030.png",
       dpi = 300,
       width = 7.5,
       height = 7.5)

# Distances

#  Mills

dat_notifications_less = 
  dat_notifications %>% 
  slice_sample(n = 10)

dat_mills = 
  "data/Data_Mills_MS_20250916.xlsx" %>% 
  read_xlsx %>% 
  vect(geom = c("Long", "Lat"),
       crs = "EPSG:4326") %>% # This could be wrong!
  project("EPSG:2992")

dat_join_mills = 
  dat_notifications_less %>% 
  centroids %>% 
  distance(dat_mills) %>% 
  as.data.frame %>% 
  bind_cols(dat_notifications_less %>% as_tibble %>% select(UID), .) %>% 
  pivot_longer(cols = starts_with("V")) %>% 
  group_by(UID) %>% 
  filter(value == min(value)) %>% 
  ungroup %>% 
  select(UID, Distance_Mill = value)

# Cities
# Roads
# Riparian Zones

# Protected Areas

dat_join_pad = 
  "data/PADUS4_1_State_OR_GDB_KMZ/PADUS4_1_StateOR.gdb" %>% 
  vect %>% 
  filter(GAP_Sts %in% c("1", "2")) %>% 
  select(Shape_Area) %>% 
  aggregate %>% 
  project("EPSG:2992") %>% 
  intersect(dat_notifications, .) %>% 
  select(UID) %>% 
  as_tibble %>% 
  mutate(PAD = 1) %>% 
  left_join(dat_notifications, .) %>% 
  mutate(PAD = PAD %>% replace_na(0))

# Prices

#  Get index and prices, map quarters to months, and join.

#  Indexes

#   PPI (All)

dat_ppi = 
  "data/BLS/data_ppi.csv" %>% 
  read_csv %>% 
  mutate(Year = observation_date %>% year,
         Month = observation_date %>% month,
         Factor_PPI_2024 = max(PPIACO) / PPIACO) %>% 
  select(Year, Month, Factor_PPI_2024)

#   PPI (Timber)

dat_ppi_timber = 
  "data/BLS/data_ppi_timber.csv" %>% 
  read_csv %>% 
  mutate(Year = observation_date %>% year,
         Month = observation_date %>% month,
         Factor_PPI_Timber_2024 = max(WPU085) / WPU085) %>% 
  select(Year, Month, Factor_PPI_Timber_2024)

#   Join.

dat_price_index = dat_ppi %>% left_join(dat_ppi_timber)

#   Prices

#    Stumpage, LogLines/FastMarkets

dat_price_stumpage = 
  "data/Prices_FastMarkets/data_stumpage.csv" %>% 
  read_csv %>% 
  rename(Stumpage_Nominal = 2) %>% 
  mutate(Year = Quarter %>% str_sub(1, 4) %>% as.numeric,
         Month = 
           Quarter %>% 
           str_sub(-1, -1) %>% 
           map(~ case_when(.x == 1 ~ 1:3,
                           .x == 2 ~ 4:6,
                           .x == 3 ~ 7:9,
                           .x == 4 ~ 10:12,
                           TRUE ~ NA))) %>% 
  unnest(Month) %>% 
  left_join(dat_price_index) %>% 
  mutate(Stumpage_Real_PPI = Stumpage_Nominal * Factor_PPI_2024,
         Stumpage_Real_PPI_Timber = Stumpage_Nominal * Factor_PPI_Timber_2024) %>% 
  select(Year, Month, starts_with("Stumpage"))

#   Delivered Logs, FastMarkets

dat_price_delivered = 
  "data/Prices_FastMarkets/data_pull_filter.csv" %>% 
  read_csv %>% 
  select(1:3) %>% 
  rename(Delivered_Nominal = 3) %>% 
  left_join(dat_price_index) %>% 
  mutate(Delivered_Real_PPI = Delivered_Nominal * Factor_PPI_2024,
         Delivered_Real_PPI_Timber = Delivered_Nominal * Factor_PPI_Timber_2024) %>% 
  select(Year, Month, starts_with("Delivered"))

#  Join

dat_join_price = 
  dat_notifications %>% 
  as_tibble %>% 
  left_join(dat_price_stumpage) %>% 
  left_join(dat_price_delivered)

# Finale

dat_notifications_out = 
  dat_notifications %>% 
  left_join(dat_join_pyrome, by = "UID") %>% 
  left_join(dat_join_elevation %>% select(UID, Elevation = OR_DEM_10M.gdb), by = "UID") %>% # Kick this up to the elevation block.
  left_join(dat_join_slope %>% select(UID, Slope = slope), by = "UID") %>% # Ditto.
  # PRISM
  left_join(dat_join_mtbs, by = "UID") %>% 
  mutate(across(starts_with("Fire"), ~ replace_na(.x, 0))) %>% # (Fix!) Accounting for observations dropped in MTBS intersect/filter steps.
  left_join(dat_join_treemap) %>% 
  # TCC
  left_join(dat_join_distances) %>% 
  left_join(dat_join_pad) %>% 
  left_join(dat_join_prices, by = "UID")

# Check reason for additional observations from start to finish.

writeVector(dat_notifications_out, "output/dat_notifications_polygons_more.gdb", overwrite = TRUE)

write_csv(dat_notifications_out %>% as_tibble, "output/dat_notifications_flat_more.csv")

time_end = Sys.time()

time_end - time_start
