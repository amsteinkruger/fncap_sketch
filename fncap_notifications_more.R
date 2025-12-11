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
#  Roughness
#  (PRISM?)
#  (VPD?)
#  Pyromes
#  Fires
#  TreeMap
#  TCC
#  (Landsat?)
#  Distances (Mills, Cities, Roads)
#  Protected Areas
#  (Slopes, Riparian Zones)
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
  "output/dat_notifications_polygons.gdb" %>% 
  vect %>% 
  filter(ActivityType == "Clearcut/Overstory Removal") %>% 
  filter(ActivityUnit == "MBF") %>% 
  filter(LandOwnerType == "Partnership/Corporate Forestland Ownership") %>% 
  rename(LandOwner = LandOwnerName_Right) %>% 
  select(-ends_with("Right")) %>% 
  rename_with(~ sub("_Left$", "", .x), everything()) %>% 
  mutate(Year_Start = year(DateStart),
         Month_Start = month(DateStart),
         YearMonth_Start = paste0(Year_Start, ifelse(str_length(Month_Start) < 2, "0", ""), Month_Start),
         Year_End = ifelse(is.na(DateContinuationEnd), year(DateEnd), year(DateContinuationEnd)),
         Month_End = ifelse(is.na(DateContinuationEnd), month(DateEnd), month(DateContinuationEnd)),
         YearMonth_End = paste0(Year_End, ifelse(str_length(Month_End) < 2, "0", ""), Month_End),
         MBF = ActivityQuantity %>% as.numeric) %>%
  arrange(desc(Year_Start), desc(Month_Start), LandOwnerType, LandOwner, desc(MBF), desc(Acres)) %>% 
  select(LandOwner,
         ends_with("_Start"), 
         ends_with("_End"), 
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

# Elevation

dat_elevation = 
  "data/Elevation_USGS.tif" %>% 
  rast %>% 
  crop(dat_bounds %>% project("EPSG:4269"),
       mask = TRUE) %>% 
  mutate(Elevation_USGS = Elevation_USGS * 3.2808399) %>% 
  project("EPSG:2992")

dat_join_elevation =
  dat_notifications %>%
  extract(x = dat_elevation,
          y = .,
          fun = mean,
          ID = FALSE,
          bind = TRUE) %>%
  rename(Elevation = 2) %>%
  as_tibble

# Slope
  
dat_slope = dat_elevation %>% terrain(v = "slope")

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

# Roughness

dat_roughness = dat_elevation %>% terrain(v = "roughness")

dat_join_roughness = 
  dat_notifications %>% 
  extract(x = dat_slope,
          y = .,
          fun = mean,
          ID = FALSE,
          bind = TRUE,
          na.rm = TRUE) %>%
  rename(Roughness = 2) %>%
  as_tibble

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
#  9 minutes as of 2025/12/10.

#  Set up notifications to support comparisons between pre- and post-years.

dat_notifications_tcc = 
  dat_notifications %>% 
  filter(Year_End < 2023) %>% 
  as_tibble %>% 
  mutate(Year_Before = Year_Start - 1,
         Year_After = Year_End + 1) %>% 
  select(UID, Year_Before, Year_After) %>% 
  pivot_longer(starts_with("Year"),
               names_prefix = "Year_",
               names_to = "Period",
               values_to = "Year")

#  Extract TCC to notifications, then subset results for comparisons of interest.

dat_join_tcc = 
  "output/TCC.tif" %>% 
  rast %>% 
  extract(., 
          dat_notifications, 
          mean, 
          na.rm = TRUE) %>% 
  bind_cols((dat_notifications$UID %>% tibble(UID = .))) %>% 
  select(-ID) %>% 
  as_tibble %>% 
  left_join((dat_notifications %>% as_tibble %>% select(UID)), ., by = "UID") %>% 
  pivot_longer(cols = starts_with("TCC"),
               names_prefix = "TCC_",
               names_to = "Year",
               values_to = "TCC") %>% 
  group_by(UID) %>% 
  nest(data = c(Year, TCC)) %>% 
  ungroup %>% 
  left_join(dat_notifications_tcc, .) %>% 
  mutate(data = data %>% map2(.x = ., .y = Year, .f = ~ filter(.x, Year == .y))) %>% 
  select(-Year) %>% 
  unnest(data) %>% 
  mutate(TCC_Change = TCC - lag(TCC)) %>% 
  filter(Period == "After") %>% 
  select(UID, TCC_Change)

#  Check the distribution of pre-to-post-notification changes in TCC.

# library(ggpubr)
# library(RColorBrewer)
# 
# pal_tcc_lower = brewer.pal('Reds', n = 8) %>% rev
# pal_tcc_upper = brewer.pal('Greys', n = 7)
# pal_tcc = c(pal_tcc_lower, pal_tcc_upper)

# vis_tcc = 
#   dat_join_tcc %>% 
#   mutate(bin = cut_interval(TCC_Change, length = 10)) %>% 
#   group_by(bin) %>% 
#   summarize(count = n()) %>% 
#   ungroup %>% 
#   ggplot() +
#   geom_col(aes(x = bin,
#                y = count,
#                fill = bin),
#            color = "#000000") +
#   labs(x = "Change in TCC (Pre- to Post-Notification)",
#        y = "Notification Count") +
#   scale_fill_manual(values = pal_tcc) +
#   scale_y_continuous(expand = c(0, 0)) +
#   theme_pubr() +
#   theme(legend.position = "none",
#         axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
#   
# ggsave("output/vis_tcc_20251210.png",
#        dpi = 300,
#        width = 6.5)

# Landsat
  

  
# Distances

#  Mills

dat_mills = 
  "data/Data_Mills_MS_20250916.xlsx" %>% 
  read_xlsx %>% 
  vect(geom = c("Long", "Lat"),
       crs = "EPSG:4326") %>% # This could be wrong!
  project("EPSG:2992")

dat_join_mills = 
  dat_notifications %>% 
  centroids %>% 
  distance(dat_mills) %>% 
  as.data.frame %>% 
  bind_cols(dat_notifications %>% as_tibble %>% select(UID), .) %>% 
  pivot_longer(cols = starts_with("V")) %>% 
  group_by(UID) %>% 
  filter(value == min(value)) %>% 
  ungroup %>% 
  distinct(UID, value) %>% 
  mutate(value = value / 5280) %>% 
  select(UID, Distance_Mill = value)

#  Roads

dat_roads = 
  "data/NAR.gdb" %>% 
  vect %>% 
  crop(dat_bounds %>% project("EPSG:4326")) %>% 
  project("EPSG:2992")

dat_join_roads = 
  dat_notifications %>% 
  centroids %>% 
  distance(dat_roads) %>% 
  as.data.frame %>% 
  bind_cols(dat_notifications %>% as_tibble %>% select(UID), .) %>% 
  pivot_longer(cols = starts_with("V")) %>% 
  group_by(UID) %>% 
  filter(value == min(value)) %>% 
  ungroup %>% 
  distinct(UID, value) %>% 
  mutate(value = value / 5280) %>% 
  select(UID, Distance_Road = value)

#  Cities

dat_cities = 
  "data/TIGER.gdb" %>% 
  vect %>% 
  crop(dat_bounds %>% project("EPSG:4269")) %>% 
  project("EPSG:2992")

dat_join_cities = 
  dat_notifications %>% 
  centroids %>% 
  distance(dat_cities) %>% 
  as.data.frame %>% 
  bind_cols(dat_notifications %>% as_tibble %>% select(UID), .) %>% 
  pivot_longer(cols = starts_with("V")) %>% 
  group_by(UID) %>% 
  filter(value == min(value)) %>% 
  ungroup %>% 
  distinct(UID, value) %>% # For multiple occurrences of a minimum value. 
  mutate(value = value / 5280) %>% 
  select(UID, Distance_Place = value)

#  Combined

dat_join_distance = 
  dat_join_roads %>% 
  left_join(dat_join_mills) %>% 
  left_join(dat_join_cities)

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

# Wrangle prices a little more.

library(gt)

dat_price_check = 
  dat_price_stumpage %>% 
  left_join(dat_price_delivered) %>% 
  select(Year, 
         Month, 
         Stumpage = Stumpage_Real_PPI_Timber, 
         Delivered = Delivered_Real_PPI_Timber) %>% 
  filter(Month %in% c(1, 4, 7, 9)) %>% 
  mutate(Delivered_1 = lag(Delivered, 1)) %>% 
  mutate(Delivered_2 = lag(Delivered, 2)) %>% 
  mutate(Delivered_3 = lag(Delivered, 3)) %>% 
  mutate(Delivered_4 = lag(Delivered, 4)) %>% 
  mutate(Delivered_5 = lag(Delivered, 5)) %>% 
  mutate(Delivered_6 = lag(Delivered, 6)) %>% 
  mutate(Delivered_7 = lag(Delivered, 7)) %>% 
  mutate(Delivered_8 = lag(Delivered, 8)) %>% 
  mutate(Cor_0 = cor(Stumpage, Delivered, use = "complete.obs")) %>% 
  mutate(Cor_1 = cor(Stumpage, Delivered_1, use = "complete.obs")) %>% 
  mutate(Cor_2 = cor(Stumpage, Delivered_2, use = "complete.obs")) %>% 
  mutate(Cor_3 = cor(Stumpage, Delivered_3, use = "complete.obs")) %>% 
  mutate(Cor_4 = cor(Stumpage, Delivered_4, use = "complete.obs")) %>% 
  mutate(Cor_5 = cor(Stumpage, Delivered_5, use = "complete.obs")) %>% 
  mutate(Cor_6 = cor(Stumpage, Delivered_6, use = "complete.obs")) %>% 
  mutate(Cor_7 = cor(Stumpage, Delivered_7, use = "complete.obs")) %>% 
  mutate(Cor_8 = cor(Stumpage, Delivered_8, use = "complete.obs")) %>% 
  select(starts_with("Cor")) %>% 
  distinct %>% 
  pivot_longer(cols = everything()) %>% 
  rename(Lag = name, PCC = value) %>% 
  mutate(Lag = Lag %>% str_sub(-1, -1)) %>% 
  gt()

# Finale

dat_notifications_out = 
  dat_notifications %>% 
  left_join(dat_join_pyrome, by = "UID") %>% 
  left_join(dat_join_elevation %>% select(UID, Elevation = OR_DEM_10M.gdb), by = "UID") %>% # Kick this up to the elevation block.
  left_join(dat_join_slope %>% select(UID, Slope = slope), by = "UID") %>% # Ditto.
  # PRISM
  # VPD
  left_join(dat_join_mtbs, by = "UID") %>% 
  mutate(across(starts_with("Fire"), ~ replace_na(.x, 0))) %>% # (Fix!) Accounting for observations dropped in MTBS intersect/filter steps.
  left_join(dat_join_treemap) %>% 
  left_join(dat_join_tcc) %>% 
  # Landsat
  left_join(dat_join_distances) %>% 
  left_join(dat_join_pad) %>% 
  # ODF Waterways/Slopes
  left_join(dat_join_prices, by = "UID")

# Check reason for additional observations from start to finish.

writeVector(dat_notifications_out, "output/dat_notifications_polygons_more.gdb", overwrite = TRUE)

write_csv(dat_notifications_out %>% as_tibble, "output/dat_notifications_flat_more.csv")

time_end = Sys.time()

time_end - time_start
