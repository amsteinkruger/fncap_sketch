# Join covariates to processed notifications with an annual result.

#  55' (2026/1/12)

# TOC:
#  Stopwatch
#  Packages
#  Bounds
#  Notifications
#  Intersections and Recurrences
#  Elevation
#  Slope
#  Roughness
#  VPD
#  Pyromes
#  Fires
#  TreeMap
#  TCC
#  NDVI
#  Distances
#  Protected Areas
#  Riparian Zones and Slopes *
#  Prices
#  Export
#  Stopwatch

# Stopwatch

time_start = Sys.time()

# Packages

library(tidyverse)
library(magrittr)
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
  summarize(Pyrome = "All Pyromes") %>% 
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
         Year_End = ifelse(is.na(DateContinuationEnd), year(DateEnd), year(DateContinuationEnd)),
         Month_End = ifelse(is.na(DateContinuationEnd), month(DateEnd), month(DateContinuationEnd)),
         MBF = ActivityQuantity %>% as.numeric) %>%
  arrange(desc(Year_Start), desc(Month_Start), LandOwnerType, LandOwner, desc(MBF), desc(Acres)) %>% 
  filter(Year_Start > 2014 & Year_End < 2025) %>% 
  select(LandOwner,
         ends_with("_Start"), 
         ends_with("_End"), 
         MBF, 
         Acres) %>% 
  project("EPSG:2992") %>% 
  # Fix invalid polygons.
  makeValid %>% 
  # Subset for quick tests.
  # slice_sample(n = 1000) %>%
  # Crop.
  crop(dat_bounds) %>% 
  # Assign unique ID.
  mutate(UID = row_number()) %>% 
  relocate(UID)

dat_notifications_less_1 = dat_notifications %>% select(UID)
dat_notifications_less_2 = dat_notifications %>% select(UID, Year_Start)

# Count intersections and recurrences within the set of notifications.

dat_notifications_intersect = 
  dat_notifications %>% 
  relate(., ., relation = "intersects") %>% 
  rowSums() %>% 
  `-` (1) %>% 
  tibble(UID = seq(1, length(.)), Intersects = .)

dat_notifications_equal = 
  dat_notifications %>% 
  relate(., ., relation = "equals") %>% 
  rowSums() %>% 
  `-` (1) %>% 
  tibble(UID = seq(1, length(.)), Equals = .)

# Elevation

dat_elevation = 
  "data/Elevation_USGS.tif" %>% 
  rast %>% 
  crop(dat_bounds %>% project("EPSG:4269"),
       mask = TRUE) %>% 
  mutate(Elevation_USGS = Elevation_USGS * 3.2808399) %>% # Meters to feet for consistency with the CRS.
  project("EPSG:2992")

dat_join_elevation =
  dat_notifications_less_1 %>%
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
  dat_notifications_less_1 %>%
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
  dat_notifications_less_1 %>% 
  extract(x = dat_roughness,
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
  dat_notifications_less_1 %>% 
  intersect(dat_pyrome) %>% 
  as_tibble %>% 
  group_by(UID) %>% 
  filter(row_number() == 1) %>% # Keep only the first pyrome intersecting with a notification. This is arbitrary.
  ungroup

# VPD

dat_vpd = "output/data_vpd.tif" %>% rast

dat_vpd_annual = 
  tibble(year = 1984:2024) %>%
  mutate(string = paste0("VPD_", year)) %>%
  mutate(data = string %>% map( ~ { dat_vpd %>% select(starts_with(.x)) %>% mean(na.rm = TRUE) })) %>% # Get annual means.
  mutate(data =
           data %>%
           map2(.x = .,
                .y = string,
                ~ {
                  names(.x) <- as.character(.y)
                  .x
                })) %>% # Restore names.
  magrittr::extract2("data") %>% # Equivalent to .$data.
  reduce(c) 

dat_join_vpd = 
  tibble(year = 2014:2024) %>% 
  mutate(string = paste0("VPD_", year),
         years = year %>% map(~ seq(.x - 30, .x - 1)) %>% map(as.character),
         data = 
           years %>% 
           map( ~ dat_vpd_annual %>% select(ends_with(.x)) %>% mean(na.rm = TRUE)) %>% 
           map2(.x = .,
                .y = string,
                ~ {
                  names(.x) <- as.character(.y)
                  .x
                })) %>% 
  magrittr::extract2("data") %>% # Equivalent to .$data.
  reduce(c) %>% 
  extract(., 
          dat_notifications_less_1, 
          mean, 
          na.rm = TRUE) %>% 
  bind_cols((dat_notifications_less_1$UID %>% tibble(UID = .))) %>% 
  select(-ID) %>% 
  as_tibble %>% 
  left_join((dat_notifications_less_1 %>% as_tibble), ., by = "UID") %>% 
  pivot_longer(cols = starts_with("VPD"),
               names_prefix = "VPD_",
               names_to = "Year",
               values_to = "VPD") %>% 
  group_by(UID) %>% 
  nest(data = c(Year, VPD)) %>% 
  ungroup %>% 
  left_join(dat_notifications_less_2 %>% as_tibble %>% rename(Year = Year_Start), .) %>% 
  mutate(data = data %>% map2(.x = ., .y = Year, .f = ~ filter(.x, Year == .y))) %>% 
  select(-Year) %>% 
  unnest(data) %>% 
  select(UID, VPD)

# MTBS

dat_mtbs = 
  "data/mtbs_perimeter_data" %>% 
  vect %>% 
  filter(substr(Event_ID, 1, 2) == "OR") %>% 
  project("EPSG:2992") %>% 
  mutate(Year_MTBS = Ig_Date %>% year, 
         .keep = "none") %>%
  filter(Year_MTBS %in% 1994:2024) %>% 
  crop(dat_bounds)

# 1. No Buffer

dat_join_mtbs_0 = 
  dat_notifications_less_2 %>% 
  intersect(dat_mtbs) %>% 
  as_tibble %>% 
  filter(Year_Start > Year_MTBS & Year_Start - 20 < Year_MTBS) %>% 
  group_by(UID) %>% 
  summarize(Fire_0 = n()) %>% 
  ungroup

# 2. 15km Buffer

dat_join_mtbs_15 = 
  dat_notifications_less_2 %>% 
  buffer(width = 15 * 3280.84) %>% # Kilometers to feet.
  intersect(dat_mtbs) %>% 
  as_tibble %>% 
  filter(Year_Start > Year_MTBS & Year_Start - 20 < Year_MTBS) %>% 
  group_by(UID) %>% 
  summarize(Fire_15 = n()) %>% 
  ungroup

# 3. 30km Buffer

dat_join_mtbs_30 = 
  dat_notifications_less_2 %>% 
  buffer(width = 30 * 3280.84) %>% # Kilometers to feet.
  intersect(dat_mtbs) %>% 
  as_tibble %>% 
  filter(Year_Start >= Year_MTBS & Year_Start - 20 < Year_MTBS) %>% 
  group_by(UID) %>% 
  summarize(Fire_30 = n()) %>% 
  ungroup

# 4. Combine

dat_join_mtbs = 
  dat_notifications_less_1 %>% 
  as_tibble %>% 
  left_join(dat_join_mtbs_0) %>% 
  left_join(dat_join_mtbs_15) %>% 
  left_join(dat_join_mtbs_30) %>% 
  mutate(Fire_0 = Fire_0 %>% replace_na(0),
         Fire_15 = Fire_15 %>% replace_na(0),
         Fire_30 = Fire_30 %>% replace_na(0),
         Fire_15_Doughnut = Fire_15 - Fire_0,
         Fire_30_Doughnut = Fire_30 - Fire_15)

# TreeMap

#  Get FIA data.

dat_fia_cond = 
  bind_rows("data/FIA/CA_COND.csv" %>% read_csv %>% select(CN = PLT_CN, INVYR, FORTYPCD, SITECLCD),
            "data/FIA/OR_COND.csv" %>% read_csv %>% select(CN = PLT_CN, INVYR, FORTYPCD, SITECLCD),
            "data/FIA/WA_COND.csv" %>% read_csv %>% select(CN = PLT_CN, INVYR, FORTYPCD, SITECLCD)) %>% 
  distinct %>% 
  drop_na %>% # For FORTYPCD and SITECLCD.
  mutate(Join = 1)

#  Get TreeMap data.

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
  left_join(dat_fia_cond)

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
  dat_notifications_less_1 %>% 
  extract(dat_treemap_fortypcd, ., fun = "mean", na.rm = TRUE) %>% 
  select(ProportionDouglasFir = tl_id) %>% 
  bind_cols(dat_notifications_less_1 %>% as_tibble, .)

dat_join_treemap_siteclcd_min = 
  dat_notifications_less_1 %>% 
  extract(dat_treemap_siteclcd, ., fun = "min", na.rm = TRUE) %>% 
  select(SiteClass_Min = tl_id) %>% 
  bind_cols(dat_notifications_less_1 %>% as_tibble, .)

dat_join_treemap_siteclcd_max = 
  dat_notifications_less_1 %>% 
  extract(dat_treemap_siteclcd, ., fun = "max", na.rm = TRUE) %>% 
  select(SiteClass_Max = tl_id) %>% 
  bind_cols(dat_notifications_less_1 %>% as_tibble, .)

dat_join_treemap_siteclcd_med = 
  dat_notifications_less_1 %>% 
  extract(dat_treemap_siteclcd, ., fun = "median", na.rm = TRUE) %>% 
  select(SiteClass_Med = tl_id) %>% 
  bind_cols(dat_notifications_less_1 %>% as_tibble, .)

dat_join_treemap = 
  dat_join_treemap_fortypcd %>% 
  left_join(dat_join_treemap_siteclcd_min) %>% 
  left_join(dat_join_treemap_siteclcd_max) %>% 
  left_join(dat_join_treemap_siteclcd_med)

# TCC

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
  "output/data_tcc.tif" %>% 
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

# NDVI

dat_join_ndvi_read = "output/data_ndvi.tif" %>% rast

#  Annual

#  Set up notifications to support comparisons between pre- and post-years.

dat_notifications_ndvi_annual =
  dat_notifications %>%
  filter(Year_End < 2024) %>%
  as_tibble %>%
  mutate(Year_Before = Year_Start - 1,
         Year_After = Year_End + 1) %>%
  select(UID, Year_Before, Year_After) %>%
  pivot_longer(starts_with("Year"),
               names_prefix = "Year_",
               names_to = "Period",
               values_to = "Year")

#  Extract NDVI to notifications, then subset results for comparisons of interest.

# Remember to add a switch here to read output if output exists and otherwise annualize data and export.

dat_join_ndvi_annual = "output/data_ndvi_annual.tif" %>% rast

dat_join_ndvi_annual = 
  # tibble(year = 2014:2024) %>% 
  # mutate(string = paste0("NDVI_", year)) %>% 
  # mutate(data = string %>% map( ~ { dat_join_ndvi_read %>% select(starts_with(.x)) %>% mean(na.rm = TRUE) })) %>% # Get annual means.
  # mutate(data = 
  #          data %>% 
  #          map2(.x = ., 
  #               .y = string, 
  #               ~ {
  #                 names(.x) <- as.character(.y)
  #                 .x
  #               })) %>% # Restore names.
  # magrittr::extract2("data") %>% # Equivalent to .$data.
  # reduce(c) %>% 
  # export and switch goes here
  dat_join_ndvi_annual %>% 
  extract(., 
          dat_notifications_less_1, 
          mean, 
          na.rm = TRUE) %>% 
  bind_cols((dat_notifications_less_1$UID %>% tibble(UID = .))) %>% 
  select(-ID) %>% 
  as_tibble %>% 
  left_join(dat_notifications_less_1 %>% as_tibble, ., by = "UID") %>% 
  pivot_longer(cols = starts_with("NDVI"),
               names_prefix = "NDVI_",
               names_to = "Year",
               values_to = "NDVI") %>% 
  group_by(UID) %>% 
  nest(data = c(Year, NDVI)) %>% 
  ungroup %>% 
  left_join(dat_notifications_ndvi_annual, .) %>% 
  mutate(data = data %>% map2(.x = ., .y = Year, .f = ~ filter(.x, Year == .y))) %>% 
  select(-Year) %>% 
  unnest(data) %>% 
  mutate(NDVI_Change = NDVI - lag(NDVI)) %>% 
  filter(Period == "After") %>% 
  select(UID, NDVI_Change)  

# Problems with quarters:
#  (1) Storing absolute names of quarters (1-44) in names of rasters in a raster stack. This is not a big problem.
#  (2) Accounting for seasonal change between quarters. This is a big problem. This requires the ML thing for change to be interpretable.
#        Well, not ML, but something statistical, and in practice it will come with the ML stuff for harvest detection.
#  (3) Accounting for during- quarters by expanding notifications to multiple rows (notification-quarters). This is a big but clean problem.

#  Quarterly (Pre- and Post-)
#   Set up notifications to support comparisons between pre- and post-quarters, excluding during- quarters.

# dat_notifications_ndvi_quarterly_less = 
#   dat_notifications %>%
#   mutate(Quarter_Start = Month_Start %>% `/` (3) %>% ceiling,
#          Quarter_End = Month_End %>% `/` (3) %>% ceiling,
#          Quarter_Start_Running = Quarter_Start + (Year_Start - min(Year_Start)) * 4,
#          Quarter_End_Running = Quarter_End + (Year_End - min(Year_End)) * 4) %>% 
#   filter(Quarter_End_Running < 44) %>%
#   as_tibble %>%
#   mutate(Quarter_Before = Quarter_Start_Running - 1,
#          Quarter_After = Quarter_End_Running + 1) %>%
#   select(UID, Quarter_Before, Quarter_After) %>%
#   pivot_longer(starts_with("Quarter"),
#                names_prefix = "Quarter_",
#                names_to = "Period",
#                values_to = "Quarter")  

# dat_join_ndvi_quarterly_less = 

#  Quarterly (Pre-, During-, Post-)

# dat_notifications_ndvi_quarterly_more
# dat_join_ndvi_quarterly_more
  
# Distances

#  Mills

dat_mills = 
  "data/Data_Mills_MS_20250916.xlsx" %>% 
  read_xlsx %>% 
  vect(geom = c("Long", "Lat"),
       crs = "EPSG:4326") %>% # This could be wrong!
  project("EPSG:2992")

dat_join_mills = 
  dat_notifications_less_1 %>% 
  centroids %>% 
  distance(dat_mills) %>% 
  as.data.frame %>% 
  bind_cols(dat_notifications_less_1 %>% as_tibble, .) %>% 
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
  dat_notifications_less_1 %>% 
  centroids %>% 
  distance(dat_roads) %>% 
  as.data.frame %>% 
  bind_cols(dat_notifications_less_1 %>% as_tibble, .) %>% 
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
  dat_notifications_less_1 %>% 
  centroids %>% # Check whether INTPTLAT, INTPTLON are more informative than centroids.
  distance(dat_cities) %>% 
  as.data.frame %>% 
  bind_cols(dat_notifications_less_1 %>% as_tibble, .) %>% 
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
  crop(dat_bounds) %>% 
  intersect(dat_notifications_less_1, .) %>% 
  select(UID) %>% 
  as_tibble %>% 
  mutate(PAD = 1) %>% 
  left_join(dat_notifications_less_1 %>% as_tibble, .) %>% 
  mutate(PAD = PAD %>% replace_na(0))

# Riparian Zones and Slopes

# The flow line geodatabase crashes R. 

# dat_fpa_1 =
#   "data/FPA/Hydrography_Flow_Line.gdb" %>%
#   vect %>%
#   aggregate %>%
#   crop(dat_bounds %>% project("EPSG:3857")) %>%
#   project("EPSG:2992") %>%
#   intersect(dat_notifications_less_1, .) %>%
#   select(UID) %>%
#   as_tibble %>%
#   mutate(FPA_1 = 1) %>%
#   left_join(dat_notifications_less_1 %>% as_tibble, .) %>%
#   mutate(FPA_1 = FPA_1 %>% replace_na(0))

# The sediment source, flow area, and flow subbasin files do not crash R, but they're pretty slow to process.

# dat_fpa_2 = 
#   "data/FPA/Topography_Designated_Sediment_Source_Area.gdb" %>% 
#   vect %>% 
#   aggregate %>% 
#   crop(dat_bounds %>% project("EPSG:3857")) %>% 
#   project("EPSG:2992") %>% 
#   intersect(dat_notifications_less_1, .) %>% 
#   select(UID) %>% 
#   as_tibble %>% 
#   mutate(FPA_2 = 1) %>% 
#   left_join(dat_notifications_less_1 %>% as_tibble, .) %>% 
#   mutate(FPA_2 = FPA_2 %>% replace_na(0))

# dat_fpa_3 = 
#   "data/FPA/Topography_Debris_Flow_Traversal_Area.gdb" %>% 
#   vect %>% 
#   aggregate %>% 
#   crop(dat_bounds %>% project("EPSG:3857")) %>% 
#   project("EPSG:2992") %>% 
#   intersect(dat_notifications_less_1, .) %>% 
#   select(UID) %>% 
#   as_tibble %>% 
#   mutate(FPA_3 = 1) %>% 
#   left_join(dat_notifications_less_1 %>% as_tibble, .) %>% 
#   mutate(FPA_3 = FPA_3 %>% replace_na(0))

# dat_fpa_4 = 
#   "data/FPA/Topography_Debris_Flow_Traversal_Subbasin.gdb" %>% 
#   vect %>% 
#   aggregate %>% 
#   crop(dat_bounds %>% project("EPSG:3857")) %>% 
#   project("EPSG:2992") %>% 
#   intersect(dat_notifications_less_1, .) %>% 
#   select(UID) %>% 
#   as_tibble %>% 
#   mutate(FPA_4 = 1) %>% 
#   left_join(dat_notifications_less_1 %>% as_tibble, .) %>% 
#   mutate(FPA_4 = FPA_4 %>% replace_na(0))

# dat_join_fpa = 
#   dat_fpa_1 %>% 
#   dat_fpa_2 %>% 
#   left_join(dat_fpa_3) %>% 
#   left_join(dat_fpa_4)

# Prices

#  Note shenanigans with years, quarters, and months across datasets.

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
  select(Year, Month, starts_with("Stumpage")) %>% 
  group_by(Year) %>% 
  summarize(across(starts_with("Stumpage"), mean, na.rm = TRUE)) %>% 
  ungroup

#   Delivered Logs, FastMarkets

dat_price_delivered = 
  "data/Prices_FastMarkets/data_pull_filter.csv" %>% 
  read_csv %>% 
  select(1:3) %>% 
  rename(Delivered_Nominal = 3) %>% 
  left_join(dat_price_index) %>% 
  mutate(Delivered_Real_PPI = Delivered_Nominal * Factor_PPI_2024,
         Delivered_Real_PPI_Timber = Delivered_Nominal * Factor_PPI_Timber_2024) %>% 
  select(Year, Month, starts_with("Delivered")) %>% 
  group_by(Year) %>% 
  summarize(across(starts_with("Delivered"), mean, na.rm = TRUE)) %>% 
  ungroup

#  Join

dat_join_price = 
  dat_notifications_less_2 %>% 
  as_tibble %>% 
  left_join(dat_price_stumpage, by = join_by(Year_Start == Year)) %>% 
  left_join(dat_price_delivered, by = join_by(Year_Start == Year)) %>% 
  select(-Year_Start)

# Export

dat_notifications_less_1 %T>% writeVector("output/dat_notifications_more_polygons.gdb", overwrite = TRUE)

dat_notifications_out = 
  dat_notifications %>% 
  as_tibble %>% 
  left_join(dat_notifications_intersect) %>% 
  left_join(dat_notifications_equal) %>% 
  left_join(dat_join_elevation) %>% 
  left_join(dat_join_slope) %>% 
  left_join(dat_join_roughness) %>% 
  left_join(dat_join_pyrome) %>% 
  left_join(dat_join_vpd) %>% 
  left_join(dat_join_mtbs) %>% 
  left_join(dat_join_treemap) %>% 
  left_join(dat_join_tcc) %>% 
  left_join(dat_join_ndvi_annual) %>% 
  left_join(dat_join_distance) %>% 
  left_join(dat_join_pad) %>% 
  # left_join(dat_join_fpa) %>% 
  left_join(dat_join_price) %T>% 
  write_csv("output/dat_notifications_more_annual.csv")

# Stopwatch

time_end = Sys.time()

time_end - time_start
