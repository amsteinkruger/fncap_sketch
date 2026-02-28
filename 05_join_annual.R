# Join covariates to processed notifications with an annual result.

#  Remember that UID is fragile throughout this script. (And maybe remember to fix that.)

#  55' (2026/1/12)
#  ~ 3h? (2026/2/17)

# TOC:
#  Stopwatch
#  Packages
#  Bounds
#  Notifications
#  Ownership
#  Intersections
#  Elevation
#  Slope
#  Roughness
#  VPD
#  Pyromes
#  Fires
#  TreeMap
#  Growth *
#  TCC
#  NDVI
#  Harvest Detection *
#  Distances
#  Public Lands
#  Protected Lands
#  Flow Lines
#  Steep Slopes *
#  Watersheds
#  Counties
#  Prices
#  Effective Federal Funds Rate
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
  rename(LandOwner = LandOwnerName_Right,
         LandOwnerCompany = LandOwnerCompany_Right) %>% 
  select(-ends_with("Right")) %>% 
  rename_with(~ sub("_Left$", "", .x), everything()) %>% 
  mutate(Year_Start = year(DateStart),
         Month_Start = month(DateStart),
         Year_End = ifelse(is.na(DateContinuationEnd), year(DateEnd), year(DateContinuationEnd)),
         Month_End = ifelse(is.na(DateContinuationEnd), month(DateEnd), month(DateContinuationEnd)),
         MBF = ActivityQuantity %>% as.numeric) %>%
  arrange(desc(Year_Start), desc(Month_Start), LandOwnerType, LandOwner, desc(MBF), desc(Acres)) %>% 
  filter(Year_Start > 2014 & Year_End < 2025) %>% 
  mutate(LandOwnerCompany = LandOwnerCompany %>% str_sub(3, -1)) %>% 
  select(LandOwner,
         LandOwnerCompany,
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

# Ownership

#  This should go in another script for ease of interpretation, but: 
#   one line exports companies for hand-fixing, 
#   the next line reads them back in,
#   and the last line drops the original company field from the main data object.

dat_notifications %>% as_tibble %>% select(LandOwner, LandOwnerCompany) %>% distinct %>% write_csv("output/landowners.csv")

dat_join_owner = 
  "output/landowners.xlsx" %>% 
  read_xlsx %>% 
  mutate(LandOwnerCompany = ifelse(is.na(LandOwnerCompany), "", LandOwnerCompany),
         LandOwnerCompany = ifelse(LandOwnerCompany == "NA", "", LandOwnerCompany)) %>% 
  left_join(dat_notifications %>% 
              select(UID, LandOwner, LandOwnerCompany) %>% 
              as_tibble %>% 
              mutate(LandOwnerCompany = ifelse(is.na(LandOwnerCompany), "", LandOwnerCompany),
                     LandOwnerCompany = ifelse(LandOwnerCompany == "NA", "", LandOwnerCompany)), 
            .) %>% 
  select(UID, Company = Company_2)

dat_notifications = dat_notifications %>% select(-LandOwnerCompany)

# Owner Scale
#  This should really run after subsetting notifications.

dat_join_scale = 
  dat_notifications %>% 
  as_tibble %>% 
  left_join(dat_join_owner) %>% 
  group_by(Company) %>% 
  summarize(Company_MBF = sum(MBF),
            Company_Acres = sum(Acres),
            Company_Notifications = n()) %>% 
  ungroup %>% 
  arrange(desc(Company_MBF)) %>% 
  mutate(Company_Percentile_MBF = ntile(Company_MBF, 100),
         Company_Percentile_Acres = ntile(Company_Acres, 100))

# Intersections

#  Count intersections.

dat_notifications_intersect = 
  dat_notifications %>% 
  relate(., ., relation = "intersects")

dat_join_intersect_binary = 
  dat_notifications_intersect %>% 
  rowSums() %>% 
  `-` (1) %>% 
  tibble(UID = seq(1, length(.)), Intersects = .)

#  Compute proportional areas of intersections.

dat_join_intersect_proportional = 
  dat_notifications_intersect %>% 
  as_tibble %>% 
  rownames_to_column("from") %>% 
  pivot_longer(cols = -from,
               names_to = "to",
               values_to = "intersects") %>% 
  mutate(to = to %>% str_sub(2, -1),
         across(c(to, from), ~ as.integer(.x))) %>% 
  filter(from != to) %>% # Drop autocomparisons.
  filter(from < to) %>% # Drop duplicate comparisons.
  filter(intersects == TRUE) %>% # Drop uninteresting comparisons.
  mutate(area_intersect = 
           map2(from, 
                to, 
                ~ expanse(intersect(dat_notifications_less_1[.x], 
                                    dat_notifications_less_1[.y]), 
                          unit = "ha"))) %>% 
  unnest(area_intersect) %>% 
  mutate(Acres_Intersect = area_intersect * 2.47) %>% # Hectares to acres.
  left_join(dat_notifications %>% 
              as_tibble %>% 
              select(UID, Year_Start, Year_End, Acres), 
            by = c("from" = "UID")) %>% 
  rename(Acres_From = Acres,
         Year_From = Year_Start) %>% 
  left_join(dat_notifications %>% 
              as_tibble %>% 
              select(UID, Year_Start, Year_End, Acres), 
            by = c("to" = "UID")) %>% 
  rename(Acres_To = Acres,
         Year_To = Year_Start) %>% 
  mutate(Acres_From_Proportion = Acres_Intersect / Acres_From,
         Acres_To_Proportion = Acres_Intersect / Acres_To) %>% 
  select(from, to, Acres_From_Proportion, Acres_To_Proportion) %>% 
  pivot_longer(c(from, to)) %>% 
  mutate(Proportion = ifelse(name == "from", Acres_From_Proportion, Acres_To_Proportion)) %>% 
  select(UID = value, Intersect = Proportion) %>% 
  group_by(UID) %>% 
  summarize(Intersect_Maximum = Intersect %>% max) %>% 
  left_join(dat_notifications_less_1 %>% as_tibble, .) %>% 
  mutate(Intersect_Maximum = Intersect_Maximum %>% replace_na(0))

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
  filter(row_number() == 1) %>% # Keep only the first pyrome intersecting a notification. This is arbitrary.
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

dat_join_treemap_siteclcd_mod = 
  dat_notifications_less_1 %>% 
  extract(dat_treemap_siteclcd, ., fun = "modal", na.rm = TRUE) %>% 
  select(SiteClass_Mod = tl_id) %>% 
  bind_cols(dat_notifications_less_1 %>% as_tibble, .)

dat_join_treemap = 
  dat_join_treemap_fortypcd %>% 
  left_join(dat_join_treemap_siteclcd_min) %>% 
  left_join(dat_join_treemap_siteclcd_max) %>% 
  left_join(dat_join_treemap_siteclcd_med) %>% 
  left_join(dat_join_treemap_siteclcd_mod)

# Growth



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

# Harvest Detection

#  Get public timber sales in a convenient format.

crs_usfs = 
  "data/USFS_Harvest/Actv_TimberHarvest.gdb" %>% 
  vect %>% 
  crs

dat_bounds_usfs = dat_bounds %>% project(crs_usfs)
  
dat_detect_usfs = 
  "data/USFS_Harvest/Actv_TimberHarvest.gdb" %>% 
  vect %>% 
  select(ADMIN_FOREST_NAME,
         ACTIVITY_CN,
         ACTIVITY_UNIT_CN,
         ACTIVITY_CODE,
         ACTIVITY_NAME,
         TREATMENT_TYPE,
         DATE_PLANNED,
         DATE_AWARDED,
         DATE_COMPLETED,
         METHOD_CODE,
         METHOD_DESC,
         EQUIPMENT_CODE,
         EQUIPMENT_DESC,
         LAND_SUITABILITY_CLASS_CODE, 
         LAND_SUITABILITY_CLASS_DESC, 
         PRODUCTIVITY_CLASS_CODE, 
         PRODUCTIVITY_CLASS_DESC, 
         OWNERSHIP_CODE, 
         OWNERSHIP_DESC, 
         ASPECT, 
         ELEVATION, 
         SLOPE, 
         STATE_ABBR) %>% 
  filter(STATE_ABBR == "OR") %>% 
  filter(ACTIVITY_NAME == "Commercial Thin") %>% 
  filter(year(DATE_COMPLETED) %in% 2015:2024) %>%
  filter(year(DATE_COMPLETED) - year(DATE_AWARDED) == 0) %>% 
  crop(dat_bounds_usfs) %>% 
  project("EPSG:2992") %>% 
  mutate(UID = row_number())

#  Get TCC for public timber sales.

#   Set up public timber sales in a convenient format for TCC extraction.

# dat_detect_usfs_tcc = 
#   dat_detect_usfs %>% 
#   as_tibble %>% 
#   mutate(Year = DATE_COMPLETED %>% year,
#          Activity = 1) %>% 
#   filter(Year %in% 2015:2023) %>% 
#   select(UID, Year, Activity)

#   Handle TCC.

# dat_detect_tcc = 
#   "output/data_tcc.tif" %>% 
#   rast %>% 
#   extract(.,
#           dat_detect_usfs, 
#           mean, 
#           na.rm = TRUE) %>% 
#   bind_cols((dat_detect_usfs %>% as_tibble %>% select(UID))) %>% 
#   select(-ID) %>% 
#   as_tibble %>% 
#   left_join((dat_detect_usfs %>% as_tibble %>% select(UID)), ., by = "UID") %>% 
#   pivot_longer(cols = starts_with("TCC"),
#                names_prefix = "TCC_",
#                names_to = "Year",
#                values_to = "TCC") %>% 
#   mutate(Year = Year %>% as.numeric) %>% 
#   left_join(dat_detect_usfs_tcc) %>% 
#   mutate(Activity = ifelse(is.na(Activity), 0, Activity)) %>% 
#   group_by(UID) %>% 
#   mutate(TCC_Change = TCC - ifelse(is.na(lag(TCC, n = 2)), lag(TCC), (lag(TCC) + lag(TCC, n = 2)) / 2)) %>% 
#   ungroup %>% 
#   drop_na(TCC_Change)

#  Get NDVI for public timber sales.

#   Set up public timber sales in a convenient format for NDVI extraction.

dat_detect_usfs_ndvi =
  dat_detect_usfs %>%
  as_tibble %>%
  mutate(Year_Before = DATE_AWARDED %>% year %>% `-` (1),
         Year_Complete = DATE_COMPLETED %>% year,
         Years = map2(Year_Before, Year_Complete, ~ seq(.x, .y))) %>%
  filter(Year_Before %in% 2014:2024 & Year_Complete %in% 2014:2024) %>%
  select(UID, Years) %>%
  unnest(Years)

#   Handle NDVI.

dat_detect_ndvi =
  "output/data_ndvi_annual.tif" %>%
  rast %>%
  extract(.,
          dat_detect_usfs,
          mean,
          na.rm = TRUE) %>%
  bind_cols((dat_detect_usfs %>% as_tibble %>% select(UID))) %>%
  select(-ID) %>%
  as_tibble %>%
  left_join((dat_detect_usfs %>% as_tibble %>% select(UID)), ., by = "UID") %>%
  pivot_longer(cols = starts_with("NDVI"),
               names_prefix = "NDVI_",
               names_to = "Year",
               values_to = "NDVI") %>%
  mutate(Year = Year %>% as.numeric) %>%
  semi_join(dat_detect_usfs_ndvi, by = c("UID", "Year" = "Years")) %>%
  group_by(UID) %>%
  mutate(NDVI_Change = NDVI - lag(NDVI),
         NDVI_Detect = ifelse(NDVI_Change == min(NDVI_Change, na.rm = TRUE) & min(NDVI_Change, na.rm = TRUE) < 0, 1, 0)) %>%
  drop_na(NDVI_Change) %>%
  ungroup

#  Compare NDVI and TCC.

# dat_detect_tcc_ndvi = 
#   dat_detect_ndvi %>% 
#   full_join(dat_detect_tcc, by = c("UID", "Year"))
# 
# val_detect_tcc_ndvi_agreement = 
#   dat_detect_tcc_ndvi %>% 
#   mutate(Agreement = NDVI_Detect * TCC_Detect) %>% 
#   summarize(Check = sum(Agreement) / n_distinct(UID)) %>% 
#   pull(Check)

#  Parameterize a model for harvest detection.

#  Detect harvests in notifications.

#   Get notifications with change in TCC and NDVI in convenient formats.

# dat_detect_notifications_tcc = dat_join_tcc
# dat_detect_notifications_ndvi = dat_join_ndvi_annual

#  Set up detection for joins.

#   Placeholder!

dat_join_detect = 
  dat_join_ndvi_annual %>% 
  mutate(NDVI_Detect = ifelse(NDVI_Change < 0, 1, 0)) %>% 
  left_join(dat_notifications_less_1 %>% as_tibble,
            .)
  
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

# Public Lands

dat_join_pad_public = 
  "data/PADUS4_1_State_OR_GDB_KMZ/PADUS4_1_StateOR.gdb" %>% 
  vect %>% 
  filter(Own_Type != "PVT") %>% 
  select(Shape_Area) %>% 
  aggregate %>% 
  project("EPSG:2992") %>% 
  crop(dat_bounds) %>% 
  intersect(dat_notifications_less_1, .) %>% 
  select(UID) %>% 
  as_tibble %>% 
  mutate(PAD_Public = 1) %>% 
  left_join(dat_notifications_less_1 %>% as_tibble, .) %>% 
  mutate(PAD_Public = PAD_Public %>% replace_na(0))

# Protected Areas

dat_join_pad_protected = 
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
  mutate(PAD_Protected = 1) %>% 
  left_join(dat_notifications_less_1 %>% as_tibble, .) %>% 
  mutate(PAD_Protected = PAD_Protected %>% replace_na(0))

# Riparian Zones and Slopes

dat_fpa_1 =
  "data/FPA/Hydrography_Flow_Line.geojson" %>%
  vect %>% # 5'
  select(OBJECTID,
         FishPresence,
         SSBTStatus,
         DistanceToFish,
         DistanceToSSBT,
         Barrier,
         FPAStreamSize,
         StreamName,
         StreamPermanence,
         StreamTermination,
         StreamWaterUse,
         HUC8Name,
         HUC8) %>%
  crop(dat_bounds %>% project("EPSG:4326")) %>% # 112'
  project("EPSG:2992") %>%
  intersect(dat_notifications_less_1, .) %>%
  select(UID) %>%
  as_tibble %>%
  mutate(FPA_1 = 1) %>%
  left_join(dat_notifications_less_1, .) %>% 
  distinct %>% 
  mutate(FPA_1 = FPA_1 %>% replace_na(0))

dat_fpa_2 =
  "data/FPA/Topography_Designated_Sediment_Source_Area.gdb" %>%
  vect %>%
  select(TriggerSource) %>% 
  crop(dat_bounds %>% project("EPSG:3857")) %>%
  project("EPSG:2992") %>%
  intersect(dat_notifications_less_1, .) %>%
  select(UID) %>%
  as_tibble %>%
  mutate(FPA_2 = 1) %>%
  left_join(dat_notifications_less_1 %>% as_tibble, .) %>%
  distinct %>% 
  mutate(FPA_2 = FPA_2 %>% replace_na(0))

# So, the flow traversal areas are described by points. This is silly.
# They should be lines. Specifically, lines with fewer (than 4000000) vertices.
# So: figure out geospatial operations to turn points into sensible lines.

# dat_fpa_3 =
#   "data/FPA/Topography_Debris_Flow_Traversal_Area.gdb" %>%
#   vect %>%
#   slice_sample(n = 10) %>% # Testing runtimes.
#   crop(dat_bounds %>% project("EPSG:3857")) %>%
#   project("EPSG:2992") %>%
#   intersect(dat_notifications_less_1, .) %>%
#   select(UID) %>%
#   as_tibble %>%
#   mutate(FPA_3 = 1) %>%
#   left_join(dat_notifications_less_1 %>% as_tibble, .) %>%
#   distinct %>%
#   mutate(FPA_3 = FPA_3 %>% replace_na(0))

dat_fpa_4 =
  "data/FPA/Topography_Debris_Flow_Traversal_Subbasin.gdb" %>%
  vect %>%
  crop(dat_bounds %>% project("EPSG:3857")) %>%
  project("EPSG:2992") %>%
  intersect(dat_notifications_less_1, .) %>%
  select(UID) %>%
  as_tibble %>%
  mutate(FPA_4 = 1) %>%
  left_join(dat_notifications_less_1 %>% as_tibble, .) %>%
  distinct %>% 
  mutate(FPA_4 = FPA_4 %>% replace_na(0))

dat_join_fpa =
  dat_fpa_1 %>%
  left_join(dat_fpa_2) %>%
  # left_join(dat_fpa_3) %>%
  left_join(dat_fpa_4) %>% 
  as_tibble

# Watersheds

dat_huc8 = 
  "data/HUC8/Oregon_Subbasins%3A_8_Digit_Hydrologic_Units_(2024).shp" %>% 
  vect %>% 
  select(Watershed = name) %>% 
  project("EPSG:2992")

dat_join_huc8 = 
  dat_notifications_less_1 %>% 
  intersect(dat_huc8) %>% 
  as_tibble %>% 
  group_by(UID) %>% 
  filter(row_number() == 1) %>% # Keep only the first watershed intersecting a notification. This is arbitrary.
  ungroup %>% 
  as_tibble

# Counties

dat_counties = 
  "data/TIGER.gdb" %>% 
  vect(layer = "County") %>% 
  select(County = NAMELSAD) %>% 
  project("EPSG:2992")

dat_join_counties = 
  dat_notifications_less_1 %>% 
  intersect(dat_counties) %>% 
  as_tibble %>% 
  group_by(UID) %>% 
  filter(row_number() == 1) %>% # Keep only the first county intersecting a notification. This is arbitrary.
  ungroup %>% 
  as_tibble

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

# Effective Federal Funds Rate

dat_join_rate = 
  "data/Fed/FEDFUNDS.csv" %>% 
  read_csv %>% 
  mutate(Year = observation_date %>% year,
         FEDFUNDS = FEDFUNDS / 100) %>% 
  group_by(Year) %>% 
  summarize(Fed_Rate = FEDFUNDS %>% mean) %>% 
  ungroup %>% 
  left_join(dat_notifications_less_2 %>% as_tibble,
            .,
            by = c("Year_Start" = "Year"))

# Export

dat_notifications_less_1 %T>% writeVector("output/dat_notifications_more_polygons.gdb", overwrite = TRUE)

dat_notifications_out = 
  dat_notifications %>% 
  as_tibble %>% 
  left_join(dat_join_owner) %>% 
  left_join(dat_join_scale) %>% 
  left_join(dat_join_intersect_binary) %>% 
  left_join(dat_join_intersect_proportional) %>% 
  left_join(dat_join_elevation) %>% 
  left_join(dat_join_slope) %>% 
  left_join(dat_join_roughness) %>% 
  left_join(dat_join_pyrome) %>% 
  left_join(dat_join_vpd) %>% 
  left_join(dat_join_mtbs) %>% 
  left_join(dat_join_treemap) %>% 
  left_join(dat_join_tcc) %>% 
  left_join(dat_join_ndvi_annual) %>% 
  left_join(dat_join_detect) %>% 
  left_join(dat_join_distance) %>% 
  left_join(dat_join_pad_public) %>% 
  left_join(dat_join_pad_protected) %>% 
  # left_join(dat_join_fpa) %>% 
  left_join(dat_join_huc8) %>%
  left_join(dat_join_counties) %>% 
  left_join(dat_join_price) %>% 
  left_join(dat_join_rate) %T>% 
  write_csv("output/dat_notifications_more_annual.csv")

# Stopwatch

time_end = Sys.time()

time_end - time_start
