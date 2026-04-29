# Join covariates to notifications.

#  Clear the environment.

rm(list = ls())

#  Start timing. 

time_start = Sys.time()

#  TOC:

#   Natural Features
#    Elevation
#    Slope
#    Roughness
#    Pyromes
#   Climate, Weather, and Fire
#    MTBS
#    VPD
#    other climate, weather, fire
#   Other?
#    Growth *
#    Flow Lines
#    Steep Slopes *
#   Social Features
#    Distances
#    Counties
#    Prices
#    Effective Federal Funds Rate

#  Notifications

dat_notifications = 
  "03_intermediate/dat_notifications_1_5.gdb" %>% 
  vect

dat_notifications_less = 
  dat_notifications %>% 
  select(UID)

dat_notifications_years = 
  dat_notifications %>% 
  mutate(Year = DateStart %>% year) %>% 
  select(UID, Year)

#  Bounds

dat_bounds = "03_intermediate/dat_bounds.gdb" %>% vect

#  Elevation

dat_elevation = 
  "02_data/1_6_1_USGS_Elevation/Elevation.tif" %>% 
  rast %>% 
  crop(dat_bounds %>% project("EPSG:4269"),
       mask = TRUE) %>% 
  mutate(Elevation = Elevation * 3.2808399) %>% # Meters to feet for consistency with the CRS.
  project("EPSG:2992")

dat_join_elevation =
  dat_notifications_less %>%
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
  dat_notifications_less %>%
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
  dat_notifications_less %>% 
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
  "02_data/1_2_2_USFS_Pyromes/Data/Pyromes_CONUS_20200206.shp" %>% 
  vect %>% 
  rename(WHICH = NAME) %>% # Band-Aid for a reserved attribute name.
  filter(WHICH %in% c("Marine Northwest Coast Forest", "Klamath Mountains", "Middle Cascades")) %>% 
  select(Pyrome = WHICH) %>% 
  project("EPSG:2992")

dat_join_pyrome = 
  dat_notifications_less %>% 
  centroids(inside = TRUE) %>% 
  intersect(dat_pyrome) %>% 
  as_tibble

# MTBS

dat_mtbs = 
  "02_data/1_6_2_MTBS/Perimeters" %>% 
  vect %>% 
  project("EPSG:2992") %>% 
  makeValid %>% 
  crop(dat_bounds) %>% 
  mutate(Year_MTBS = ig_date %>% year, 
         .keep = "none")

# 1. No Buffer

dat_join_mtbs_0 = 
  dat_notifications_years %>% 
  intersect(dat_mtbs) %>% 
  as_tibble %>% 
  filter(Year > Year_MTBS & Year - 30 < Year_MTBS) %>% 
  group_by(UID) %>% 
  summarize(Fire_0 = n()) %>% 
  ungroup

# 2. 15km Buffer

dat_join_mtbs_15 = 
  dat_notifications_years %>% 
  buffer(width = 15 * 3280.84) %>% # Kilometers to feet.
  intersect(dat_mtbs) %>% 
  as_tibble %>% 
  filter(Year > Year_MTBS & Year - 30 < Year_MTBS) %>% 
  group_by(UID) %>% 
  summarize(Fire_15 = n()) %>% 
  ungroup

# 3. 30km Buffer

dat_join_mtbs_30 = 
  dat_notifications_years %>% 
  buffer(width = 30 * 3280.84) %>% # Kilometers to feet.
  intersect(dat_mtbs) %>% 
  as_tibble %>% 
  filter(Year >= Year_MTBS & Year - 30 < Year_MTBS) %>% 
  group_by(UID) %>% 
  summarize(Fire_30 = n()) %>% 
  ungroup

# 4. Combine

dat_join_mtbs = 
  dat_notifications_less %>% 
  as_tibble %>% 
  left_join(dat_join_mtbs_0) %>% 
  left_join(dat_join_mtbs_15) %>% 
  left_join(dat_join_mtbs_30) %>% 
  mutate(Fire_0 = Fire_0 %>% replace_na(0),
         Fire_15 = Fire_15 %>% replace_na(0),
         Fire_30 = Fire_30 %>% replace_na(0),
         Fire_15_Doughnut = Fire_15 - Fire_0,
         Fire_30_Doughnut = Fire_30 - Fire_15)

# VPD

dat_vpd = "03_intermediate/dat_vpd.tif" %>% rast

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
  tibble(year = 2014:2025) %>% 
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
          dat_notifications_less, 
          mean, 
          na.rm = TRUE) %>% 
  bind_cols((dat_notifications_less$UID %>% tibble(UID = .))) %>% 
  select(-ID) %>% 
  as_tibble %>% 
  left_join((dat_notifications_less %>% as_tibble), ., by = "UID") %>% 
  pivot_longer(cols = starts_with("VPD"),
               names_prefix = "VPD_",
               names_to = "Year",
               values_to = "VPD") %>% 
  group_by(UID) %>% 
  nest(data = c(Year, VPD)) %>% 
  ungroup %>% 
  left_join(dat_notifications %>% 
              as_tibble %>% 
              mutate(Year = DateStart %>% year) %>% 
              select(UID, Year), 
            .) %>% 
  mutate(data = data %>% map2(.x = ., .y = Year, .f = ~ filter(.x, Year == .y))) %>% 
  select(-Year) %>% 
  unnest(data) %>% 
  select(UID, VPD)

# Growth

#  Pull growth work into an earlier script (0_*), then get parameters in here. 

# Riparian Zones and Slopes

#  Flow Lines

# dat_fpa_1 =
#   "02_data/1_6_3_FPA/Hydrography_Flow_Line.geojson" %>%
#   vect %>% # 5'
#   select(OBJECTID,
#          FishPresence,
#          SSBTStatus,
#          DistanceToFish,
#          DistanceToSSBT,
#          Barrier,
#          FPAStreamSize,
#          StreamName,
#          StreamPermanence,
#          StreamTermination,
#          StreamWaterUse,
#          HUC8Name,
#          HUC8) %>%
#   crop(dat_bounds %>% project("EPSG:4326")) %>% # 112'
#   project("EPSG:2992") %>%
#   intersect(dat_notifications_less, .) %>%
#   select(UID) %>%
#   as_tibble %>%
#   mutate(FPA_1 = 1) %>%
#   left_join(dat_notifications_less, .) %>% 
#   distinct %>% 
#   mutate(FPA_1 = FPA_1 %>% replace_na(0))

#  Sediment Source Areas

# dat_fpa_2 =
#   "02_data/1_6_3_FPA/Topography_Designated_Sediment_Source_Area.gdb" %>%
#   vect %>%
#   select(TriggerSource) %>% 
#   crop(dat_bounds %>% project("EPSG:3857")) %>%
#   project("EPSG:2992") %>%
#   intersect(dat_notifications_less_1, .) %>%
#   select(UID) %>%
#   as_tibble %>%
#   mutate(FPA_2 = 1) %>%
#   left_join(dat_notifications_less_1 %>% as_tibble, .) %>%
#   distinct %>% 
#   mutate(FPA_2 = FPA_2 %>% replace_na(0))

#  Flow Traversal Areas

# So, the flow traversal areas are described by points. This is silly.
# They should be lines. Specifically, lines with fewer (than 4000000) vertices.
# So: figure out geospatial operations to turn points into sensible lines.

# dat_fpa_3 =
#   "02_data/1_6_3_FPA/Topography_Debris_Flow_Traversal_Area.gdb" %>%
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

#  Flow Traversal Subbasins

# dat_fpa_4 =
#   "02_data/1_6_3_FPA/opography_Debris_Flow_Traversal_Subbasin.gdb" %>%
#   vect %>%
#   crop(dat_bounds %>% project("EPSG:3857")) %>%
#   project("EPSG:2992") %>%
#   intersect(dat_notifications_less_1, .) %>%
#   select(UID) %>%
#   as_tibble %>%
#   mutate(FPA_4 = 1) %>%
#   left_join(dat_notifications_less_1 %>% as_tibble, .) %>%
#   distinct %>% 
#   mutate(FPA_4 = FPA_4 %>% replace_na(0))

# dat_join_fpa =
#   dat_fpa_1 %>%
#   left_join(dat_fpa_2) %>%
#   left_join(dat_fpa_3) %>%
#   left_join(dat_fpa_4) %>% 
#   as_tibble

# Distances

#  Mills

dat_mills = 
  "02_data/1_6_4_USFS_Mills/Mills_MS_20250916.xlsx" %>% 
  read_xlsx %>% 
  vect(geom = c("Long", "Lat"),
       crs = "EPSG:4326") %>% # This could be wrong!
  project("EPSG:2992") %>% 
  crop(dat_bounds)

dat_join_mills = 
  dat_notifications_less %>% 
  centroids %>% 
  distance(dat_mills) %>% 
  as.data.frame %>% 
  bind_cols(dat_notifications_less %>% as_tibble, .) %>% 
  pivot_longer(cols = starts_with("V")) %>% 
  group_by(UID) %>% 
  filter(value == min(value)) %>% 
  ungroup %>% 
  distinct(UID, value) %>% 
  mutate(value = value / 5280) %>% 
  select(UID, Distance_Mill = value)

#  Roads

dat_roads = 
  "02_data/1_6_5_NAR_Roads/NAR.gdb" %>% 
  vect %>% 
  crop(dat_bounds %>% project("EPSG:4326")) %>% 
  project("EPSG:2992")

dat_join_roads = 
  dat_notifications_less %>% 
  centroids %>% 
  distance(dat_roads) %>% 
  as.data.frame %>% 
  bind_cols(dat_notifications_less %>% as_tibble, .) %>% 
  pivot_longer(cols = starts_with("V")) %>% 
  group_by(UID) %>% 
  filter(value == min(value)) %>% 
  ungroup %>% 
  distinct(UID, value) %>% 
  mutate(value = value / 5280) %>% 
  select(UID, Distance_Road = value)

#  Cities

dat_cities = 
  "02_data/1_6_6_TIGER/TIGER.gdb" %>% 
  vect %>% 
  crop(dat_bounds %>% project("EPSG:4269")) %>% 
  project("EPSG:2992")

dat_join_cities = 
  dat_notifications_less %>% 
  centroids %>% # Check whether INTPTLAT, INTPTLON are more informative than centroids.
  distance(dat_cities) %>% 
  as.data.frame %>% 
  bind_cols(dat_notifications_less %>% as_tibble, .) %>% 
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

# Counties

dat_counties = 
  "02_data/1_6_6_TIGER/TIGER.gdb" %>% 
  vect(layer = "County") %>% 
  select(County = NAMELSAD) %>% 
  project("EPSG:2992")

dat_join_counties = 
  dat_notifications_less %>% 
  centroids(inside = TRUE) %>% 
  intersect(dat_counties) %>% 
  as_tibble

# Prices

#  Get notifications by quarter. 

dat_notifications_quarters = 
  dat_notifications %>% 
  mutate(Year_Quarter = 
           paste0(DateStart %>% 
                    year, 
                  "_Q", 
                  DateStart %>% 
                    month %>% 
                    multiply_by(1 / 3) %>% 
                    ceiling)) %>% 
  select(UID, Year_Quarter) %>% 
  as_tibble

#  Indexes

#   PPI (Timber)

dat_ppi_timber = 
  "02_data/1_6_7_BLS/data_ppi_timber.csv" %>% 
  read_csv %>% 
  mutate(Year = observation_date %>% year,
         Month = observation_date %>% month,
         Quarter = Month %>% multiply_by(1 / 3) %>% ceiling,
         Year_Quarter = paste0(Year, "_Q", Quarter)) %>% 
  filter(Year %in% 2015:2025) %>% 
  group_by(Year_Quarter) %>% 
  summarize(PPI_Timber = WPU08510502 %>% mean)
  ungroup %>% 
  mutate(Check = Year_Quarter == max(Year_Quarter),
         Reference = ifelse(Check, PPI_Timber, NA) %>% max(na.rm = TRUE),
         Factor_PPI_Timber = Reference / PPI_Timber) %>% 
  select(Year_Quarter, Factor_PPI_Timber)

#   PPI (Lumber)

# dat_ppi_lumber = 
#   "02_data/1_6_7_BLS/data_ppi_lumber.csv" %>% 
#   read_csv %>% 
#   mutate(Year = observation_date %>% year,
#          Month = observation_date %>% month,
#          Quarter = Month %>% multiply_by(1 / 3) %>% ceiling,
#          Year_Quarter = paste0(Year, "_Q", Quarter)) %>% 
#   filter(Year %in% 2015:2025) %>% 
#   group_by(Year_Quarter) %>% 
#   summarize(PPI_Lumber = WPU0811 %>% max) %>% 
#   ungroup %>% 
#   mutate(Check = Year_Quarter == max(Year_Quarter),
#          Reference = ifelse(Check, PPI_Lumber, NA) %>% max(na.rm = TRUE),
#          Factor_PPI_Lumber = Reference / PPI_Lumber) %>% 
#   select(Year_Quarter, Factor_PPI_Lumber)

#   Join.

# dat_ppi = dat_ppi_timber %>% left_join(dat_ppi_lumber)
dat_ppi = dat_ppi_timber

#   Prices

#    Stumpage, LogLines/FastMarkets

dat_price_stumpage = 
  "02_data/1_6_8_FastMarkets/data_stumpage.csv" %>% 
  read_csv %>% 
  rename(Stumpage_Nominal = 2) %>% 
  mutate(Year_Quarter = 
           paste0(str_sub(Quarter, 1, 4), 
                  "_", 
                  str_sub(Quarter, -2, -1))) %>% 
  select(Year_Quarter, Stumpage_Nominal) %>% 
  filter(Year_Quarter > "2014_Q4" & Year_Quarter < "2025_Q1") %>% 
  left_join(dat_ppi) %>% 
  mutate(Stumpage_Real = Stumpage_Nominal * Factor_PPI_Timber)

#   Delivered Logs, FastMarkets

# dat_price_delivered = 
#   "data/Prices_FastMarkets/data_pull_filter.csv" %>% 
#   read_csv %>% 
#   select(1:3) %>% 
#   rename(Delivered_Nominal = 3) %>% 
#   left_join(dat_price_index) %>% 
#   mutate(Delivered_Real_PPI = Delivered_Nominal * Factor_PPI_2024,
#          Delivered_Real_PPI_Timber = Delivered_Nominal * Factor_PPI_Timber_2024) %>% 
#   select(Year, Month, starts_with("Delivered")) %>% 
#   group_by(Year) %>% 
#   summarize(across(starts_with("Delivered"), mean, na.rm = TRUE)) %>% 
#   ungroup

#  Join

dat_join_price = 
  dat_notifications_quarters %>% 
  left_join(dat_price_stumpage) %>% 
  # left_join(dat_price_delivered) %>% 
  select(UID, Stumpage_Nominal, Stumpage_Real)

#  Effective Federal Funds Rate

dat_join_rate = 
  "02_data/1_6_9_FRED/FEDFUNDS.csv" %>% 
  read_csv %>% 
  mutate(Year = observation_date %>% year,
         Month = observation_date %>% month,
         Quarter = Month %>% multiply_by(1 / 3) %>% ceiling,
         Year_Quarter = paste0(Year, "_Q", Quarter),
         Rate_Fed = FEDFUNDS) %>% 
  group_by(Year_Quarter) %>% 
  summarize(Rate_Fed = Rate_Fed %>% mean) %>% 
  ungroup %>% 
  left_join(dat_notifications_quarters, 
            .) %>% 
  select(UID, Rate_Fed)

#   Natural Features
#    Elevation
#    Slope
#    Roughness
#    Pyromes
#   Climate, Weather, and Fire
#    MTBS
#    VPD
#    other climate, weather, fire
#   Other?
#    Growth *
#    Flow Lines
#    Steep Slopes *
#   Social Features
#    Distances
#    Counties
#    Prices
#    Effective Federal Funds Rate

#  Export

dat_notifications_out = 
  dat_notifications %>% 
  left_join(dat_join_elevation) %>% 
  left_join(dat_join_slope) %>% 
  left_join(dat_join_roughness) %>% 
  left_join(dat_join_pyrome) %>% 
  left_join(dat_join_mtbs) %>% 
  left_join(dat_join_vpd) %>% 
  left_join(dat_join_distance) %>% 
  left_join(dat_join_counties) %>% 
  left_join(dat_join_price) %>% 
  left_join(dat_join_rate) %T>% 
  # Export with spatial data. 
  writeVector("03_intermediate/dat_notifications_1_6.gdb") %>% 
  # Export without spatial data. 
  as_tibble %T>% 
  write_csv("03_intermediate/dat_notifications_1_6.csv")

#  Stop timing. 

time_end = Sys.time()

time_end - time_start
