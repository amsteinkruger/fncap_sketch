# Join time-invariant covariates to notifications.

#  Clear the environment.

rm(list = ls())

#  Start timing. 

time_start = Sys.time()

#  TOC:

#    Growth
#    Elevation
#    Slope
#    Roughness
#    Flow Lines
#    Steep Slopes
#    Pyromes
#    ODF Private Forest Districts
#    Counties
#    Distances

#  Notifications

dat_notifications = 
  "03_intermediate/dat_notifications_1_5.gdb" %>% 
  vect %>% 
  makeValid(buffer = TRUE)

dat_notifications_less = 
  dat_notifications %>% 
  select(UID)

#  Bounds

dat_bounds = "03_intermediate/dat_bounds.gdb" %>% vect

#  Growth

#  Pull growth work into an earlier script (0_*), then get parameters in here. 

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

#  Pyromes

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

#  ODF Private Forest Districts

dat_districts = 
  "02_data/1_6_7_ODF_Districts/District_Boundaries.geojson" %>%
  vect %>%
  select(District = pf_dist) %>%
  project("EPSG:2992") %>%
  makeValid(buffer = TRUE) %>%
  crop(dat_bounds)
  
dat_join_districts = 
  dat_notifications_less %>% 
  centroids(inside = TRUE) %>% 
  intersect(dat_districts) %>% 
  as_tibble

#  Counties

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

#  Distances

#   Mills

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

#   Roads

dat_roads = 
  "02_data/1_6_5_ODT_Roads/All_Public_Roads.geojson" %>% 
  vect %>% 
  crop(dat_bounds %>% project("EPSG:4326")) %>% 
  project("EPSG:2992")

dat_join_roads = 
  dat_notifications_less %>% 
  centroids %>% 
  nearest(dat_roads) %>% 
  as_tibble %>% 
  mutate(Distance_Road = distance / 5280,
         .keep = "none") %>% 
  bind_cols(dat_notifications_less %>% as_tibble, .)

#   Cities

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

#   Combined

dat_join_distance = 
  dat_join_roads %>% 
  left_join(dat_join_mills) %>% 
  left_join(dat_join_cities)

#  Export

dat_notifications_out = 
  dat_notifications %>% 
  # Growth
  left_join(dat_join_elevation) %>% 
  left_join(dat_join_slope) %>% 
  left_join(dat_join_roughness) %>% 
  # FPA
  left_join(dat_join_pyrome) %>% 
  left_join(dat_join_districts) %>%
  left_join(dat_join_counties) %>% 
  left_join(dat_join_distance) %T>% 
  # Export with spatial data. 
  writeVector("03_intermediate/dat_notifications_1_6.gdb") %>% 
  # Export without spatial data. 
  as_tibble %T>% 
  write_csv("03_intermediate/dat_notifications_1_6.csv")

#  Stop timing. 

time_end = Sys.time()

time_end - time_start
