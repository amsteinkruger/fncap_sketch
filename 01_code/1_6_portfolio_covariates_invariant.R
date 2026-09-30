# Join time-invariant covariates to land portfolios.

#  Clear the environment.

rm(list = ls())

#  Start timing. 

time_start = Sys.time()

#  Set up futures.

par_cores = 16

plan(multisession, workers = par_cores)

#  TOC:

#    Area
#    Elevation
#    Slope
#    Flow Lines (Skipping)
#    Steep Slopes (Skipping)
#    Pyromes
#    ODF Private Forest Districts
#    Counties
#    Mill Distance
#    Road Distance
#    City Distance

#  Parcels

dat_parcels = 
  "03_intermediate/dat_portfolios.gdb" %>% 
  vect %>% 
  distinct(Parcel, Area) %T>% 
  writeVector("03_intermediate/dat_portfolios_distinct.gdb")

#  Bounds

dat_bounds = "03_intermediate/dat_bounds.gdb" %>% vect

#  Elevation

dat_elevation = 
  "02_data/1_6_1_USGS_Elevation/Elevation.tif" %>% 
  rast %>% 
  crop(dat_bounds %>% project("EPSG:4269"), mask = TRUE) %>% 
  mutate(Elevation = Elevation * 3.2808399) %>% # Meters to feet for consistency with the CRS.
  project("EPSG:2992") %T>% 
  writeRaster("03_intermediate/dat_elevation.tif")

dat_parcels_elevation = 
  tibble(Chunk = 1:par_cores) %>% 
  mutate(
    Data_Parcels = 
      Chunk %>% 
      future_map(
        ~ "03_intermediate/dat_portfolios_distinct.gdb" %>% 
          vect %>% 
          mutate(Chunk = (row_number() * par_cores / nrow(.)) %>% ceiling) %>%
          filter(Chunk == .x) %>% 
          select(Parcel) %>% 
          extract(
            x = "03_intermediate/dat_elevation.tif" %>% rast,
            y = .,
            fun = mean,
            ID = FALSE,
            bind = TRUE
          ) %>%
          as_tibble, 
        .options = furrr_options(seed = TRUE)
      )
  ) %>% 
  unnest(Data_Parcels) %>% 
  select(-Chunk)

# Slope
  
dat_slope = 
  dat_elevation %>% 
  terrain(v = "slope") %T>% 
  writeRaster("03_intermediate/dat_slope.tif")

dat_parcels_slope = 
  tibble(Chunk = 1:par_cores) %>% 
  mutate(
    Data_Parcels = 
      Chunk %>% 
      future_map(
        ~ "03_intermediate/dat_portfolios_distinct.gdb" %>% 
          vect %>% 
          mutate(Chunk = (row_number() * par_cores / nrow(.)) %>% ceiling) %>%
          filter(Chunk == .x) %>% 
          select(Parcel) %>% 
          extract(
            x = "03_intermediate/dat_slope.tif" %>% rast,
            y = .,
            fun = mean,
            ID = FALSE,
            bind = TRUE
          ) %>%
          as_tibble, 
        .options = furrr_options(seed = TRUE)
      )
  ) %>% 
  unnest(Data_Parcels) %>% 
  select(-Chunk) %>% 
  rename(Slope = slope)

# Riparian Zones and Slopes

#  Flow Lines

# dat_join_riparian =
#   # "02_data/1_6_3_FPA/Hydrography_Flow_Line.geojson" %>%
#   "02_data/1_6_3_FPA/Hydrography_Flow_Line.gdb" %>%
#   vect %>% # 5'
#   select(FPAStreamSize,
#          StreamName,
#          StreamPermanence,
#          StreamTermination,
#          StreamWaterUse,
#          FishPresence,
#          SSBTStatus,
#          DistanceToFish,
#          DistanceToSSBT) %>%
#   crop(dat_bounds) %>% 
#   # This is where you could split the spatvector by stream attributes for different buffer widths. 
#   buffer(20) %>% 
#   # Find intersections. 
#   intersect(dat_notifications, .) %>%
#   # Combine intersections. This is important because intersections can overlap. 
#   group_by(UID, Acres_1) %>%
#   summarize %>%
#   ungroup %>%
#   # Calculate areas of intersections. 
#   mutate(Acres_Riparian = expanse(., unit = "ha") * 2.47105381) %>% 
#   as_tibble %>% 
#   # Calculate proportional areas of intersections.
#   mutate(Acres_Riparian_Proportion = Acres_Riparian / Acres_1) %>% 
#   # Correct for one oddball case.
#   mutate(Acres_Riparian_Proportion = ifelse(Acres_Riparian_Proportion > 1, 1, Acres_Riparian_Proportion)) %>% 
#   # Tag on to all notifications and add a binary variable.
#   left_join(dat_notifications_less, .) %>% 
#   mutate(Acres_Riparian = Acres_Riparian %>% replace_na(0),
#          Acres_Riparian_Proportion = Acres_Riparian_Proportion %>% replace_na(0),
#          Acres_Riparian_Binary = ifelse(Acres_Riparian > 0, 1, 0)) %>% 
#   # Tidy up.
#   as_tibble %>% 
#   select(-Acres_1)

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
  project("EPSG:2992") %T>% 
  writeVector("03_intermediate/dat_pyromes.gdb")

dat_parcels_pyromes = 
  tibble(Chunk = 1:par_cores) %>% 
  mutate(
    Data_Parcels = 
      Chunk %>% 
      future_map(
        ~ "03_intermediate/dat_portfolios_distinct.gdb" %>% 
          vect %>% 
          mutate(Chunk = (row_number() * par_cores / nrow(.)) %>% ceiling) %>%
          filter(Chunk == .x) %>% 
          select(Parcel) %>% 
          centroids %>% 
          intersect("03_intermediate/dat_pyromes.gdb" %>% vect) %>%
          as_tibble, 
        .options = furrr_options(seed = TRUE)
      )
  ) %>% 
  unnest(Data_Parcels) %>% 
  select(-Chunk)

#  ODF Private Forest Districts

dat_districts = 
  "02_data/1_6_7_ODF_Districts/District_Boundaries.geojson" %>%
  vect %>%
  select(District = pf_dist) %>%
  project("EPSG:2992") %>%
  makeValid(buffer = TRUE) %>%
  crop(dat_bounds) %T>% 
  writeVector("03_intermediate/dat_districts.gdb")

dat_parcels_districts = 
  tibble(Chunk = 1:par_cores) %>% 
  mutate(
    Data_Parcels = 
      Chunk %>% 
      future_map(
        ~ "03_intermediate/dat_portfolios_distinct.gdb" %>% 
          vect %>% 
          mutate(Chunk = (row_number() * par_cores / nrow(.)) %>% ceiling) %>%
          filter(Chunk == .x) %>% 
          select(Parcel) %>% 
          centroids %>% 
          intersect("03_intermediate/dat_districts.gdb" %>% vect) %>%
          as_tibble, 
        .options = furrr_options(seed = TRUE)
      )
  ) %>% 
  unnest(Data_Parcels) %>% 
  select(-Chunk)

#  Counties

dat_counties = 
  "02_data/1_6_6_TIGER/TIGER.gdb" %>% 
  vect(layer = "County") %>% 
  select(County = NAMELSAD) %>% 
  project("EPSG:2992") %T>% 
  writeVector("03_intermediate/dat_counties.gdb")

dat_parcels_counties = 
  tibble(Chunk = 1:par_cores) %>% 
  mutate(
    Data_Parcels = 
      Chunk %>% 
      future_map(
        ~ "03_intermediate/dat_portfolios_distinct.gdb" %>% 
          vect %>% 
          mutate(Chunk = (row_number() * par_cores / nrow(.)) %>% ceiling) %>%
          filter(Chunk == .x) %>% 
          select(Parcel) %>% 
          centroids %>% 
          intersect("03_intermediate/dat_counties.gdb" %>% vect) %>%
          as_tibble, 
        .options = furrr_options(seed = TRUE)
      )
  ) %>% 
  unnest(Data_Parcels) %>% 
  select(-Chunk)

#  Distances

#   Mills

dat_mills = 
  "02_data/1_6_4_USFS_Mills/Mills_MS_20250916.xlsx" %>% 
  read_xlsx %>% 
  vect(geom = c("Long", "Lat"),
       crs = "EPSG:4326") %>% # This could be wrong!
  project("EPSG:2992") %>% 
  crop(dat_bounds) %T>% 
  writeVector("03_intermediate/dat_mills.gdb")

dat_parcels_mills = 
  tibble(Chunk = 1:par_cores) %>% 
  mutate(
    Data_Parcels = 
      Chunk %>% 
      future_map(
        ~ "03_intermediate/dat_portfolios_distinct.gdb" %>% 
          vect %>% 
          mutate(Chunk = (row_number() * par_cores / nrow(.)) %>% ceiling) %>%
          filter(Chunk == .x) %>% 
          select(Parcel) %>% 
          centroids %>% 
          nearest("03_intermediate/dat_mills.gdb" %>% vect) %>%
          as_tibble %>% 
          mutate(Distance_Mill = distance / 5280, .keep = "none"), 
        .options = furrr_options(seed = TRUE)
      )
  ) %>% 
  unnest(Data_Parcels) %>% 
  bind_cols(dat_parcels %>% as_tibble, .) %>% 
  select(-Chunk)

#   Roads

dat_roads = 
  "02_data/1_6_5_ODT_Roads/All_Public_Roads.geojson" %>% 
  vect %>% 
  select(OBJECTID) %>% 
  crop(dat_bounds %>% project("EPSG:4326")) %>% 
  project("EPSG:2992") %T>% 
  writeVector("03_intermediate/dat_roads.gdb")

dat_parcels_roads = 
  tibble(Chunk = 1:par_cores) %>% 
  mutate(
    Data_Parcels = 
      Chunk %>% 
      future_map(
        ~ "03_intermediate/dat_portfolios_distinct.gdb" %>% 
          vect %>% 
          mutate(Chunk = (row_number() * par_cores / nrow(.)) %>% ceiling) %>%
          filter(Chunk == .x) %>% 
          select(Parcel) %>% 
          centroids %>% 
          nearest("03_intermediate/dat_roads.gdb" %>% vect) %>%
          as_tibble %>% 
          mutate(Distance_Road = distance / 5280, .keep = "none"), 
        .options = furrr_options(seed = TRUE)
      )
  ) %>% 
  unnest(Data_Parcels) %>% 
  bind_cols(dat_parcels %>% as_tibble, .) %>% 
  select(-Chunk)

#   Cities

dat_cities = 
  rbind(
    "02_data/1_6_6_TIGER/TIGER.gdb" %>% vect(layer = "Incorporated_Place"),
    "02_data/1_6_6_TIGER/TIGER.gdb" %>% vect(layer = "Census_Designated_Place")
  ) %>% 
  crop(dat_bounds %>% project("EPSG:4269")) %>% 
  centroids %>% 
  project("EPSG:2992") %T>% 
  writeVector("03_intermediate/dat_cities.gdb")

dat_parcels_cities = 
  tibble(Chunk = 1:par_cores) %>% 
  mutate(
    Data_Parcels = 
      Chunk %>% 
      future_map(
        ~ "03_intermediate/dat_portfolios_distinct.gdb" %>% 
          vect %>% 
          mutate(Chunk = (row_number() * par_cores / nrow(.)) %>% ceiling) %>%
          filter(Chunk == .x) %>% 
          select(Parcel) %>% 
          centroids %>% 
          nearest("03_intermediate/dat_cities.gdb" %>% vect) %>%
          as_tibble %>% 
          mutate(Distance_City = distance / 5280, .keep = "none"), 
        .options = furrr_options(seed = TRUE)
      )
  ) %>% 
  unnest(Data_Parcels) %>% 
  bind_cols(dat_parcels %>% as_tibble, .) %>% 
  select(-Chunk)

#  Join to portfolios in a panel and export. 

dat_parcels_covariates = 
  dat_parcels %>% 
  as_tibble %>% 
  left_join(dat_parcels_elevation) %>% 
  left_join(dat_parcels_slope) %>% 
  left_join(dat_parcels_pyromes) %>% 
  left_join(dat_parcels_districts) %>% 
  left_join(dat_parcels_counties) %>% 
  left_join(dat_parcels_roads) %>% 
  left_join(dat_parcels_mills) %>% 
  left_join(dat_parcels_cities)

dat_portfolios = "03_intermediate/dat_portfolios.gdb" %>% vect %>% as_tibble

dat_portfolios_covariates = 
  dat_portfolios %>% 
  # filter(Year_Quarter %in% c("2016_1", "2016_2", "2016_3", "2016_4")) %>% 
  left_join(dat_parcels_covariates) %>% 
  pivot_longer(
    cols = c(Pyrome, District, County), 
    names_to = "Region_Type",
    values_to = "Region_Name"
  ) %>% 
  mutate(Region_Name = Region_Name %>% str_replace_all(" ", "_")) %>% 
  group_by(Owner_Cotality, Year_Quarter, Region_Type, Region_Name) %>% 
  summarize(
    across(c(Elevation, Slope, starts_with("Distance")), ~ weighted.mean(.x, Area, na.rm = TRUE)),
    Area = sum(Area, na.rm = TRUE)
  ) %>% 
  group_by(Owner_Cotality, Year_Quarter, Region_Type) %>% 
  mutate(Area_Proportion = Area / sum(Area, na.rm = TRUE)) %>% 
  ungroup %>% 
  arrange(desc(Region_Type), Region_Name, Owner_Cotality, Year_Quarter) %>% 
  pivot_wider(
    names_from = c(Region_Type, Region_Name),
    names_glue = "{Region_Type}_{Region_Name}_{.value}",
    values_from = starts_with("Area"),
    values_fill = 0
  ) %>% 
  mutate(
    Area = 
      Pyrome_Marine_Northwest_Coast_Forest_Area +
      Pyrome_Klamath_Mountains_Area + 
      Pyrome_Middle_Cascades_Area +
      Pyrome_NA_Area
  ) %>% 
  relocate(Area, .after = "Year_Quarter") %>% 
  relocate(starts_with("County") & ends_with("Proportion"), .after = "Distance_City") %>% 
  relocate(starts_with("County") & ends_with("Area"), .after = "Distance_City") %>% 
  relocate(starts_with("District") & ends_with("Proportion"), .after = "Distance_City") %>% 
  relocate(starts_with("District") & ends_with("Area"), .after = "Distance_City") %>% 
  relocate(starts_with("Pyrome") & ends_with("Proportion"), .after = "Distance_City") %>% 
  relocate(starts_with("Pyrome") & ends_with("Area"), .after = "Distance_City") %T>% 
  write_csv("03_intermediate/dat_portfolios_covariates_invariant.csv")
  
#  Stop timing. 

time_end = Sys.time()

time_end - time_start
