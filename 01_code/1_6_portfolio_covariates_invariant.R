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
  writeRaster("03_intermediate/dat_elevation.tif", overwrite = TRUE)

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
  writeRaster("03_intermediate/dat_slope.tif", overwrite = TRUE)

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
  relocate(Area, .after = "Year_Quarter") %T>% 
  write_csv("03_intermediate/dat_portfolios_covariates_invariant.csv") %>% 
  select(-starts_with(c("Pyrome", "District", "County"))) %T>% 
  write_csv("03_intermediate/dat_portfolios_covariates_invariant_less.csv")
  
#  Stop timing. 

time_end = Sys.time()

time_end - time_start
