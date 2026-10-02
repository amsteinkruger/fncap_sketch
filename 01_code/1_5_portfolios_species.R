# Join data on forest type to restrict the data to Douglas fir.

#  Clear the environment.

rm(list = ls())

#  Start timing. 

time_start = Sys.time()

#  Set up futures.

par_cores = 16

plan(multisession, workers = par_cores)

#  TOC:

#  Parcels

dat_parcels = "03_intermediate/dat_portfolios.gdb" %>% vect

dat_parcels_less = 
  dat_parcels %>% 
  distinct(Parcel) %T>% 
  writeVector("03_intermediate/dat_portfolios_distinct.gdb")

dat_parcels_more = 
  dat_parcels_less %>% 
  as_tibble %>% 
  mutate(Row_Parcel = row_number()) %T>% 
  write_csv("03_intermediate/dat_portfolios_distinct.csv")

#  Notifications

# dat_notifications = 
#   "03_intermediate/dat_notifications_1_4.gdb" %>% 
#   vect %>% 
#   makeValid(buffer = TRUE) %>%
#   filter(is.valid(.))
# 
# dat_notifications_less = 
#   dat_notifications %>% 
#   select(UID)

# TreeMap

#  Get FIA data.

#   Trees

dat_fia_tree = 
  bind_rows(
    "02_data/1_5_1_FIA/CA_TREE.csv" %>% 
      read_csv,
    "02_data/1_5_1_FIA/OR_TREE.csv" %>% 
      read_csv,
    "02_data/1_5_1_FIA/WA_TREE.csv" %>% 
      read_csv
  ) %>% 
  select(
    CN = PLT_CN, 
    TREE, 
    SPCD,
    VOLBFNET,
    TPA_UNADJ
  ) %>% 
  filter(VOLBFNET > 0) %>% 
  mutate(
    SPCD_USE = 
      case_when(
        SPCD %in% 201:202 ~ "DouglasFir",
        SPCD == 263 ~ "WesternHemlock",
        TRUE ~ "Other"
      ),
    VOLBFNET_ACRE = VOLBFNET * TPA_UNADJ
  ) %>% 
  group_by(CN, SPCD_USE) %>% 
  summarize(
    VOLBFNET_ACRE = VOLBFNET_ACRE %>% sum,
    .groups = "drop_last"
  ) %>% 
  ungroup %>% 
  group_by(CN) %>% 
  mutate(Species_Proportion = VOLBFNET_ACRE / sum(VOLBFNET_ACRE)) %>% 
  ungroup %>% 
  select(-VOLBFNET_ACRE) %>% 
  pivot_wider(
    names_from = SPCD_USE, 
    values_from = Species_Proportion, 
    names_prefix = "Proportion"
  ) %>% 
  mutate(across(starts_with("Proportion"), ~ replace_na(.x, 0)))

#  Conditions

dat_fia_cond = 
  bind_rows(
    "02_data/1_5_1_FIA/CA_COND.csv" %>% read_csv,
    "02_data/1_5_1_FIA/OR_COND.csv" %>% read_csv,
    "02_data/1_5_1_FIA/WA_COND.csv" %>% read_csv
  ) %>% 
  select(
    CN = PLT_CN, 
    FORTYPCD, 
    SITECLCD
  ) %>%
  drop_na %>% 
  distinct %>% 
  group_by(CN) %>% 
  filter(n() == 1) %>% 
  ungroup

#  Join

dat_fia = left_join(dat_fia_cond, dat_fia_tree)

#  Get TreeMap data.

#   Handle initial data.

crs_treemap = 
  "02_data/1_5_2_TreeMap_2014/national_c2014_tree_list.tif" %>% 
  rast %>% 
  crs

dat_bounds_treemap = 
  "03_intermediate/dat_bounds.gdb" %>% 
  vect %>% 
  project(crs_treemap)

dat_treemap = 
  "02_data/1_5_2_TreeMap_2014/national_c2014_tree_list.tif" %>% 
  rast %>% 
  crop(dat_bounds_treemap, mask = TRUE) %>% 
  project("EPSG:2992")

vec_treemap = dat_treemap %>% as.vector %>% na.omit %>% unique

#  Get FIA data by way of TreeMap's look-up table. This avoids a confusing raster operation.

dat_treemap_lookup = 
  "02_data/1_5_2_TreeMap_2014/TL_CN_Lookup.txt" %>% 
  read_delim %>% 
  rename(TL_ID = tl_id) %>% 
  select(TL_ID, CN)

dat_treemap_join =
  vec_treemap %>%
  tibble(TL_ID = .) %>%
  left_join(dat_treemap_lookup) %>% 
  left_join(dat_fia)

#  Reclassify Treemap into 
#   (1) binary forest types (Douglas Fir / Not) 
#   (2) site class (1-7).
#   (3) proportions of Douglas fir (from all sawlog timber by MBF)
#   (4) proportions of western hemlock ("")

dat_treemap_fortypcd = 
  dat_treemap_join %>% 
  mutate(FORTYPCD_BIN = ifelse(FORTYPCD %in% 201:203, 1, ifelse(!is.na(FORTYPCD), 0, NA))) %>% 
  select(tl_id = TL_ID, FORTYPCD_BIN) %>% 
  as.matrix %>% 
  classify(dat_treemap, .) %>% 
  trim %>% 
  writeRaster("03_intermediate/dat_treemap_fortypcd.tif", overwrite = TRUE)

dat_treemap_siteclcd = 
  dat_treemap_join %>% 
  select(tl_id = TL_ID, SITECLCD) %>% 
  rename(from = 1, to = 2) %>% 
  as.matrix %>% 
  classify(dat_treemap, .) %>% 
  trim %>% 
  writeRaster("03_intermediate/dat_treemap_siteclcd.tif", overwrite = TRUE)

dat_treemap_proportiondouglasfir = 
  dat_treemap_join %>% 
  select(tl_id = TL_ID, ProportionDouglasFir) %>% 
  as.matrix %>% 
  classify(dat_treemap, .) %>% 
  trim %>% 
  writeRaster("03_intermediate/dat_treemap_proportiondouglasfir.tif", overwrite = TRUE)

dat_treemap_proportionwesternhemlock = 
  dat_treemap_join %>% 
  select(tl_id = TL_ID, ProportionWesternHemlock) %>% 
  as.matrix %>% 
  classify(dat_treemap, .) %>% 
  trim %>% 
  writeRaster("03_intermediate/dat_treemap_proportionwesternhemlock.tif", overwrite = TRUE)

#  Extract results onto notifications.

dat_parcels_treemap_fortypcd = 
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
          sf::st_as_sf() %>% 
          exact_extract(
            x = "03_intermediate/dat_treemap_fortypcd.tif" %>% rast, 
            y = ., 
            fun = "mean", 
            append_cols = "Parcel", 
            max_cells_in_memory = 1e+09
          ) %>% 
          # extract(
          #   x = "03_intermediate/dat_treemap_fortypcd.tif" %>% rast,
          #   y = .,
          #   fun = mean,
          #   na.rm = TRUE,
          #   ID = FALSE,
          #   bind = TRUE
          # ) %>%
          as_tibble, 
        .options = furrr_options(seed = TRUE),
        .progress = TRUE
        )
  ) %>% 
  unnest(Data_Parcels) %>% 
  select(-Chunk) %>% 
  rename(ProportionDouglasFirCondition = mean)

dat_parcels_treemap_siteclcd = 
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
          sf::st_as_sf() %>% 
          exact_extract(
            x = "03_intermediate/dat_treemap_siteclcd.tif" %>% rast, 
            y = ., 
            fun = "mode", 
            append_cols = "Parcel", 
            max_cells_in_memory = 1e+09
          ) %>% 
          as_tibble, 
        .options = furrr_options(seed = TRUE),
        .progress = TRUE
      )
  ) %>% 
  unnest(Data_Parcels) %>% 
  select(-Chunk) %>% 
  rename(SiteClassMode = mode)

dat_parcels_treemap_proportiondouglasfir = 
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
          sf::st_as_sf() %>% 
          exact_extract(
            x = "03_intermediate/dat_treemap_proportiondouglasfir.tif" %>% rast, 
            y = ., 
            fun = "mean", 
            append_cols = "Parcel", 
            max_cells_in_memory = 1e+09
          ) %>% 
          as_tibble, 
        .options = furrr_options(seed = TRUE),
        .progress = TRUE
      )
  ) %>% 
  unnest(Data_Parcels) %>% 
  select(-Chunk) %>% 
  rename(ProportionDouglasFirTree = mean)

dat_parcels_treemap_proportionwesternhemlock = 
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
          sf::st_as_sf() %>% 
          exact_extract(
            x = "03_intermediate/dat_treemap_proportionwesternhemlock.tif" %>% rast, 
            y = ., 
            fun = "mean", 
            append_cols = "Parcel", 
            max_cells_in_memory = 1e+09
          ) %>% 
          as_tibble, 
        .options = furrr_options(seed = TRUE),
        .progress = TRUE
      )
  ) %>% 
  unnest(Data_Parcels) %>% 
  select(-Chunk) %>% 
  rename(ProportionWesternHemlockTree = mean)

dat_parcels_treemap = 
  dat_parcels_treemap_fortypcd %>% 
  left_join(dat_parcels_treemap_siteclcd) %>%
  left_join(dat_parcels_treemap_proportiondouglasfir) %>%
  left_join(dat_parcels_treemap_proportionwesternhemlock)

#  Aggregate and export.

dat_portfolios = "03_intermediate/dat_portfolios.gdb" %>% vect %>% as_tibble

dat_portfolios_treemap = 
  dat_portfolios %>% 
  left_join(dat_parcels_treemap) %>% 
  group_by(Owner_Cotality, Year_Quarter) %>% 
  summarize(
    across(c(SiteClassMode, starts_with("Proportion")), ~ weighted.mean(.x, Area, na.rm = TRUE)),
    Area = sum(Area, na.rm = TRUE)
  ) %>% 
  ungroup %T>% 
  write_csv("03_intermediate/dat_portfolios_covariates_treemap.csv")

#  Stop timing. 

time_end = Sys.time()

time_end - time_start
