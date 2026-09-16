# Reconcile parcel, tax, and deed data to get a panel of land ownership. 

#   what's the deal with non-1-1 joins between PB and parcels?
#   how do multi-parcel records work for both PB and OT?
#   what are the right land use codes to reduce OT, PB on?
#    what about cases of land use change?

#  Get bounds for later.

dat_bounds = "03_intermediate/dat_bounds.gdb" %>% vect %>% project("EPSG:3857")

#  Get Property Basic data.

#   Get all variable names.

vec_pb_names_all = 
  "02_data/0_0_0_Cotality/2_PB/OR_PB_SELECT.csv" %>% 
  read_csv %>% 
  pull(VARIABLE)

#   Get useful variable names.

vec_pb_names_use = 
  "02_data/0_0_0_Cotality/2_PB/OR_PB_SELECT.csv" %>% 
  read_csv %>% 
  filter(SELECT == 1) %>% 
  pull(VARIABLE)

#   Get even more useful variable names.

vec_pb_names_bind = 
  "02_data/0_0_0_Cotality/2_PB/OR_PB_SELECT.csv" %>% 
  read_csv %>% 
  filter(BIND == 1) %>% 
  pull(VARIABLE) %>% 
  str_replace_all(" ", "_")

#   Read. 

dat_pb = 
  "02_data/0_0_0_Cotality/2_PB/OR_PB_08052026.csv" %>% 
  read_csv(col_names = FALSE) %>% 
  slice(-1:-2) %>% 
  set_names(vec_pb_names_all) %>% 
  select(all_of(vec_pb_names_use)) %>% 
  rename_with(~ str_replace_all(.x, " ", "_"))

#  Get Owner Transfer data.

#   Get all variable names.

vec_ot_names_all = 
  "02_data/0_0_0_Cotality/3_OT/OR_OT_SELECT.csv" %>% 
  read_csv %>% 
  mutate(VARIABLE = VARIABLE %>% str_replace_all("�", " ")) %>% 
  pull(VARIABLE)

#   Get useful variable names.

vec_ot_names_use = 
  "02_data/0_0_0_Cotality/3_OT/OR_OT_SELECT.csv" %>% 
  read_csv %>% 
  mutate(VARIABLE = VARIABLE %>% str_replace_all("�", " ")) %>% 
  filter(SELECT == 1) %>% 
  pull(VARIABLE)

#   Get even more useful variable names.

vec_ot_names_bind = 
  "02_data/0_0_0_Cotality/3_OT/OR_OT_SELECT.csv" %>% 
  read_csv %>% 
  filter(BIND == 1) %>% 
  pull(VARIABLE) %>% 
  str_replace_all(" ", "_")

#   Get data, handle names, and select useful variables. 

dat_ot = 
  "02_data/0_0_0_Cotality/3_OT/OR_OT_08052026.csv" %>% 
  read_csv(col_names = FALSE) %>% 
  slice(-1:-2) %>% 
  set_names(vec_ot_names_all) %>% 
  select(all_of(vec_ot_names_use)) %>% 
  rename_with(~ str_replace_all(.x, " ", "_"))

#  Prepare data for wrangling into implicit and explicit panels.  

#   Handle Property Basic.

#    Reduce to relevant counties.

vec_counties_fips = 
  c(3, 5, 7, 9, 11, 15, 17, 19, 27, 29, 31, 33, 35, 39, 41, 43, 47, 51, 53, 57, 75, 67, 71) %>% 
  as.character %>% 
  str_pad(3, "left", "0") %>% 
  paste0("41", .)

dat_pb_less = dat_pb %>% filter(FIPS_CODE %in% vec_counties_fips)

#    Explicate spatial data. Note that some observations are missing coordinates. 

dat_pb_less_spatial = 
  dat_pb_less |> 
  select(CLIP, FIPS_CODE, starts_with("PARCEL_LEVEL")) %>% 
  drop_na(starts_with("PARCEL_LEVEL")) %>% 
  mutate(across(starts_with("PARCEL_LEVEL"), as.numeric)) %>% 
  vect(
    geom = c("PARCEL_LEVEL_LONGITUDE", "PARCEL_LEVEL_LATITUDE"), 
    crs = "+proj=longlat +datum=WGS84"
  ) |> 
  project("EPSG:3857") %>% 
  crop(dat_bounds)

#    Plot.

dat_pb_less_spatial %>% 
  select(CLIP) %>% 
  slice_sample(n = 10000) %>% 
  ggplot() + 
  geom_spatvector(color = "gray50", fill = NA, shape = 21, alpha = 0.25)

#   Join PB to parcels by county with centroid nearest neighbors. 

#    Handle parcel data. 

dat_parcels = 
  "02_data/0_0_0_Cotality/1_Parcels/2020_shapefile" %>%
  read_sf %>% 
  select(PARCEL = OBJECTID, COUNTY = County) %>% 
  mutate(
    VALID_GEOM = st_is_valid(geometry),
    EMPTY_GEOM = st_is_empty(geometry)) %>% 
  filter(VALID_GEOM & !EMPTY_GEOM) %>% 
  select(PARCEL, COUNTY) %>% 
  vect %>% 
  crop(dat_bounds) %T>%
  writeVector("03_intermediate/dat_parcels_polygons.gdb") %>%
  centroids %T>% 
  writeVector("03_intermediate/dat_parcels_points.gdb")

#    Crosswalk counties. 

dat_pb_less_spatial_counties = 
  dat_pb_less_spatial %>% 
  as_tibble %>% 
  select(CLIP, FIPS_CODE) %>% 
  group_by(FIPS_CODE) %>% 
  nest %>% 
  mutate(data = data %>% map(~ slice_sample(.x, n = 1000))) %>% 
  unnest %>% 
  ungroup %>% 
  semi_join(dat_pb_less_spatial, .)

dat_parcels_counties = 
  dat_parcels %>% 
  as_tibble %>% 
  group_by(COUNTY) %>% 
  nest %>% 
  mutate(data = data %>% map(~ slice_sample(.x, n = 1000))) %>% 
  unnest %>% 
  ungroup %>% 
  semi_join(dat_parcels, .)

dat_crosswalk_counties = 
  dat_pb_less_spatial_counties %>% 
  nearest(dat_parcels_counties) %>% 
  as_tibble %>% 
  left_join(dat_pb_less_spatial_counties %>% as_tibble %>% mutate(from_id = row_number())) %>% 
  left_join(dat_parcels_counties %>% as_tibble %>% mutate(to_id = row_number())) %>% 
  group_by(FIPS_CODE, COUNTY) %>% 
  summarize(COUNT = n()) %>% 
  ungroup %>% 
  group_by(COUNTY) %>% 
  filter(COUNT == max(COUNT)) %>% 
  ungroup %>% 
  select(-COUNT)
  
#    Join NN.

dat_pb_nest = 
  dat_pb_less %>% 
  distinct(FIPS_CODE) %>% 
  arrange(FIPS_CODE) %>% 
  mutate(DATA_PB = FIPS_CODE %>% map(~ filter(dat_pb_less_spatial, FIPS_CODE == .x)))

dat_parcels_nest = 
  dat_parcels %>% 
  as_tibble %>% 
  distinct(COUNTY) %>% 
  left_join(dat_crosswalk_counties) %>% 
  arrange(FIPS_CODE) %>% 
  mutate(DATA_PARCELS = COUNTY %>% map(~ filter(dat_parcels, COUNTY == .x))) %>% 
  select(-COUNTY)

dat_pb_parcels = 
  left_join(dat_pb_nest, dat_parcels_nest) %>% 
  mutate(DATA_NEAREST = map2(DATA_PB, DATA_PARCELS, nearest)) %>% 
  mutate(
    DATA_OUT = 
      DATA_NEAREST %>% 
      map(as_tibble) %>% 
      map(~ select(.x, ends_with("id"))) %>% 
      map2(
        DATA_PB, 
        ~ left_join(
          .x, 
          .y %>% as_tibble %>% select(CLIP) %>% mutate(from_id = row_number())
        )
      ) %>% 
      map2(
        DATA_PARCELS, 
        ~ left_join(
          .x, 
          .y %>% as_tibble %>% select(PARCEL) %>% mutate(to_id = row_number())
        )
      ) %>% 
      map(~ select(.x, CLIP, PARCEL))
  ) %>% 
  select(DATA_OUT) %>% 
  unnest(DATA_OUT) %T>% 
  write_csv("03_intermediate/dat_pb_parcels_test.csv")

dat_parcels_pb = 
  left_join(dat_parcels_nest, dat_pb_nest) %>% 
  drop_na(FIPS_CODE) %>% # Eliminate an oddball county from the parcel subset. 
  mutate(DATA_NEAREST = map2(DATA_PARCELS, DATA_PB, nearest)) %>% 
  mutate(
    DATA_OUT = 
      DATA_NEAREST %>% 
      map(as_tibble) %>% 
      map(~ select(.x, ends_with("id"))) %>% 
      map2(
        DATA_PARCELS, 
        ~ left_join(
          .x, 
          .y %>% as_tibble %>% select(PARCEL) %>% mutate(from_id = row_number())
        )
      ) %>% 
      map2(
        DATA_PB, 
        ~ left_join(
          .x, 
          .y %>% as_tibble %>% select(CLIP) %>% mutate(to_id = row_number())
        )
      ) %>% 
      map(~ select(.x, PARCEL, CLIP))
  ) %>% 
  select(DATA_OUT) %>% 
  unnest(DATA_OUT) %T>% 
  write_csv("03_intermediate/dat_parcels_pb_test.csv")

#   Set up PB for a semijoin to OT and for appending to OT. 

dat_pb_bind = 
  dat_pb_less %>% 
  select(all_of(vec_pb_names_bind)) %>% 
  semi_join(dat_pb_parcels)

#   Owner Transfers

#    Reduce to Lane County.

dat_ot_less = dat_ot %>% filter(FIPS_CODE %in% vec_counties_fips)

#    Set up OT for a semi-join with PB-Parcels and for appending to PB.  

#     CLIP is unique in PB but not in OT. 
#     OWNER_TRANSFER_COMPOSITE_TRANSACTION_ID is a unique ID in OT. 
#     There are no non-missing values in field PREVIOUS_CLIP for either PB or OT. 

dat_ot_bind = 
  dat_ot_less %>% 
  select(all_of(vec_ot_names_bind)) %>% 
  rename_with(
    ~ .x %>% 
      str_trim %>% 
      str_remove_all("DEED_") %>% 
      str_remove_all("_-_STATIC") %>% 
      str_replace_all("SALE_DERIVED_", "SALE_")
  ) %>% 
  semi_join(dat_pb_parcels)

#   Prepare an implicit panel. 

dat_pb_ot_implicit = 
  dat_pb_bind %>% 
  bind_rows(dat_ot_bind) %>% 
  mutate(
    SALE_RECORDING_YEAR_QUARTER = 
      ifelse(
        !is.na(SALE_RECORDING_DATE),
        paste0(str_sub(SALE_RECORDING_DATE, 1, 4), "_", ceiling(as.numeric(str_sub(SALE_RECORDING_DATE, 5, 6)) / 3)),
        NA
      ),
    OWNER_BUYER_1 = 
      ifelse(
        !is.na(OWNER_1_FULL_NAME),
        OWNER_1_FULL_NAME,
        BUYER_1_FULL_NAME
      )
  ) %>% 
  select(
    CLIP, 
    SALE_RECORDING_YEAR_QUARTER, 
    OWNER_TRANSFER_COMPOSITE_TRANSACTION_ID, 
    OWNER_BUYER_1
  ) %>% 
  # This arrange() call is for easier review of pre-summarize() records. 
  arrange(
    CLIP, 
    desc(SALE_RECORDING_YEAR_QUARTER), 
    desc(OWNER_TRANSFER_COMPOSITE_TRANSACTION_ID), 
    OWNER_BUYER_1
  ) %>% 
  #  Drop OT records with no useful dates. There are just a few of these. 
  filter(!is.na(SALE_RECORDING_YEAR_QUARTER) | is.na(OWNER_TRANSFER_COMPOSITE_TRANSACTION_ID)) %>% 
  #  Drop all but the latest record for each quarter. 
  group_by(CLIP, SALE_RECORDING_YEAR_QUARTER) %>% 
  filter(row_number() == min(row_number())) %>% 
  ungroup %>% 
  #  Drop the OT ID, since the previous step means CLIP-YEAR_QUARTER is the UID. 
  select(-OWNER_TRANSFER_COMPOSITE_TRANSACTION_ID) %>% 
  #  Assume that PB records can reasonably be dated to Q4 2024. (This is easy to check.)
  mutate(SALE_RECORDING_YEAR_QUARTER = SALE_RECORDING_YEAR_QUARTER %>% replace_na("2024_4")) %>% 
  rename(YEAR_QUARTER = SALE_RECORDING_YEAR_QUARTER) %T>% 
  # Export. 
  write_csv("03_intermediate/data_cotality_implicit.csv")

#   Prepare data in an explicit panel. 

#    Set up a function to pick buyer/owner information. 

fun_explicate = 
  function(owner_0, owner_1){
    
    ifelse(!is.na(owner_0) & is.na(owner_1), 
           owner_0,
           owner_1)
    
  }

#    Get the first observed quarter of ownership information for each CLIP. 

dat_pb_ot_first = 
  dat_pb_ot_implicit %>% 
  select(CLIP, YEAR_QUARTER) %>% 
  group_by(CLIP) %>% 
  filter(YEAR_QUARTER == min(YEAR_QUARTER, na.rm = TRUE)) %>% 
  ungroup %>% 
  distinct %>% 
  rename(FIRST_YEAR_QUARTER = YEAR_QUARTER)

#    Prepare an explicit panel. 

dat_pb_ot_explicit = 
  dat_pb_ot_implicit %>% 
  # Transform data into an explicit panel. 
  complete(CLIP, YEAR_QUARTER = paste0(rep(2000:2024, each = 4), "_", 1:4)) %>% 
  # Drop year-quarters without implicit observations.
  #  Get first observed year-quarters.
  left_join(dat_pb_ot_first) %>% 
  #  Filter to quasi-observed year-quarters or, for single-owner CLIPs, year-quarters from 2015_1 forward. 
  group_by(CLIP) %>% 
  filter(YEAR_QUARTER >= FIRST_YEAR_QUARTER | YEAR_QUARTER > "2014_4" & n_distinct(OWNER_BUYER_1) == 2) %>% 
  select(-FIRST_YEAR_QUARTER) %>% 
  # Turn implicit observations into explicit observations. 
  arrange(CLIP, YEAR_QUARTER) %>% # Order matters for accumulate(). 
  mutate(OWNER_BUYER_1_EXPLICATE_FORWARD = accumulate(OWNER_BUYER_1, ~ fun_explicate(.x, .y))) %>% 
  arrange(CLIP, desc(YEAR_QUARTER)) %>% # Order matters for accumulate(). 
  mutate(OWNER_BUYER_1_EXPLICATE_BACKWARD = accumulate(OWNER_BUYER_1_EXPLICATE_FORWARD, ~ fun_explicate(.x, .y))) %>% 
  ungroup %>% 
  # Clean up. 
  filter(YEAR_QUARTER > "2014_4" & YEAR_QUARTER < "2025_1") %>% 
  select(CLIP, YEAR_QUARTER, OWNER = OWNER_BUYER_1_EXPLICATE_BACKWARD) %T>% 
  # Export.
  write_csv("03_intermediate/data_cotality_explicit.csv")
