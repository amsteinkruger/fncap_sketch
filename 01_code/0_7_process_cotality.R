# Reconcile parcels, tax, and deed data to get a panel of forestland ownership. 

#   what's the deal with non-1-1 joins between PB and parcels?
#   how do multi-parcel records work for both PB and OT?
#   what are the right land use codes to reduce OT, PB on?
#    what about cases of land use change?

#  Get parcel data.

dat_parcels = 
  "02_data/0_0_0_Cotality/1_Parcels/2020_shapefile" %>% 
  vect

# Note that variable names require a little extra handling for PB, OT. 

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

#  Visualize data and prepare for wrangling into an implicit panel.  

#   Parcels

#    Reduce to relevant counties.

vec_counties_parcels = c(2, 3, 4, 5, 6, 8, 9, 10, 14, 15, 16, 17, 18, 20, 21, 22, 24, 26, 29, 33, 34, 36)

dat_parcels_less = dat_parcels |> filter(County %in% vec_counties_parcels) 

#   Property Basic

#    Reduce to relevant counties.

vec_counties_fips = 
  c(3, 5, 7, 9, 11, 15, 17, 19, 27, 29, 31, 33, 35, 39, 41, 43, 47, 51, 53, 57, 75, 67, 71) %>% 
  as.character %>% 
  str_pad(3, "left", "0") %>% 
  paste0("41", .)

dat_pb_less = dat_pb %>% filter(FIPS_CODE %in% vec_counties_fips)

#    Explicate spatial data. Note that 3393 of 180444 observations are missing coordinates. 

dat_bounds = "03_intermediate/dat_bounds.gdb" %>% vect %>% project("EPSG:3857")

dat_pb_less_spatial = 
  dat_pb_less |> 
  select(CLIP, starts_with("PARCEL_LEVEL")) %>% 
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

#   Join PB to parcels by centroid nearest neighbors. 

dat_parcels_less_centroids = 
  dat_parcels_less %>% 
  select(PARCEL = OBJECTID) %>% 
  mutate(ROW = row_number()) %>% 
  makeValid(buffer = TRUE) %T>% 
  writeVector("03_intermediate/dat_parcels.gdb") %>% # Set aside for later. 
  centroids

dat_pb_parcels = 
  dat_pb_less_spatial %>% 
  nearest(dat_parcels_less_centroids) %>% 
  as_tibble %>% 
  left_join(
    dat_parcels_less_centroids %>% as_tibble, 
    by = c("to_id" = "ROW")
  ) %>% 
  left_join(
    dat_pb_less_spatial %>% as_tibble %>% mutate(ROW = row_number()),
    by = c("from_id" = "ROW")
  ) %>% 
  select(PARCEL, CLIP)

#   Set up PB for an anti-join to OT and for appending to OT. 

dat_pb_bind = 
  dat_pb_less %>% 
  select(all_of(vec_pb_names_bind)) %>% 
  semi_join(dat_pb_parcels)

#   Owner Transfers

#    Reduce to Lane County.

dat_ot_less = dat_ot %>% filter(FIPS_CODE == %in% vec_counties_fips)

#    Set up OT for a semi-join to PB-Parcels and for appending to PB.  

#     CLIP is unique in OT but not in PB. 
#     There are no non-missing values in field PREVIOUS_CLIP for either PB or OT. 
#     OWNER_TRANSFER_COMPOSITE_TRANSACTION_ID is a unique ID in OT. 

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
    LAST_ASSESSOR_UPDATE_YEAR_QUARTER = 
      ifelse(
        !is.na(LAST_ASSESSOR_UPDATE_DATE),
        paste0(str_sub(LAST_ASSESSOR_UPDATE_DATE, 1, 4), "_", ceiling(as.numeric(str_sub(LAST_ASSESSOR_UPDATE_DATE, 6, 7)) / 3)),
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
    OWNER_TRANSFER_COMPOSITE_TRANSACTION_ID,
    OWNER_BUYER_1, 
    SALE_RECORDING_YEAR_QUARTER, 
    LAST_ASSESSOR_UPDATE_YEAR_QUARTER
  ) %>% 
  # This arrange() call is for easier review of pre-summarize() records. 
  arrange(
    CLIP, 
    LAST_ASSESSOR_UPDATE_YEAR_QUARTER, 
    desc(SALE_RECORDING_YEAR_QUARTER), 
    OWNER_TRANSFER_COMPOSITE_TRANSACTION_ID, 
    OWNER_BUYER_1
  ) %>% 
  # Drop oddball records for easier summarizing. 
  #  Drop records with no useful dates. There are just a few of these. 
  filter(!is.na(SALE_RECORDING_YEAR_QUARTER) | !is.na(LAST_ASSESSOR_UPDATE_YEAR_QUARTER)) %>% 
  #  Drop all but the last record within each dataset for each CLIP in each quarter.
  #  The point is to keep only the last buyer in sequences of transfers.
  group_by(CLIP, SALE_RECORDING_YEAR_QUARTER, LAST_ASSESSOR_UPDATE_YEAR_QUARTER) %>% 
  filter(row_number() == max(row_number())) %>% 
  ungroup %>% 
  # Summarize over PB and OT -- this collapses the last observed transfer and ownership for each CLIP. 
  group_by(CLIP, OWNER_BUYER_1) %>% 
  summarize(
    OWNER_TRANSFER_COMPOSITE_TRANSACTION_ID = max(OWNER_TRANSFER_COMPOSITE_TRANSACTION_ID, na.rm = TRUE),
    SALE_RECORDING_YEAR_QUARTER = max(SALE_RECORDING_YEAR_QUARTER, na.rm = TRUE),
    LAST_ASSESSOR_UPDATE_YEAR_QUARTER = max(LAST_ASSESSOR_UPDATE_YEAR_QUARTER, na.rm = TRUE)
  ) %>% 
  ungroup %>% 
  # This arrange() call is also for easier review (correcting group_by() shenanigans). 
  arrange(
    CLIP, 
    LAST_ASSESSOR_UPDATE_YEAR_QUARTER, 
    desc(SALE_RECORDING_YEAR_QUARTER), 
    OWNER_TRANSFER_COMPOSITE_TRANSACTION_ID, 
    OWNER_BUYER_1    
  ) %T>% 
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
  select(CLIP, SALE_RECORDING_YEAR_QUARTER) %>% 
  group_by(CLIP) %>% 
  filter(SALE_RECORDING_YEAR_QUARTER == min(SALE_RECORDING_YEAR_QUARTER, na.rm = TRUE)) %>% 
  ungroup %>% 
  distinct %>% 
  rename(FIRST_YEAR_QUARTER = SALE_RECORDING_YEAR_QUARTER) 

#    Prepare an explicit panel. 

dat_pb_ot_explicit = 
  dat_pb_ot %>% 
  # Reduce PB and OT year-quarters to a single variable. 
  pivot_longer(
    c(SALE_RECORDING_YEAR_QUARTER, LAST_ASSESSOR_UPDATE_YEAR_QUARTER),
    names_to = "SOURCE",
    values_to = "YEAR_QUARTER") %>% 
  drop_na(YEAR_QUARTER) %>% 
  # Prepare data for transformation into an explicit panel. 
  arrange(CLIP, desc(YEAR_QUARTER)) %>% # This is superfluous, I guess?
  select(-OWNER_TRANSFER_COMPOSITE_TRANSACTION_ID, -SOURCE) %>% 
  # Transform data into an explicit panel. 
  complete(CLIP, YEAR_QUARTER = paste0(rep(1987:2025, each = 4), "_", 1:4)) %>% 
  arrange(CLIP, YEAR_QUARTER) %>% 
  # Drop year-quarters without implicit observations.
  #  Get first observed year-quarters.
  left_join(dat_pb_ot_first) %>% 
  #  Filter to quasi-observed year-quarters or, for PB-only CLIPs, year-quarters from 2015_1 forward. 
  #   This would be cleaner with a flag set up after the pivot_longer() above. 
  group_by(CLIP) %>% 
  filter(YEAR_QUARTER >= FIRST_YEAR_QUARTER | YEAR_QUARTER > "2014_4" & n_distinct(OWNER_BUYER_1) == 2) %>% 
  select(-FIRST_YEAR_QUARTER) %>% 
  # Turn implicit observations into explicit observations. 
  mutate(OWNER_BUYER_1_EXPLICATE_FORWARD = accumulate(OWNER_BUYER_1, ~ fun_explicate(.x, .y))) %>% 
  arrange(CLIP, desc(YEAR_QUARTER)) %>% 
  mutate(OWNER_BUYER_1_EXPLICATE_BACKWARD = accumulate(OWNER_BUYER_1_EXPLICATE_FORWARD, ~ fun_explicate(.x, .y))) %>% 
  ungroup %>% 
  # Clean up. 
  filter(YEAR_QUARTER > "2014_4") %>% # Watch out for this in advancing past the 2015-2024 panel. 
  select(CLIP, YEAR_QUARTER, OWNER = OWNER_BUYER_1_EXPLICATE_BACKWARD) %T>% 
  # Export.
  write_csv("03_intermediate/data_cotality_explicit.csv")
