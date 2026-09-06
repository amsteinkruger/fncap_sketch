# Reconcile parcels, tax, and deed data to get a panel of forestland ownership. 

#  Problems: 
#   is the OT-PB join actually a join or more of a panel appending thing?
#   what's the deal with non-1-1 joins between PB and parcels?
#   or the equivalent issue for PB-OT if that crops up
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

#    Reduce to Lane County.

dat_parcels_less = dat_parcels |> filter(County == 20) 

#    Plot.

dat_parcels_less |>
  slice_sample(n = 10000) |>
  makeValid() |>
  ggplot() +
  geom_spatvector(fill = "gray50", color = NA)

#   Property Basic

#    Reduce to Lane County.

dat_pb_less = dat_pb %>% filter(FIPS_CODE == "41039")

#    Explicate spatial data. Note that 3393 of 180444 observations are missing coordinates. 

dat_pb_less_spatial = 
  dat_pb_less |> 
  select(CLIP, starts_with("PARCEL_LEVEL")) %>% 
  drop_na(starts_with("PARCEL_LEVEL")) %>% 
  mutate(across(starts_with("PARCEL_LEVEL"), as.numeric)) %>% 
  vect(
    geom = c("PARCEL_LEVEL_LONGITUDE", "PARCEL_LEVEL_LATITUDE"), 
    crs = "+proj=longlat +datum=WGS84"
  ) |> 
  project("EPSG:3857")

#    Plot.

dat_pb_less_spatial %>% 
  select(CLIP) %>% 
  ggplot() + 
  geom_spatvector(color = "gray50", fill = NA, shape = 21, alpha = 0.25)

#   Join PB to parcels by centroid nearest neighbors. 

dat_parcels_less_centroids = 
  dat_parcels_less %>% 
  select(PARCEL = OBJECTID) %>% 
  mutate(ROW = row_number()) %>% 
  makeValid(buffer = TRUE) %>% 
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

dat_ot_less = dat_ot %>% filter(FIPS_CODE == "41039")

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
      str_replace_all("SALE_", "SALE_DERIVED_")
  ) %>% 
  semi_join(dat_pb_parcels)

#   Prepare data in an implicit panel. 

dat_pb_ot = dat_pb_bind %>% bind_rows(dat_ot_bind)

#  ???

#  Then do something with that
#  I guess use that to subset parcels by years (to avoid extra geospatial work)
#  Then get geospatial data of interest just from the subset of parcels with associated years
#  Then finalize the panel with covariates
#  Note that geospatial variables are only for gentrification work, so don't do that now
#  Next piece with forest work is handling notification-PB/OT intersections with land use codes and landowner details
#  So, work through handling covariates and subsetting parcels, then split workflows

# reference code follows

dat_transactions_less =
  dat_transactions |> 
  select(clip, starts_with("parcel_"), year_sold, ends_with("_1_full_name")) |> 
  filter(year_sold %in% 2015:2024)

dat_transactions_spatial = 
  dat_transactions_less |> 
  select(clip, starts_with("parcel_")) |> 
  vect(geom = c("parcel_longitude", "parcel_latitude")) |> 
  project("EPSG:3857")

dat_owners_transactions_extract = 
  dat_owners_parcels_join |> 
  select(clip) |> 
  terra::extract(dat_transactions_spatial) |> 
  rename(clip_owner = clip)

dat_owners_transactions_pivot = 
  dat_transactions_less |> 
  rename(clip_transaction = clip) |> 
  mutate(id.y = row_number()) |> 
  left_join(dat_owners_transactions_extract) |> 
  select(-id.y, -starts_with("parcel_")) |> 
  drop_na(clip_owner)

dat_owners_panel_set = 
  dat_owners_parcels_join |> 
  as_tibble() |> 
  select(clip_owner = clip,
         parcel = ID_Parcel,
         landusecode,
         stateusedescription,
         countyusedescription,
         owner = owner1fullname) |> 
  mutate(year = 2024)

dat_transactions_panel_set = 
  dat_owners_transactions_pivot |> 
  drop_na(clip_owner) |> 
  select(clip_owner,
         clip_transaction,
         year = year_sold,
         owner = seller_1_full_name)

dat_panel_set = bind_rows(dat_owners_panel_set, dat_transactions_panel_set)

dat_panel = 
  dat_panel_set |> 
  relocate(year, .before = owner) |> 
  arrange(clip_owner, desc(year), parcel)

# handle complex observations -- here, "handle" means "drop"
# so, with all the other conditions in place, this:
#  discards properties/parcels with multiple transactions in one year
#  discards properties/parcels with a transaction in 2024
# this is dumb but easier than reconciling multiple transactions within years. 

dat_panel_check = 
  dat_panel |> 
  group_by(clip_owner, year) |> 
  mutate(count = n()) |> 
  group_by(clip_owner) |> 
  mutate(count_max = count |> max()) |> 
  ungroup() |> 
  mutate(count_check = (count == count_max)) |> 
  filter(count_max == 1) |> 
  mutate(which = ifelse(is.na(parcel), "transaction", "owner")) |> 
  select(clip_owner, which, year, owner) |> 
  pivot_wider(names_from = which,
              values_from = owner) |> 
  mutate(which = ifelse(is.na(owner), "transaction", "ownership"),
         owner_combine = ifelse(is.na(owner), transaction, owner)) |> 
  select(-owner, -transaction)

dat_panel_complete = 
  dat_panel_check |> 
  select(clip_owner, year) |> 
  distinct() |> 
  complete(clip_owner, year) |> 
  left_join(dat_panel_check)

fun_fill = 
  function(owner_0, owner_1){
    
    ifelse(!is.na(owner_0) & is.na(owner_1), 
           owner_0,
           owner_1)
    
  }

dat_panel_filled =
  dat_panel_complete |> 
  arrange(clip_owner, desc(year)) |> 
  group_by(clip_owner) |> 
  mutate(owner_fill = accumulate(owner_combine, ~ fun_fill(.x, .y))) |> 
  ungroup() |> 
  mutate(owner = owner_fill,
         which = ifelse(is.na(which), "inferred", which)) |> 
  select(-c(owner_combine, owner_fill))