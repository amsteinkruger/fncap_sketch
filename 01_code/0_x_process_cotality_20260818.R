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

dat_pb = 
  "02_data/0_0_0_Cotality/2_PB/OR_PB_08052026.csv" %>% 
  read_csv(col_names = FALSE) %>% 
  slice(-1:-2) %>% 
  set_names(vec_pb_names_all) %>% 
  select(all_of(vec_pb_names_use)) %>% 
  rename_with(~ str_replace_all(.x, " ", "_"))

dat_pb_problems = dat_pb %>% problems

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

#   Get data, handle names, and select useful variables. 

dat_ot = 
  "02_data/0_0_0_Cotality/3_OT/OR_OT_08052026.csv" %>% 
  read_csv(col_names = FALSE) %>% 
  slice(-1:-2) %>% 
  set_names(vec_ot_names_all) %>% 
  select(all_of(vec_ot_names_use)) %>% 
  rename_with(~ str_replace_all(.x, " ", "_"))

#   Check problems. (They're fine.)

dat_ot_problems = dat_ot %>% problems

#  Reduce and visualize each dataset.  

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

dat_pb_less_spatial = 
  dat_pb_less |> 
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

#   Owner Transfers

#    Reduce to Lane County.

#    Plot.

#  Join Property Basic to parcels by centroid nearest neighbors. 

#   Set up data to join.

#   Diagnose join issues. 

#  Join Owner Transfer to Property Basic by (coordinates and/or CLIP?).

#   Set up data to join.

#   Diagnose join issues. 

#  Transform the OT-PB join into a panel of explicit and implicit ownership. 

#  ???


# Here be reference code. 

library(haven)

#  Owners

#   With pre-processing in Stata to avoid variable type issues. 

dat_owners = 
  "02_data/0_0_0_Cotality/Owners/owners.csv" %>%
  read_csv

#   Note from problems() that columns 31 and 35 have difficult values.

#  Parcels

dat_parcels = 
  "02_data/0_0_0_Cotality/Parcels/2020_shapefile/OR_parcels_2020.shp" %>% 
  vect

#  Spatial Join

dat_parcels_less =
  dat_parcels |> 
  filter(County == 20) # %in% c(4, 29, 21, 20, 10, 6, 8)

dat_parcels_less |>
  slice_sample(n = 10000) |>
  makeValid() |>
  ggplot() +
  geom_spatvector(aes(fill = Shape_Area), color = NA)

# dat_parcels_summarize = 
#   dat_parcels_less |> 
#   makeValid() |> 
#   summarize(Shape_Area = sum(Shape_Area)) |> 
#   fillHoles()

# owners to parcels by (1) intersection and (2) centroid nearest-neighbor

dat_owners_less = dat_owners |> filter(ctyfips == 39) |> select(clip, starts_with("parcellevel"))

dat_owners_less_spatial = 
  dat_owners_less |> 
  vect(geom = c("parcellevellongitude", "parcellevellatitude")) |> 
  project("EPSG:3857")

dat_owners_parcels_extract = terra::extract(dat_parcels_less, dat_owners_less_spatial)

# Clean up for checks and joins.

dat_owners_parcels_extract_less = 
  dat_owners_parcels_extract |> 
  select(ID_Parcel = OBJECTID,
         ID_Owner = id.y)

# Check.

dat_owners_parcels_extract_less |> nrow() # 185168. Note multiple matches.
dat_owners_parcels_extract_less |> drop_na(ID_Owner) |> nrow() # 185168
dat_owners_parcels_extract_less |> drop_na(ID_Parcel) |> nrow() # 180578. So, 2.8% missing.

dat_owners_parcels_extract_less |> 
  group_by(ID_Owner) |> 
  summarize(Count = n()) |> 
  group_by(Count) |> 
  summarize(Counter = n()) |> 
  ungroup() |> 
  arrange(desc(Counter))

# 177385 owner records have only one parcel match. So, 4.2% of matches are problematic. 

dat_owners_parcels_extract_less |> 
  group_by(ID_Parcel) |> 
  summarize(Count = n()) |> 
  group_by(Count) |> 
  summarize(Counter = n()) |> 
  ungroup() |> 
  arrange(desc(Counter))

# 133874 of 154058 (87%) have only one owner match. Are multiple owner matches problematic? 

# Reduce to 1-1 matches. 

# dat_owners_parcels_clean = 
#   dat_owners_parcels_extract_less |> 
#   drop_na() |> 
#   group_by(ID_Parcel) |> 
#   mutate(Count_Parcel = n()) |> 
#   group_by(ID_Owner) |> 
#   mutate(Count_Owner = n()) |> 
#   ungroup() |> 
#   filter(Count_Parcel == 1) |> 
#   filter(Count_Owner == 1)

# 132902 (71%) of 185168 matches are 1-1. Centroid methods are a first suspect.

# This is where matching on centroids rather than point-polygon extraction would go. 

# Instead, reduce to the first match for each owner; preserve all owners.

dat_owners_parcels_clean = 
  dat_owners_parcels_extract_less |>
  drop_na() |>
  group_by(ID_Owner) |>
  mutate(Number_Owner = row_number()) |>
  ungroup() |>
  filter(Number_Owner == 1) %>% 
  select(starts_with("ID"))

# Suppose these matches are fine, though. Go ahead with the join.

dat_owners_parcels_join = 
  dat_owners_parcels_clean |> 
  left_join(dat_owners_less |> 
              mutate(ID_Owner = row_number()) |> 
              select(ID_Owner, clip)) |> 
  select(-ID_Owner) %>% # magrittr pipe matters here. 
  left_join(dat_parcels_less |> select(ID_Parcel = OBJECTID), .) |> 
  drop_na(clip) |> 
  left_join(dat_owners %>% select(clip, stateusedescription)) %>% 
  select(clip, stateusedescription)

# Check whether forest/timberland parcels fall within ODF's private forestry layer.

dat_odf = 
  "02_data/3_4_ODF_Ownership/Ownership.gdb" %>% 
  vect %>% 
  select(Owner = LandManager) %>% 
  project("EPSG:3857") %>% 
  group_by(Owner) %>% 
  summarize %>% 
  ungroup |> 
  makeValid(buffer = TRUE)

dat_owners_parcels_odf = 
  dat_owners_parcels_join |> 
  # slice_head(n = 100) |>
  makeValid(buffer = TRUE) |> 
  is.related(dat_odf, "intersects") |> 
  tibble() |> 
  rename(intersects = 1) %>%
  bind_spat_cols(dat_owners_parcels_join, .) |> #  |> slice_head(n = 100)
  filter(intersects)

dat_owners_parcels_other = 
  dat_owners_parcels_join |> 
  # slice_head(n = 100) |>
  anti_join(dat_owners_parcels_odf |> as_tibble() |> select(clip))

# Pull values for land use codes in private forest/timberland. 

dat_owners_parcels_odf |> 
  as_tibble() |> 
  group_by(stateusedescription) |> 
  summarize(count = n()) |> 
  ungroup() |> 
  arrange(desc(count)) |> 
  slice_head(n = 10)

# Pull values for land use codes outside of forest/timberland.

dat_owners_parcels_other |> 
  as_tibble() |> 
  group_by(stateusedescription) |> 
  summarize(count = n()) |> 
  ungroup() |> 
  arrange(desc(count)) |> 
  slice_head(n = 10)

# Check top landowners (by counts of parcels) for forest/timberland and other land. 

dat_owners_parcels_odf |> 
  as_tibble() |> 
  group_by(owner1fullname) |> 
  summarize(count = n()) |> 
  ungroup() |> 
  arrange(desc(count)) |> 
  slice_head(n = 10)

dat_owners_parcels_other |> 
  as_tibble() |> 
  group_by(owner1fullname) |> 
  summarize(count = n()) |> 
  ungroup() |> 
  arrange(desc(count)) |> 
  slice_head(n = 10)

# Join transfers onto ownership and parcels to obtain a panel of ownership. 

dat_transactions = 
  "02_data/0_0_0_Cotality/Transactions/Lane_Res_clean_v2026.dta" |> 
  read_dta()

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

#  Export
