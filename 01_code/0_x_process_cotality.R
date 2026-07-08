# Process ownership data. 

# timer goes here

library(haven)

#  Owners

# dat_owners = 
#   "02_data/0_0_0_Cotality/Owners/OR41_PB_080524.dta" %>%
#   read_dta()

#   Use a modified version to avoid string storage issues?

dat_owners = 
  "02_data/0_0_0_Cotality/Owners/owners.csv" %>%
  read_csv

#   Note from problems() that columns 31 and 35 have difficult values.

vis = 
  dat_owners |> 
  filter(ctyfips == 39) |> 
  select(starts_with("parcel") & (ends_with("latitude") | ends_with("longitude"))) |> 
  ggplot() + 
  geom_point(aes(x = parcellevellongitude, 
                 y = parcellevellatitude))

#   Looks like Oregon. 

#   Note that a reasonable next step would be filtering on land use codes.
#   However, land use codes are complex: there are a lot, many apply, and many are missing. 
#   Could be worth checking out land use change codes (not in this excerpt), but that's gravy. 

#  Parcels

dat_parcels = 
  "02_data/0_0_0_Cotality/Parcels/2020_shapefile/OR_parcels_2020.shp" %>% 
  vect

#  Spatial Join

# Scratch: 

dat_parcels_less =
  dat_parcels |> 
  filter(County == 20) # %in% c(4, 29, 21, 20, 10, 6, 8)

dat_parcels_less |> 
  slice_sample(n = 10000) |> 
  makeValid() |> 
  ggplot() + 
  geom_spatvector(aes(fill = Shape_Area), color = NA)

dat_parcels_summarize = 
  dat_parcels_less |> 
  makeValid() |> 
  summarize(Shape_Area = sum(Shape_Area)) |> 
  fillHoles()

# owners to parcels by (1) intersection and (2) centroid nearest-neighbor

dat_owners_less = dat_owners |> filter(ctyfips == 39) |> select(clip, starts_with("parcellevel"))

dat_owners_less_spatial = 
  dat_owners_less |> 
  vect(geom = c("parcellevellongitude", "parcellevellatitude")) |> 
  project("EPSG:3857")

ggplot() + 
  geom_spatvector(data = dat_parcels_less, fill = "red", alpha = 0.25) +
  geom_spatvector(data = dat_owners_less_spatial, alpha = 0.25)

dat_owners_parcels_extract = terra::extract(dat_parcels_less, dat_owners_less_spatial)

# Check the result.

dat_owners_parcels_extract |> head()

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

# 177385 owner records have only one parcel matches. So, 4.2% (?) matches are problematic. 

dat_owners_parcels_extract_less |> 
  group_by(ID_Parcel) |> 
  summarize(Count = n()) |> 
  group_by(Count) |> 
  summarize(Counter = n()) |> 
  ungroup() |> 
  arrange(desc(Counter))

# 133874 of 154058 (87%) have only one owner match. Does that line up with the previous result?

# What about counting 1-1 matches?

dat_owners_parcels_clean = 
  dat_owners_parcels_extract_less |> 
  drop_na() |> 
  group_by(ID_Parcel) |> 
  mutate(Count_Parcel = n()) |> 
  group_by(ID_Owner) |> 
  mutate(Count_Owner = n()) |> 
  ungroup() |> 
  filter(Count_Parcel == 1) |> 
  filter(Count_Owner == 1)

# 132902 (71%) of 185168 matches are 1-1. Centroid methods are a first suspect.

# (centroid NN goes here)

# Suppose these matches are fine, though. Go ahead with the join.

dat_owners_parcels_join = 
  dat_owners_parcels_clean |> 
  select(starts_with("ID")) |> 
  left_join(dat_owners_less |> 
              mutate(ID_Owner = row_number()) |> 
              select(ID_Owner, clip)) |> 
  select(-ID_Owner) %>% # magrittr pipe matters here. 
  left_join(dat_parcels_less |> select(ID_Parcel = OBJECTID), .) |> 
  drop_na(clip) |> 
  left_join(dat_owners)
  
# nice

# check whether forest/timberland parcels are all within relevant ODF spatial definition

# should be overlaps rather than intersects?

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

# land use codes in forest/timberland (counts by occurrences and area)

dat_owners_parcels_odf |> 
  as_tibble() |> 
  group_by(stateusedescription) |> 
  summarize(count = n()) |> 
  ungroup() |> 
  arrange(desc(count)) |> 
  slice_head(n = 10)

# land use codes outside of forest/timberland (counts by occurrences and area)

dat_owners_parcels_other |> 
  as_tibble() |> 
  group_by(stateusedescription) |> 
  summarize(count = n()) |> 
  ungroup() |> 
  arrange(desc(count)) |> 
  slice_head(n = 10)

# owners

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

# figure out transaction join to transform ownership into a panel

# note that this is only the file for Lane County

# note a problem: if owner data came from 1/1/2025, good; if 12/31/2024, then one day of checking transactions against owners
# since in fact it's ~8/2024, then quite a few cases of needing to check transaction parties against owners

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
  drop_na(clip_owner) # |> 
  # pivot_wider() # Kidding?

# problem: panelizing a spatvector
# so we start with the pseudo-knowledge that each property was owned by its 2024 owner from 2015 on
# then we add the actual knowledge that some properties were owned by other owners
# so create both panels (pseudo, actual), left join actual onto pseudo, then reconcile columns
# and get all of that out of spatial formats for easier handling, then join back onto spatial data for export?
# or stick to two separate exports as in the time-variant variable case for later scripts
  
# full join to get incomplete panel: 2024 plus sale years
# then arrange by year, clip_owner
# then mutate out a lagged year_sold (plus one?) to get the start of each owner's tenure
# then nest/expand/unnest or something to get a row for each year within each tenure w/o overlaps in tenure
# note subannual challenge


# dat_owners_transactions_pivot
# dat_owners_parcels_join
# clip_owner, clip_transaction, ID_Parcel, year, landusecode, stateusedescription, countyusedescription, owner1fullname
# note trickery with parcel identification: depends on filtering to 1-1 matches, messy otherwise


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

# panel, but with missing years (no interpolation) and duplicate years (owner and transaction)

dat_panel = 
  dat_panel_set |> 
  relocate(year, .before = owner) |> 
  arrange(clip_owner, desc(year), parcel)

# handle complex observations -- here, "handle" means "drop"
# so, with all the other conditions in place, this:
#  discards properties/parcels with multiple transactions in one year
#  discards properties/parcels with a transaction in 2024
# this is dumb but a lot easier than reconciling multiple transactions within years. 

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

# bottom line for now: with 72% of owner records accounted for,
# 23% of owner records relate to one or more sales.
# but this is with only residential transactions.
# so, this is useful for the gentrification stuff
# but also could resolve the snapshot-panel dilemma for timber supply accounting. 
# open questions: what's going on with the 28% of difficult owner records? 
# and what would it take to get all transactions or all timberland transactions?
# resolve land use code stuff before proceeding to emails
#  what proportion of ODF-designated private forest/timberland is accounted for in parcels?
#  "" but for ownership records
#  what proportion of parcels in forest-related land uses falls outside of ODF-designated private forest/timberland?
#  who are the owners of parcels in forest/timberland? Do they roughly match to notifications?

#  Export

# timer stops here
