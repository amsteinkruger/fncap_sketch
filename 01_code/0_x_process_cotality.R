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

# figure out land use code

# check whether forest/timberland parcels are all within relevant ODF spatial definition

# figure out transaction join to transform ownership into a panel
  
#  Export
  
# timer stops here
