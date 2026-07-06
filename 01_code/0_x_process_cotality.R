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

# dat_owners_vect = 
# reduce to parcels of interest 
# reduce to a unique ID and parcel centroid coordinates
# then turn into a spatvector

#  Parcels

dat_parcels = 
  "02_data/0_0_0_Cotality/Parcels/2020_shapefile/OR_parcels_2020.shp" %>% 
  vect

#  Spatial Join

#   centroid coordinates to polygon identification

#   join dat_owners onto the spatially joined result

#   what about calculating polygon centroids then joining on point-point NN?

#  Clean?

#  Export

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

dat_owners_parcels_extract |> head()

# things to check: 
#  are all matches 1-to-1?
#  are some matches missing?
#  if yes and no, then use id.y-OBJECTID pairs to join owner information onto parcels

# dat_owners_parcels_relate = 
#   terra::relate(
#     dat_owners_less_spatial, 
#     dat_parcels_less, 
#     relation = "within",
#     pairs = TRUE
#     )

# then check for conflicts

# then do something to resolve conflicts

# figure out land use code

# check whether forest/timberland parcels are all within relevant ODF spatial definition

# timer stops here
