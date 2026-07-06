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

# timer stops here
