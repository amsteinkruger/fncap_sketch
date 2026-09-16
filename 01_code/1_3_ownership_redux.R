# Handle land ownership. 

#  (1) Assign owner strings to notifications by nearest neighbors (notifications-parcels).
#  (2) Assign acres to owners. 

#  Clear the environment.

rm(list = ls())

#  Start timing. 

time_start = Sys.time()

#  (1) 

#  Set up notifications for nearest-neighbor computation.

#   Pick activities to keep.

vec_activities = c("Clearcut/Overstory Removal", "Commercial Thinning/Selective Cutting", "Salvage")

#   Pick landowners to keep. This hides extensive identification of landowners by hand. 
#    Note that this is a holdover from identificatin of FERNS owners. 
#    Also note that this object is overwritten later in the script -- leaving it for now. 

dat_owners = 
  "03_intermediate/dat_owners_in.xlsx" %>% 
  read_xlsx %>%
  filter(Landowner_Private == 1) %>%
  drop_na(Landowner_Company_Reviewed) %>%
  select(1:2)

#   Handle notifications. 
  
dat_notifications =
  "03_intermediate/dat_notifications_1_2.gdb" %>% 
  vect %>% 
  # Activities
  filter(ActivityType %in% vec_activities) %>% 
  # Landowners
  semi_join(dat_owners) %>% 
  left_join(dat_owners) %>% 
  # Centroids
  centroids

#  Set up parcels for nearest-neighbor computation. 

dat_parcels = 
  "03_intermediate/dat_parcels_points.gdb" %>% 
  vect %>% 
  project("EPSG:2992")

#  Get nearest neighbors (parcels to notifications).

dat_nearest = 
  dat_notifications %>% 
  nearest(dat_parcels) %>% 
  as_tibble

dat_nearest_parcels = 
  dat_parcels %>% 
  as_tibble %>% 
  mutate(ROW = row_number()) %>% 
  semi_join(dat_nearest_flat, by = c("ROW" = "to_id"))

dat_nearest_notifications = 
  dat_notifications %>% 
  as_tibble %>% 
  select(
    UID, 
    NOAPID, 
    Landowner_Company, 
    DateStart # Using a year-quarter from change detection would be better.
  ) %>%  
  mutate(
    Year_Quarter = paste0(DateStart %>% year, "_", DateStart %>% quarter),
    Row = row_number()
  ) %>% 
  left_join(dat_nearest %>% select(Row = from_id, Parcel_Row = to_id)) %>% 
  left_join(dat_nearest_parcels %>% select(Parcel_Row = ROW, Parcel = PARCEL)) %>% 
  select(-ends_with("Row"), -DateStart) %T>% 
  write_csv("03_intermediate/dat_notifications_parcels.csv")

#  Get owners. 

dat_parcel_clip = "03_intermediate/dat_pb_parcels.csv" %>% read_csv # change this to grab new parcel-PB results. 
  
dat_owners = "03_intermediate/data_cotality_explicit.csv" %>% read_csv

dat_nearest_notifications_owners = 
  dat_nearest_notifications %>% 
  left_join(dat_parcel_clip, by = c("Parcel" = "PARCEL")) %>% # Note many-many.
  left_join(dat_owners, by = c("CLIP", "Year_Quarter" = "YEAR_QUARTER")) %>% # Note NA for 2014.
  # Deal with ambiguous parcel-CLIP-owner matches.
  # For now, Keep first owner in alphabetical order. This is wholly arbitrary. 
  arrange(UID, OWNER) %>% 
  group_by(UID) %>% 
  filter(row_number() == 1) %>% 
  ungroup %>% 
  # Clean up and export. 
  select(UID, NOAPID, Owner_FERNS = Landowner_Company, Owner_Cotality = OWNER, Year_Quarter) %T>% 
  write_csv("03_intermediate/dat_notifications_owners.csv")

# Export as a modified version of 1_3.

"03_intermediate/dat_notifications_1_3.csv" %>% 
  read_csv %>% 
  left_join(dat_nearest_notifications_owners %>% select(UID, NOAPID, Owner_Cotality)) %T>% 
  write_csv("03_intermediate/dat_notifications_1_3_X.csv")
  
#  (2) 

# get owners from pb/ot join
# subset by owners matched to notifications
# reduce parcel polygons
# get acreage
# maybe get acreage from original parcel data to avoid a hassle
# join acreage onto owner-quarter panel; then this only comes around again in 1_7 or later

dat_acres = 
  "02_data/0_0_0_Cotality/1_Parcels/2020_shapefile" %>% # Watch out for invalid geometries.
  vect %>% 
  as_tibble %>% 
  select(PARCEL = OBJECTID, METERS = Shape_Area) %>% 
  left_join(dat_parcel_clip) %>% 
  drop_na(CLIP) %>% 
  mutate(ACRES = METERS * 0.00024711) %>% 
  select(CLIP, ACRES)
  
vec_owners = 
  dat_nearest_notifications_owners %>% 
  drop_na(Owner_Cotality) %>% 
  pull(Owner_Cotality) %>% 
  unique

dat_owners_notifications = 
  dat_owners %>% 
  filter(OWNER %in% vec_owners) %>% 
  left_join(dat_acres)
  
dat_owners_acres = 
  dat_owners_notifications %>% 
  group_by(YEAR_QUARTER, OWNER) %>% 
  summarize(ACRES = sum(ACRES, na.rm = TRUE)) %>% 
  ungroup %>% 
  rename(Year_Quarter = YEAR_QUARTER, Owner_Cotality = OWNER, Owner_Acres = ACRES) %T>% 
  write_csv("03_intermediate/dat_owners_acres.csv")

#  Stop timing. 

time_end = Sys.time()

time_end - time_start
