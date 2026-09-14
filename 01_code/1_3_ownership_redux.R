# Handle land ownership. 

#  (1) Assign owner strings to notifications by nearest neighbors (notifications-parcels).
#  (2) Assign acres to owners. 

#  Clear the environment.

# rm(list = ls())

#  Start timing. 

time_start = Sys.time()

#  (1) 

#  Set up notifications for nearest-neighbor computation.

#   Pick activities to keep.

vec_activities = c("Clearcut/Overstory Removal", "Commercial Thinning/Selective Cutting", "Salvage")

#   Pick landowners to keep. This hides extensive identification of landowners by hand. 

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

dat_nearest = nearest(dat_notifications, dat_parcels)

#  Set up PB/OT owners to join onto notifications.

#  Join. 

#  (2) 

# get owners from pb/ot join
# reduce parcel polygons
# get acreage
# maybe get acreage from original parcel data to avoid a hassle
# join acreage onto owner-quarter panel; then this only comes around again in 1_7 or later

#  Stop timing. 

time_end = Sys.time()

time_end - time_start
