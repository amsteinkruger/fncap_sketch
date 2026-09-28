# Handle land ownership. 

#  (1) Assign owner strings to notifications by nearest neighbors (notifications-parcels).
#  (2) Assemble land portfolios from landowners associated with notifications. 
#  (3) Assign acres to owners. 
#  (4) Combine results for later joins. 

#  Clear the environment.

rm(list = ls())

#  Start timing. 

time_start = Sys.time()

#  (1) 

#  Set up notifications for nearest-neighbor computation.

#   Pick activities to keep.

vec_activities = c("Clearcut/Overstory Removal", "Commercial Thinning/Selective Cutting", "Salvage")

#   Export landowners for review. 

dat_owners_out =
  "03_intermediate/dat_notifications_1_2.csv" %>%
  read_csv %>%
  filter(ActivityType %in% vec_activities) %>% 
  select(Landowner_Company) %>%
  distinct %>%
  arrange(Landowner_Company) %T>%
  write_xlsx("03_intermediate/dat_owners_out.xlsx")

#   Pick landowners to keep. This hides extensive identification of landowners by hand. 
#    Note that this is a holdover from the pre-Cotality approach. 
#    Also note that this object is overwritten later in the script -- leaving it for now. 

dat_owners = 
  "03_intermediate/dat_owners_in.xlsx" %>% 
  read_xlsx %>%
  filter(Landowner_Private == 1) %>%
  drop_na(Landowner_Company_Reviewed) %>%
  select(1:2)

#   Set up counties.

dat_counties = 
  "02_data/1_6_6_TIGER/TIGER.gdb" %>% 
  vect(layer = "County") %>% 
  select(County = NAMELSAD, FIPS_Code = GEOID) %>% 
  project("EPSG:2992")

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
  centroids %>% 
  # Counties
  intersect(dat_counties)

dat_notifications_nest = 
  dat_notifications %>% 
  as_tibble %>% 
  distinct(FIPS_Code) %>% 
  arrange(FIPS_Code) %>% 
  mutate(Data_Notifications = FIPS_Code %>% map(~ filter(dat_notifications, FIPS_Code == .x)))
  
#  Set up parcels for nearest-neighbor computation. 

dat_parcels = 
  "03_intermediate/dat_parcels_points.gdb" %>% 
  vect %>% 
  project("EPSG:2992") %>% 
  left_join("03_intermediate/dat_crosswalk_counties.csv" %>% read_csv) %>% 
  rename(FIPS_Code = FIPS_CODE) %>% 
  mutate(FIPS_Code = FIPS_Code %>% as.character)
  
dat_parcels_nest = 
  dat_parcels %>% 
  as_tibble %>% 
  distinct(FIPS_Code) %>% 
  arrange(FIPS_Code) %>% 
  mutate(Data_Parcels = FIPS_Code %>% map(~ filter(dat_parcels, FIPS_Code == .x)))

#  Get nearest neighbors (parcels to notifications).

#   Run NN over counties. 

dat_nearest = 
  dat_notifications_nest %>% 
  left_join(dat_parcels_nest) %>% 
  mutate(Data_Nearest = map2(Data_Notifications, Data_Parcels, nearest)) %>% 
  mutate(
    Data_Out = 
      Data_Nearest %>% 
      map(as_tibble) %>% 
      map(~ select(.x, ends_with("id"))) %>% 
      map2(
        Data_Notifications, 
        ~ left_join(
          .x, 
          .y %>% as_tibble %>% select(UID) %>% mutate(from_id = row_number())
        )
      ) %>% 
      map2(
        Data_Parcels, 
        ~ left_join(
          .x, 
          .y %>% as_tibble %>% select(PARCEL) %>% mutate(to_id = row_number())
        )
      ) %>% 
      map(~ select(.x, UID, PARCEL))
  ) %>% 
  select(Data_Out) %>% 
  unnest(Data_Out) %>% 
  left_join(
    dat_notifications %>% 
      as_tibble %>% 
      select(UID, NOAPID, Landowner_Company, DateStart) %>% # Note that quarter of harvest would be ideal. 
      mutate(Year_Quarter = paste0(DateStart %>% year, "_", DateStart %>% quarter)) %>% 
      select(-DateStart)
    ) %>% 
  relocate(PARCEL, .after = "Year_Quarter") %T>% 
  write_csv("03_intermediate/dat_notifications_parcels.csv")

#  Get owners. 

dat_parcel_clip = "03_intermediate/dat_parcels_pb.csv" %>% read_csv 
  
dat_owners = "03_intermediate/data_cotality_explicit.csv" %>% read_csv

dat_notifications_owners = 
  dat_nearest %>% 
  left_join(dat_parcel_clip) %>% 
  left_join(dat_owners, by = c("CLIP", "Year_Quarter" = "YEAR_QUARTER")) %>% # NA via PB. 
  # Clean up and export. 
  select(UID, NOAPID, Owner_FERNS = Landowner_Company, Owner_Cotality = OWNER, Year_Quarter) %T>% 
  write_csv("03_intermediate/dat_notifications_owners.csv")
  
#  (2) 

dat_parcels_polygons = 
  "03_intermediate/dat_parcels_polygons.gdb" %>% 
  vect %>% 
  project("EPSG:2992") %>% 
  select(PARCEL)

dat_portfolios = 
  dat_parcels_polygons %>% 
  left_join(
    dat_notifications_owners %>% 
      distinct(Owner_Cotality) %>% 
      semi_join(dat_owners, ., by = c("OWNER" = "Owner_Cotality")) %>% 
      left_join(dat_parcel_clip)
  ) %>% 
  drop_na(CLIP) %>% 
  select(Owner_Cotality = OWNER, Year_Quarter = YEAR_QUARTER, CLIP, Parcel = PARCEL) %>% 
  arrange(Owner_Cotality, Year_Quarter, CLIP, Parcel) %T>% 
  writeVector("03_intermediate/dat_portfolios.gdb")

#  (3)

dat_acres = 
  "02_data/0_0_0_Cotality/1_Parcels/2020_shapefile" %>% # Watch out for invalid geometries.
  vect %>% 
  as_tibble %>% 
  select(PARCEL = OBJECTID, METERS = Shape_Area) %>% # Meters are assumed. This would be worth computing. 
  left_join(dat_parcel_clip) %>% 
  drop_na(CLIP) %>% 
  mutate(ACRES = METERS * 0.00024711) %>% 
  select(CLIP, ACRES)
  
vec_owners = 
  dat_notifications_owners %>% 
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

#  (4)

dat_join = 
  dat_notifications_owners %>% 
  group_by(Owner_FERNS, Owner_Cotality) %>% 
  summarize(Owner_Cotality_Count = n()) %>% 
  group_by(Owner_FERNS) %>% 
  arrange(desc(Owner_Cotality_Count)) %>% 
  slice_head(n = 1) %>% 
  ungroup %>% 
  select(Owner_FERNS, Owner_Cotality_Frequent = Owner_Cotality) %>% 
  left_join(dat_owners_acres, by = c("Owner_Cotality_Frequent" = "Owner_Cotality")) %T>% 
  write_csv("03_intermediate/dat_owners_join.csv")

#  Stop timing. 

time_end = Sys.time()

time_end - time_start
