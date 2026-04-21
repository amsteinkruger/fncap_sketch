# Bound FERNS to western Oregon by pyromes. 

#  Clear the environment.

rm(list = ls())

#  Start timing. 

time_start = Sys.time()

#  Notifications

dat_notifications = 
  "03_intermediate/dat_notifications_1_1.gdb" %>% 
  vect %>% 
  project("EPSG:2992")

#  Bounds

#   OR

dat_bounds_or = 
  "02_data/1_2_1_Census_States" %>% 
  vect %>% 
  filter(STUSPS == "OR") %>% 
  select(STUSPS) %>% 
  project("EPSG:2992")

#   Pyromes

dat_bounds_pyromes = 
  "02_data/1_2_2_USFS_Pyromes/Data/Pyromes_CONUS_20200206.shp" %>% 
  vect %>% 
  rename(WHICH = NAME) %>% # Band-Aid for a reserved attribute name.
  filter(WHICH %in% c("Marine Northwest Coast Forest", "Klamath Mountains", "Middle Cascades")) %>% 
  select(Pyrome = WHICH) %>% 
  summarize(Pyrome = "All Pyromes") %>% 
  fillHoles %>% 
  project("EPSG:2992")

#   Intersection

dat_bounds = 
  intersect(dat_bounds_or, dat_bounds_pyromes) %>% 
  # Handle island polygons. These are not real islands.
  disagg %>% 
  cbind(., expanse(., unit = "ha")) %>% 
  filter(y == max(y)) %>% 
  select(STUSPS)

#   Relation

dat_notifications_bounds = 
  dat_notifications %>% 
  relate(dat_bounds, 
         relation = "within") %>% 
  cbind(dat_notifications %>% 
          select(UID) %>% 
          as_tibble, 
        .) %>% 
  rename(Within = ".") %>% 
  filter(Within) %>% 
  semi_join(dat_notifications, .) %T>% 
  # Export with spatial data. 
  writeVector("03_intermediate/dat_notifications_1_2.gdb") %>% 
  # Export without spatial data. 
  as_tibble %T>% 
  write_csv("03_intermediate/dat_notifications_1_2.csv")

#  Stop timing. 

time_end = Sys.time()

time_end - time_start
