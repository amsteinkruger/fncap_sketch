# Count notifications by proportional species assignments.  

# Data

dat_counties = 
  "02_data/1_6_6_TIGER/TIGER.gdb" %>% 
  vect(layer = "County") %>% 
  select(County = NAMELSAD) %>% 
  project("EPSG:2992")

dat_bounds = 
  "03_intermediate/dat_bounds.gdb" %>% 
  vect

dat_pyrome = 
  "02_data/1_2_2_USFS_Pyromes/Data/Pyromes_CONUS_20200206.shp" %>% 
  vect %>% 
  rename(WHICH = NAME) %>% # Band-Aid for a reserved attribute name.
  filter(WHICH %in% c("Marine Northwest Coast Forest", "Klamath Mountains", "Middle Cascades")) %>% 
  select(Pyrome = WHICH) %>% 
  project("EPSG:2992") %>% 
  crop(dat_bounds)

dat_notifications = 
  "03_intermediate/dat_notifications_1_6.csv" %>% 
  read_csv
