# Join data on public lands to restrict FERNS to notifications on private land. 

#  Clear the environment.

rm(list = ls())

# Start timing. 

time_start = Sys.time()

# Notifications

dat_notifications = 
  "03_intermediate/dat_notifications_1_4.gdb" %>% 
  vect
  
dat_notifications_less = 
  dat_notifications %>% 
  select(UID)

dat_notifications_flat = 
  dat_notifications_less %>% 
  as_tibble

# Public Lands

crs_pad = 
  "02_data/1_5_3_PADUS/PADUS4_1_StateOR.gdb" %>% 
  vect %>% 
  crs

dat_bounds_pad = 
  "03_intermediate/dat_bounds.gdb" %>% 
  vect %>% 
  project(crs_pad)

dat_join_pad_public = 
  "02_data/1_5_3_PADUS/PADUS4_1_StateOR.gdb" %>% 
  vect %>% 
  filter(Own_Type != "PVT") %>% 
  select(Shape_Area) %>% 
  aggregate %>% 
  crop(dat_bounds_pad) %>% 
  project("EPSG:2992") %>% 
  intersect(dat_notifications %>% 
              select(UID, Acres), 
            .) %>% 
  mutate(Acres_Intersect = expanse(., unit = "ha") * 2.47) %>% 
  as_tibble %>% 
  mutate(PAD_Public_Proportion = Acres_Intersect / Acres,
         PAD_Public_Binary = 1) %>% 
  select(UID, starts_with("PAD")) %>% 
  left_join(dat_notifications_flat, .) %>% 
  mutate(across(starts_with("PAD"), ~ replace_na(.x, 0)))
