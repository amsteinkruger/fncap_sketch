# Handle spatial overlaps among notifications; intersections go to later observations. 

#  Clear the environment.

rm(list = ls())

#  Start timing. 

time_start = Sys.time()

#  Set up data. Filtering here is clumsy but convenient.  

dat_prepare = 
  "03_intermediate/dat_notifications_1_3.gdb" %>% 
  vect %>% 
  filter((DateStart %>% year) %in% 2015:2025 & (DateEnd %>% year) %in% 2015:2025) %>% 
  filter(ActivityType == "Clearcut/Overstory Removal") %>% # Implicit in 1_3. 
  filter(ActivityUnit == "MBF") %>%
  filter(str_sub(OperationName, 1, 10) != "do not use") %>% 
  mutate(Acres = expanse(., unit = "ha") * 2.47105381,
         MBF = ActivityQuantity %>% as.numeric,
         MBF_Acre = MBF / Acres) %>% 
  select(UID,
         Landowner = Landowner_Company_Reviewed, 
         DateSubmit, 
         DateStart,
         DateEnd,
         DateCompletion,
         Completion = ActivityStatus,
         Acres,
         MBF,
         MBF_Acre)

#  Set aside attributes. 

dat_prepare_flat = dat_prepare %>% as_tibble

#  Reorder data. 

dat_order = 
  dat_prepare %>% 
  group_by(Landowner) %>% 
  mutate(Acres_Landowner = Acres %>% sum,
         MBF_Landowner = MBF %>% sum) %>% 
  ungroup %>% 
  arrange(desc(DateStart),
          desc(DateEnd),
          desc(DateSubmit),
          desc(MBF),
          desc(Acres),
          desc(MBF_Landowner),
          desc(Acres_Landowner)) %>% 
  select(UID) %>% 
  makeValid(buffer = TRUE) 

#  Intersect data. 

dat_intersect = 
  dat_order %>% 
  mutate(Order = row_number()) %>% # Standing in for dates, etc. 
  intersect(., .) %>% 
  makeValid(buffer = TRUE) 

#  Set aside intersections. 

dat_erase = 
  dat_intersect %>% 
  filter(Order_1 < Order_2) %>% # Get intersections where UID_1 is later. 
  group_by(UID_2) %>% # Match those intersections to UID_2. 
  summarize() %>% 
  ungroup %>% 
  rename(UID = UID_2) %>% 
  makeValid(buffer = TRUE) 

#  Erase intersections from all but the latest notification, then export. 

dat_clean = 
  dat_order %>% 
  as_tibble %>% 
  mutate(data_original = 
           UID %>% 
           map(~ filter(dat_order, UID == .x)),
         data_intersections = 
           UID %>% 
           map(~ filter(dat_erase, UID == .x)),
         data_erase = 
           map2(data_original,
                data_intersections,
                erase), 
         data_null = 
           map(data_erase,
               ~ nrow(as_tibble(.x)))) %>% 
  unnest(data_null) %>% 
  filter(data_null > 0) %>% 
  pull(data_erase) %>% 
  bind_spat_rows %>% 
  makeValid(buffer = TRUE) %>% 
  left_join(dat_prepare_flat) %>% 
  mutate(Acres_Less = expanse(., unit = "ha") * 2.47105381, 
         Acres_Change = Acres_Less - Acres,
         MBF_Less = MBF * (Acres_Less / Acres),
         MBF_Change = MBF_Less - MBF,
         MBF_Acre_Less = MBF_Less / Acres_Less) %>% 
  filter(Acres_Less > 1) %>% 
  arrange(UID) %>% 
  select(UID,
         Landowner,
         DateSubmit,
         DateStart,
         DateEnd,
         DateCompletion,
         Completion,
         Acres_0 = Acres,
         MBF_0 = MBF,
         MBF_Acre_0 = MBF_Acre,
         Acres_1 = Acres_Less,
         MBF_1 = MBF_Less,
         MBF_Acre_1 = MBF_Acre_Less) %T>% 
  # Export with spatial data. 
  writeVector("03_intermediate/dat_notifications_1_4.gdb") %>% 
  # Export without spatial data. 
  as_tibble %T>% 
  write_csv("03_intermediate/dat_notifications_1_4.csv")

#  Stop timing. 

time_end = Sys.time()

time_end - time_start
