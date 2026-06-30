# Handle spatial overlaps among notifications by activity; intersections go to later notifications. 

#  Clear the environment.

rm(list = ls())

#  Start timing. 

time_start = Sys.time()

#  Set up data. Filtering here is clumsy but convenient.  

vec_activities = c("Clearcut/Overstory Removal", "Commercial Thinning/Selective Cutting", "Salvage")

dat_prepare = 
  "03_intermediate/dat_notifications_1_3.gdb" %>% 
  vect %>% 
  filter((DateStart %>% year) %in% 2015:2025 & (DateEnd %>% year) %in% 2015:2025) %>% 
  filter(ActivityType %in% vec_activities) %>% 
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
         Activity = ActivityType,
         Acres,
         MBF,
         MBF_Acre)

#  Set aside attributes. 

dat_prepare_flat = dat_prepare %>% as_tibble

#  Nest data.

dat_nest = 
  vec_activities %>% 
  as_tibble %>% 
  rename(Activity = value) %>% 
  mutate(Data = list(dat_prepare)) %>% 
  mutate(Data = 
           Data %>% 
           map2(Activity,
                ~ filter(.x, Activity == .y)))

#  Reorder data.

dat_order = 
  dat_nest %>% 
  mutate(Data = 
           Data %>% 
           map(~ group_by(.x, Landowner)) %>% 
           map(~ mutate(.x,
                        Acres_Landowner = Acres %>% sum,
                        MBF_Landowner = MBF %>% sum)) %>% 
           map(~ ungroup(.x)) %>% 
           map(~ arrange(.x,
                         desc(DateStart),
                         desc(DateEnd),
                         desc(DateSubmit),
                         Activity,
                         desc(MBF),
                         desc(Acres),
                         desc(MBF_Landowner),
                         desc(Acres_Landowner))) %>% 
           map(~ select(.x, UID)) %>% 
           map(~ makeValid(.x, buffer = TRUE)))

#  Intersect data. 

dat_intersect = 
  dat_order %>% 
  mutate(Data = 
           Data %>% 
           map(~ mutate(.x, Order = row_number())) %>% 
           map(~ intersect(.x, .x)) %>% 
           map(~ makeValid(.x, buffer = TRUE)))

#  Set aside intersections. 

dat_erase = 
  dat_intersect %>% 
  mutate(Data = 
           Data %>% 
           map(~ filter(.x, Order_1 < Order_2)) %>% 
           map(~ group_by(.x, UID_2)) %>% 
           map(~ summarize(.x)) %>% 
           map(~ ungroup(.x)) %>% 
           map(~ rename(.x, UID = UID_2)) %>% 
           map(~ makeValid(.x, buffer = TRUE)))

#  Erase intersections from all but the latest notification, then export. 

dat_clean = 
  dat_order %>% 
  mutate(Data = Data %>% map(as_tibble)) %>% 
  left_join(dat_order %>% rename(Data_Order = Data)) %>% 
  left_join(dat_erase %>% rename(Data_Erase = Data)) %>% 
  unnest(Data) %>% 
  mutate(Data_Original = map2(Data_Order, UID, ~ filter(.x, UID == .y)),
         Data_Intersections = map2(Data_Erase, UID, ~ filter(.x, UID == .y)),
         Data_Erase = map2(Data_Original, Data_Intersections, erase),
         Data_Null = map(Data_Erase, ~ nrow(as_tibble(.x)))) %>% 
  unnest(Data_Null) %>% 
  filter(Data_Null > 0) %>% 
  pull(Data_Erase) %>% 
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
         Activity,
         Acres_0 = Acres,
         MBF_0 = MBF,
         MBF_Acre_0 = MBF_Acre,
         Acres_1 = Acres_Less,
         MBF_1 = MBF_Less,
         MBF_Acre_1 = MBF_Acre_Less) %T>% 
  # Export with spatial data. 
  writeVector("03_intermediate/dat_notifications_1_4_x.gdb") %>% 
  # Export without spatial data. 
  as_tibble %T>% 
  write_csv("03_intermediate/dat_notifications_1_4_x.csv")

#  Stop timing. 

time_end = Sys.time()

time_end - time_start
