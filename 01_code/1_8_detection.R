# Detect clearcuts, thinning, and salvage. 

# TCC

#  Set up notifications to support comparisons between pre- and post-years.

dat_notifications_tcc = 
  dat_notifications %>% 
  filter(Year_End < 2023) %>% 
  as_tibble %>% 
  mutate(Year_Before = Year_Start - 1,
         Year_After = Year_End + 1) %>% 
  select(UID, Year_Before, Year_After) %>% 
  pivot_longer(starts_with("Year"),
               names_prefix = "Year_",
               names_to = "Period",
               values_to = "Year")

#  Extract TCC to notifications, then subset results for comparisons of interest.

dat_join_tcc = 
  "output/data_tcc.tif" %>% 
  rast %>% 
  extract(., 
          dat_notifications, 
          mean, 
          na.rm = TRUE) %>% 
  bind_cols((dat_notifications$UID %>% tibble(UID = .))) %>% 
  select(-ID) %>% 
  as_tibble %>% 
  left_join((dat_notifications %>% as_tibble %>% select(UID)), ., by = "UID") %>% 
  pivot_longer(cols = starts_with("TCC"),
               names_prefix = "TCC_",
               names_to = "Year",
               values_to = "TCC") %>% 
  group_by(UID) %>% 
  nest(data = c(Year, TCC)) %>% 
  ungroup %>% 
  left_join(dat_notifications_tcc, .) %>% 
  mutate(data = data %>% map2(.x = ., .y = Year, .f = ~ filter(.x, Year == .y))) %>% 
  select(-Year) %>% 
  unnest(data) %>% 
  mutate(TCC_Change = TCC - lag(TCC)) %>% 
  filter(Period == "After") %>% 
  select(UID, TCC_Change)

# NDVI

dat_join_ndvi_read = "output/data_ndvi.tif" %>% rast

#  Annual

#  Set up notifications to support comparisons between pre- and post-years.

dat_notifications_ndvi_annual =
  dat_notifications %>%
  filter(Year_End < 2024) %>%
  as_tibble %>%
  mutate(Year_Before = Year_Start - 1,
         Year_After = Year_End + 1) %>%
  select(UID, Year_Before, Year_After) %>%
  pivot_longer(starts_with("Year"),
               names_prefix = "Year_",
               names_to = "Period",
               values_to = "Year")

#  Extract NDVI to notifications, then subset results for comparisons of interest.

# Remember to add a switch here to read output if output exists and otherwise annualize data and export.

dat_join_ndvi_annual = "output/data_ndvi_annual.tif" %>% rast

dat_join_ndvi_annual = 
  # tibble(year = 2014:2024) %>% 
  # mutate(string = paste0("NDVI_", year)) %>% 
  # mutate(data = string %>% map( ~ { dat_join_ndvi_read %>% select(starts_with(.x)) %>% mean(na.rm = TRUE) })) %>% # Get annual means.
  # mutate(data = 
  #          data %>% 
  #          map2(.x = ., 
  #               .y = string, 
  #               ~ {
  #                 names(.x) <- as.character(.y)
  #                 .x
  #               })) %>% # Restore names.
  # magrittr::extract2("data") %>% # Equivalent to .$data.
  # reduce(c) %>% 
  # export and switch goes here
  dat_join_ndvi_annual %>% 
  extract(., 
          dat_notifications_less_1, 
          mean, 
          na.rm = TRUE) %>% 
  bind_cols((dat_notifications_less_1$UID %>% tibble(UID = .))) %>% 
  select(-ID) %>% 
  as_tibble %>% 
  left_join(dat_notifications_less_1 %>% as_tibble, ., by = "UID") %>% 
  pivot_longer(cols = starts_with("NDVI"),
               names_prefix = "NDVI_",
               names_to = "Year",
               values_to = "NDVI") %>% 
  group_by(UID) %>% 
  nest(data = c(Year, NDVI)) %>% 
  ungroup %>% 
  left_join(dat_notifications_ndvi_annual, .) %>% 
  mutate(data = data %>% map2(.x = ., .y = Year, .f = ~ filter(.x, Year == .y))) %>% 
  select(-Year) %>% 
  unnest(data) %>% 
  mutate(NDVI_Change = NDVI - lag(NDVI)) %>% 
  filter(Period == "After") %>% 
  select(UID, NDVI_Change) 

# Harvest Detection

#  Get public timber sales in a convenient format.

crs_usfs = 
  "data/USFS_Harvest/Actv_TimberHarvest.gdb" %>% 
  vect %>% 
  crs

dat_bounds_usfs = dat_bounds %>% project(crs_usfs)
  
dat_detect_usfs = 
  "data/USFS_Harvest/Actv_TimberHarvest.gdb" %>% 
  vect %>% 
  select(ADMIN_FOREST_NAME,
         ACTIVITY_CN,
         ACTIVITY_UNIT_CN,
         ACTIVITY_CODE,
         ACTIVITY_NAME,
         TREATMENT_TYPE,
         DATE_PLANNED,
         DATE_AWARDED,
         DATE_COMPLETED,
         METHOD_CODE,
         METHOD_DESC,
         EQUIPMENT_CODE,
         EQUIPMENT_DESC,
         LAND_SUITABILITY_CLASS_CODE, 
         LAND_SUITABILITY_CLASS_DESC, 
         PRODUCTIVITY_CLASS_CODE, 
         PRODUCTIVITY_CLASS_DESC, 
         OWNERSHIP_CODE, 
         OWNERSHIP_DESC, 
         ASPECT, 
         ELEVATION, 
         SLOPE, 
         STATE_ABBR) %>% 
  filter(STATE_ABBR == "OR") %>% 
  filter(ACTIVITY_NAME == "Commercial Thin") %>% 
  filter(year(DATE_COMPLETED) %in% 2015:2024) %>%
  filter(year(DATE_COMPLETED) - year(DATE_AWARDED) == 0) %>% 
  crop(dat_bounds_usfs) %>% 
  project("EPSG:2992") %>% 
  mutate(UID = row_number())

#  Get TCC for public timber sales.

#   Set up public timber sales in a convenient format for TCC extraction.

# dat_detect_usfs_tcc = 
#   dat_detect_usfs %>% 
#   as_tibble %>% 
#   mutate(Year = DATE_COMPLETED %>% year,
#          Activity = 1) %>% 
#   filter(Year %in% 2015:2023) %>% 
#   select(UID, Year, Activity)

#   Handle TCC.

# dat_detect_tcc = 
#   "output/data_tcc.tif" %>% 
#   rast %>% 
#   extract(.,
#           dat_detect_usfs, 
#           mean, 
#           na.rm = TRUE) %>% 
#   bind_cols((dat_detect_usfs %>% as_tibble %>% select(UID))) %>% 
#   select(-ID) %>% 
#   as_tibble %>% 
#   left_join((dat_detect_usfs %>% as_tibble %>% select(UID)), ., by = "UID") %>% 
#   pivot_longer(cols = starts_with("TCC"),
#                names_prefix = "TCC_",
#                names_to = "Year",
#                values_to = "TCC") %>% 
#   mutate(Year = Year %>% as.numeric) %>% 
#   left_join(dat_detect_usfs_tcc) %>% 
#   mutate(Activity = ifelse(is.na(Activity), 0, Activity)) %>% 
#   group_by(UID) %>% 
#   mutate(TCC_Change = TCC - ifelse(is.na(lag(TCC, n = 2)), lag(TCC), (lag(TCC) + lag(TCC, n = 2)) / 2)) %>% 
#   ungroup %>% 
#   drop_na(TCC_Change)

#  Get NDVI for public timber sales.

#   Set up public timber sales in a convenient format for NDVI extraction.

dat_detect_usfs_ndvi =
  dat_detect_usfs %>%
  as_tibble %>%
  mutate(Year_Before = DATE_AWARDED %>% year %>% `-` (1),
         Year_Complete = DATE_COMPLETED %>% year,
         Years = map2(Year_Before, Year_Complete, ~ seq(.x, .y))) %>%
  filter(Year_Before %in% 2014:2024 & Year_Complete %in% 2014:2024) %>%
  select(UID, Years) %>%
  unnest(Years)

#   Handle NDVI.

dat_detect_ndvi =
  "output/data_ndvi_annual.tif" %>%
  rast %>%
  extract(.,
          dat_detect_usfs,
          mean,
          na.rm = TRUE) %>%
  bind_cols((dat_detect_usfs %>% as_tibble %>% select(UID))) %>%
  select(-ID) %>%
  as_tibble %>%
  left_join((dat_detect_usfs %>% as_tibble %>% select(UID)), ., by = "UID") %>%
  pivot_longer(cols = starts_with("NDVI"),
               names_prefix = "NDVI_",
               names_to = "Year",
               values_to = "NDVI") %>%
  mutate(Year = Year %>% as.numeric) %>%
  semi_join(dat_detect_usfs_ndvi, by = c("UID", "Year" = "Years")) %>%
  group_by(UID) %>%
  mutate(NDVI_Change = NDVI - lag(NDVI),
         NDVI_Detect = ifelse(NDVI_Change == min(NDVI_Change, na.rm = TRUE) & min(NDVI_Change, na.rm = TRUE) < 0, 1, 0)) %>%
  drop_na(NDVI_Change) %>%
  ungroup

#  Compare NDVI and TCC.

# dat_detect_tcc_ndvi = 
#   dat_detect_ndvi %>% 
#   full_join(dat_detect_tcc, by = c("UID", "Year"))
# 
# val_detect_tcc_ndvi_agreement = 
#   dat_detect_tcc_ndvi %>% 
#   mutate(Agreement = NDVI_Detect * TCC_Detect) %>% 
#   summarize(Check = sum(Agreement) / n_distinct(UID)) %>% 
#   pull(Check)

#  Parameterize a model for harvest detection.

#  Detect harvests in notifications.

#   Get notifications with change in TCC and NDVI in convenient formats.

# dat_detect_notifications_tcc = dat_join_tcc
# dat_detect_notifications_ndvi = dat_join_ndvi_annual

#  Set up detection for joins.

#   Placeholder!

dat_join_detect = 
  dat_join_ndvi_annual %>% 
  mutate(NDVI_Detect = ifelse(NDVI_Change < 0, 1, 0)) %>% 
  left_join(dat_notifications_less_1 %>% as_tibble,
            .)

# Stopwatch

time_end = Sys.time()

time_end - time_start
