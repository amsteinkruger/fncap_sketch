# Join data on forest type to restrict the data to Douglas fir (and whem). 

#  Clear the environment.

rm(list = ls())

# Start timing. 

time_start = Sys.time()

# Notifications

dat_notifications = 
  "03_intermediate/dat_notifications_1_4.gdb" %>% 
  vect %>% 
  mutate(Year_Start = DateStart %>% year,
         Month_Start = DateStart %>% month,
         Year_End = DateEnd %>% year,
         Month_End = DateEnd %>% month,
         MBF = ActivityQuantity %>% as.numeric,
         MBF_Acre = MBF / Acres) %>% 
  # The following lines are applied in earlier scripts. 
  # filter(ActivityType == "Clearcut/Overstory Removal") %>% 
  # filter(ActivityUnit == "MBF") %>%
  # filter(LandOwnerType == "Partnership/Corporate Forestland Ownership") %>% 
  # filter(str_sub(OperationName, 1, 10) != "do not use") %>% 
  filter(MBF_Acre > quantile(MBF_Acre, 0.01) & MBF_Acre < quantile(MBF_Acre, 0.99)) %>% 
  select(UID,
         Landowner = Landowner_Company_Reviewed, 
         Year_Start,
         Month_Start,
         Year_End,
         Month_End, 
         Completion_String = ActivityStatus,
         Completion_Date = DateCompletion, 
         MBF, 
         Acres, 
         MBF_Acre)
  
dat_notifications_less = 
  dat_notifications %>% 
  select(UID)

dat_notifications_flat = 
  dat_notifications_less %>% 
  as_tibble

# TreeMap

#  Get FIA data.

dat_fia_cond = 
  bind_rows("02_data/1_5_1_FIA/CA_COND.csv" %>% 
              read_csv %>% 
              select(CN = PLT_CN, 
                     INVYR, 
                     FORTYPCD, 
                     SITECLCD),
            "02_data/1_5_1_FIA/OR_COND.csv" %>% 
              read_csv %>% 
              select(CN = PLT_CN, 
                     INVYR, 
                     FORTYPCD, 
                     SITECLCD),
            "02_data/1_5_1_FIA/WA_COND.csv" %>% 
              read_csv %>% 
              select(CN = PLT_CN, 
                     INVYR, 
                     FORTYPCD, 
                     SITECLCD)) %>% 
  distinct %>% 
  drop_na %>% # For FORTYPCD and SITECLCD.
  mutate(Join = 1)

#  Get TreeMap data.

#   Handle initial data.

crs_treemap = 
  "02_data/1_5_2_TreeMap_2014/national_c2014_tree_list.tif" %>% 
  rast %>% 
  crs

dat_bounds_treemap = 
  "03_intermediate/dat_bounds.gdb" %>% 
  vect %>% 
  project(crs_treemap)

dat_treemap = 
  "02_data/1_5_2_TreeMap_2014/national_c2014_tree_list.tif" %>% 
  rast %>% 
  crop(dat_bounds_treemap, mask = TRUE) %>% 
  project("EPSG:2992") # %>% 
  # as.numeric # This might matter, but it runs for ~20 minutes (2025/11/25).

vec_treemap = dat_treemap %>% as.vector %>% na.omit %>% unique

#  Get FIA data by way of TreeMap's look-up table. This avoids a confusing raster operation.

dat_treemap_lookup = 
  "02_data/1_5_2_TreeMap_2014/TL_CN_Lookup.txt" %>% 
  read_delim %>% 
  rename(TL_ID = tl_id) %>% 
  select(TL_ID, CN)

dat_treemap_join =
  vec_treemap %>%
  tibble(TL_ID = .) %>%
  left_join(dat_treemap_lookup) %>% 
  left_join(dat_fia_cond)

#  Reclassify Treemap into (1) binary forest types (Douglas Fir / Not) and (2) site class (1-7).

dat_treemap_fortypcd = 
  dat_treemap_join %>% 
  mutate(FORTYPCD_BIN = ifelse(FORTYPCD %in% 201:203, 1, ifelse(!is.na(Join) | !is.na(FORTYPCD), 0, NA))) %>% 
  select(tl_id = TL_ID, FORTYPCD_BIN) %>% 
  as.matrix %>% 
  classify(dat_treemap, .)

dat_treemap_siteclcd = 
  dat_treemap_join %>% 
  select(tl_id = TL_ID, SITECLCD) %>% 
  rename(from = 1, to = 2) %>% 
  as.matrix %>% 
  classify(dat_treemap, .)

#  Extract both results onto notifications for later joins.

dat_join_treemap_fortypcd = 
  dat_notifications_less %>% 
  extract(dat_treemap_fortypcd, ., fun = "mean", na.rm = TRUE) %>% 
  select(ProportionDouglasFir = CN) %>% 
  bind_cols(dat_notifications_less %>% as_tibble, .)

dat_join_treemap_siteclcd_min = 
  dat_notifications_less %>% 
  extract(dat_treemap_siteclcd, ., fun = "min", na.rm = TRUE) %>% 
  select(SiteClass_Min = CN) %>% 
  bind_cols(dat_notifications_less %>% as_tibble, .)

dat_join_treemap_siteclcd_max = 
  dat_notifications_less %>% 
  extract(dat_treemap_siteclcd, ., fun = "max", na.rm = TRUE) %>% 
  select(SiteClass_Max = CN) %>% 
  bind_cols(dat_notifications_less %>% as_tibble, .)

dat_join_treemap_siteclcd_med = 
  dat_notifications_less %>% 
  extract(dat_treemap_siteclcd, ., fun = "median", na.rm = TRUE) %>% 
  select(SiteClass_Med = CN) %>% 
  bind_cols(dat_notifications_less %>% as_tibble, .)

dat_join_treemap_siteclcd_mod = 
  dat_notifications_less %>% 
  extract(dat_treemap_siteclcd, ., fun = "modal", na.rm = TRUE) %>% 
  select(SiteClass_Mod = CN) %>% 
  bind_cols(dat_notifications_less %>% as_tibble, .)

dat_join_treemap = 
  dat_join_treemap_fortypcd %>% 
  left_join(dat_join_treemap_siteclcd_min) %>% 
  left_join(dat_join_treemap_siteclcd_max) %>% 
  left_join(dat_join_treemap_siteclcd_med) %>% 
  left_join(dat_join_treemap_siteclcd_mod)
