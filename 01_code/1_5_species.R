# Join data on forest type to restrict the data to Douglas fir.

#  The ideal here might be to: 
#   Get the subset of cells in each rasterized polygon that are forest/timber. 
#   Get the proportions of forest/timber cells that are Douglas fir or hemlock. 
#   Transform MBF into Douglas fir, western hemlock, and "other" timber. 
#   Note that condition-level forest type != proportions of trees in tree table. 

#  No idea how to defend that, but it's a cleaner connection between FIA and FERNS. 

#  Clear the environment.

rm(list = ls())

#  Start timing. 

time_start = Sys.time()

#  Notifications

dat_notifications = 
  "03_intermediate/dat_notifications_1_4.gdb" %>% 
  vect %>% 
  filter(is.valid(.)) 
  
# This filter is important: it costs ~ 1/6 obs. to maintain valid polygons. 

dat_notifications_less = 
  dat_notifications %>% 
  select(UID)

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
  project("EPSG:2992")

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

dat_notifications_treemap = 
  dat_notifications %>% 
  left_join(dat_join_treemap) %T>% 
  # Export with spatial data. 
  writeVector("03_intermediate/dat_notifications_1_5.gdb") %>% 
  # Export without spatial data. 
  as_tibble %T>% 
  write_csv("03_intermediate/dat_notifications_1_5.csv")

#  Stop timing. 

time_end = Sys.time()

time_end - time_start
