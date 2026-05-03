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
  vect # %>% 
  # makeValid(buffer = TRUE) %>% 
  # filter(is.valid(.))

dat_notifications_less = 
  dat_notifications %>% 
  select(UID)

# TreeMap

#  Get FIA data.

#   Trees

dat_fia_tree = 
  bind_rows("02_data/1_5_1_FIA/CA_TREE.csv" %>% 
              read_csv,
            "02_data/1_5_1_FIA/OR_TREE.csv" %>% 
              read_csv,
            "02_data/1_5_1_FIA/WA_TREE.csv" %>% 
              read_csv) %>% 
  select(CN = PLT_CN, 
         TREE, 
         SPCD,
         VOLBFNET,
         TPA_UNADJ) %>% 
  filter(VOLBFNET > 0) %>% 
  mutate(SPCD_USE = 
           case_when(SPCD %in% 201:202 ~ "DouglasFir",
                     SPCD == 263 ~ "WesternHemlock",
                     TRUE ~ "Other"),
         VOLBFNET_ACRE = VOLBFNET * TPA_UNADJ) %>% 
  group_by(CN, SPCD_USE) %>% 
  summarize(VOLBFNET_ACRE = VOLBFNET_ACRE %>% sum,
            .groups = "drop_last") %>% 
  ungroup %>% 
  group_by(CN) %>% 
  mutate(Species_Proportion = VOLBFNET_ACRE / sum(VOLBFNET_ACRE)) %>% 
  ungroup %>% 
  select(-VOLBFNET_ACRE) %>% 
  pivot_wider(names_from = SPCD_USE, 
              values_from = Species_Proportion, 
              names_prefix = "Proportion") %>% 
  mutate(across(starts_with("Proportion"), ~ replace_na(.x, 0)))
  
#  Conditions

dat_fia_cond = 
  bind_rows("02_data/1_5_1_FIA/CA_COND.csv" %>% read_csv,
            "02_data/1_5_1_FIA/OR_COND.csv" %>% read_csv,
            "02_data/1_5_1_FIA/WA_COND.csv" %>% read_csv) %>% 
  select(CN = PLT_CN, 
         FORTYPCD, 
         SITECLCD) %>%
  drop_na %>% 
  distinct %>% 
  group_by(CN) %>% 
  filter(n() == 1) %>% 
  ungroup

#  Join

dat_fia = left_join(dat_fia_cond, dat_fia_tree)

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
  left_join(dat_fia)

#  Reclassify Treemap into 
#   (1) binary forest types (Douglas Fir / Not) 
#   (2) site class (1-7).
#   (3) proportions of Douglas fir (from all sawlog timber by MBF)
#   (4) proportions of western hemlock ("")

dat_treemap_fortypcd = 
  dat_treemap_join %>% 
  mutate(FORTYPCD_BIN = ifelse(FORTYPCD %in% 201:203, 1, ifelse(!is.na(FORTYPCD), 0, NA))) %>% 
  select(tl_id = TL_ID, FORTYPCD_BIN) %>% 
  as.matrix %>% 
  classify(dat_treemap, .)

dat_treemap_siteclcd = 
  dat_treemap_join %>% 
  select(tl_id = TL_ID, SITECLCD) %>% 
  rename(from = 1, to = 2) %>% 
  as.matrix %>% 
  classify(dat_treemap, .)

dat_treemap_proportiondouglasfir = 
  dat_treemap_join %>% 
  select(tl_id = TL_ID, ProportionDouglasFir) %>% 
  as.matrix %>% 
  classify(dat_treemap, .)

dat_treemap_proportionwesternhemlock = 
  dat_treemap_join %>% 
  select(tl_id = TL_ID, ProportionWesternHemlock) %>% 
  as.matrix %>% 
  classify(dat_treemap, .)

#  Extract both results onto notifications for later joins.

dat_join_treemap_fortypcd = 
  dat_notifications_less %>% 
  extract(dat_treemap_fortypcd, ., fun = "mean", na.rm = TRUE) %>% 
  select(ProportionDouglasFirCondition = CN) %>% 
  bind_cols(dat_notifications_less %>% as_tibble, .)

dat_join_treemap_siteclcd = 
  dat_notifications_less %>% 
  extract(dat_treemap_siteclcd, ., fun = "modal", na.rm = TRUE) %>% 
  select(SiteClassMode = CN) %>% 
  bind_cols(dat_notifications_less %>% as_tibble, .)

dat_join_treemap_proportiondouglasfir = 
  dat_notifications_less %>% 
  extract(dat_treemap_proportiondouglasfir, ., fun = "mean", na.rm = TRUE) %>% 
  select(ProportionDouglasFirTree = CN) %>% 
  bind_cols(dat_notifications_less %>% as_tibble, .)

dat_join_treemap_proportionwesternhemlock = 
  dat_notifications_less %>% 
  extract(dat_treemap_proportionwesternhemlock, ., fun = "mean", na.rm = TRUE) %>% 
  select(ProportionWesternHemlockTree = CN) %>% 
  bind_cols(dat_notifications_less %>% as_tibble, .)

dat_join_treemap = 
  dat_join_treemap_fortypcd %>% 
  left_join(dat_join_treemap_siteclcd) %>%
  left_join(dat_join_treemap_proportiondouglasfir) %>%
  left_join(dat_join_treemap_proportionwesternhemlock)

dat_notifications_treemap = 
  dat_notifications %>% 
  left_join(dat_join_treemap) %T>% 
  #  This is where you could adjust acres, MBF, MBF_Acre. 
  # Export with spatial data. 
  writeVector("03_intermediate/dat_notifications_1_5.gdb") %>% 
  # Export without spatial data. 
  as_tibble %T>% 
  write_csv("03_intermediate/dat_notifications_1_5.csv")

#  Stop timing. 

time_end = Sys.time()

time_end - time_start
