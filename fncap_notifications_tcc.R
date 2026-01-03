# TCC
#  ## minutes and 76? GB in memory for 2014-2024 (2026/01/02).

# Get CRS.

crs_tcc = 
  "data/TCC/TCC_Science_2014/science_tcc_conus_wgs84_v2023-5_20140101_20141231.tif" %>% 
  rast %>% 
  crs

# Get bounds.

dat_bounds_tcc = dat_bounds %>% project(crs_tcc) # Note busted dependency on notifications_more.

# Get data.

dat_tcc = 
  list.files("data/TCC") %>% 
  tibble(file = .) %>% 
  filter(file %>% str_sub(1, 7) == "TCC_Sci") %>% 
  mutate(year = file %>% str_sub(-4, -1) %>% as.numeric,
         layer = paste0("TCC_", year)) %>% 
  arrange(year) %>% 
  mutate(path = paste0("data/TCC/", file, "/science_tcc_conus_wgs84_v2023-5_", year, "0101_", year, "1231.tif")) %>% # Fragile!
  mutate(data = 
           path %>% 
           map(rast) %>% 
           map(as.numeric) %>% 
           map(crop, dat_bounds_tcc, mask = TRUE) %>%
           map2(.x = ., 
                .y = layer, 
                ~ {
                  names(.x) <- as.character(.y)
                  .x
                })) %>% 
  select(year, data)

# Export data. 

dat_tcc_stack = 
  dat_ndvi$data %>% 
  reduce(c) %>% 
  map(project, "EPSG:2992") %T>% 
  writeRaster("output/data_tcc.tif", filetype = "GTiff", overwrite = TRUE)

# Check export.

dat_check = "output/data_tcc.tif" %>% rast

# End current code.

# dat_tcc =
#   list.files("data/TCC") %>%
#   tibble(file = .) %>%
#   filter(file %>% str_sub(1, 7) == "TCC_Sci") %>%
#   mutate(year = file %>% str_sub(-4, -1) %>% as.numeric) %>%
#   filter(year %in% 2015:2018) %>% # Band-Aid.
#   rename(folder = file) %>% # Fiddling around.
#   mutate(file = paste0("data/TCC/", folder, "/science_tcc_conus_wgs84_v2023-5_", year, "0101_", year, "1231.tif")) %>% # Fragile!
#   mutate(data_0 =
#            file %>%
#            map(rast) %>%
#            map(as.numeric) %>%
#            map(crop,
#                dat_bounds_tcc,
#                mask = TRUE) %>%
#            map(project,
#                "EPSG:2992"),
#          data_1 = data_0 %>% lag) %>%
#   filter(year > min(year)) %>% # Avoid a frustrating problem with NULL.
#   mutate(data_difference = map2(data_0,
#                                 data_1,
#                                 ~ .x - .y)) %>% # ifelse(is.null(.y), "This is a null!", ~ .x - .y)
#   select(year, starts_with("data"))
# 
# dat_join_tcc = 
#   dat_notifications %>% 
#   pull(Year) %>% 
#   unique %>% 
#   sort %>% 
#   tibble(year = .) %>% 
#   mutate(notifications = 
#            year %>% 
#            map(~ filter(dat_notifications, Year == .x))) %>% 
#   inner_join(dat_tcc) %>% 
#   mutate(notifications_0 = 
#            map2(data_0,
#                 notifications,
#                 extract,
#                 fun = mean,
#                 ID = FALSE,
#                 bind = TRUE) %>% 
#            map(rename,
#                TCC_0 = category) %>% 
#            map(as_tibble) %>% 
#            map(select, UID, TCC_0),
#          notifications_1 = 
#            map2(data_1,
#                 notifications,
#                 extract,
#                 fun = mean,
#                 ID = FALSE,
#                 bind = TRUE) %>% 
#            map(rename,
#                TCC_1 = category) %>% 
#            map(as_tibble) %>% 
#            map(select, UID, TCC_1),
#          notifications_d = 
#            map2(data_difference,
#                 notifications,
#                 extract,
#                 fun = mean,
#                 ID = FALSE,
#                 bind = TRUE) %>% 
#            map(rename,
#                TCC_D = category) %>% 
#            map(as_tibble) %>% 
#            map(select, UID, TCC_D)) %>% 
#   select(year, starts_with("notifications_")) %>% 
#   mutate(notifications = map2(notifications_0, notifications_1, full_join),
#          notifications = map2(notifications, notifications_d, full_join)) %>% 
#   select(year, notifications) %>% 
#   unnest(notifications)

# Ad hoc check

# Annual histograms of TCC change for each year for the full study area (inclusive of notifications)

# dat_tcc %>% 
#   # filter(year %in% 2015:2017) %>% 
#   mutate(data_difference_out = 
#            data_difference %>% 
#            map(as.vector) %>% 
#            map(as_tibble) %>% 
#            map(drop_na)) %>% 
#   select(year, data_difference_out) %>% 
#   unnest(data_difference_out) %>% 
#   rename(tcc = value) %>% 
#   group_by(year) %>% 
#   mutate(tcc_decile = tcc %>% ntile(100)) %>% 
#   ungroup %>% 
#   filter(tcc_decile %in% 6:96) %>% 
#   mutate(bin = cut_interval(tcc, n = 20)) %>% # cut_width(tcc, width = 25.5, boundary = -255)
#   group_by(year, bin) %>% 
#   summarize(count = n()) %>% 
#   ggplot() +
#   geom_col(aes(x = count,
#                y = bin)) +
#   facet_wrap(~ year) +
#   labs(x = "Pixels", 
#        y = "Binned Interannual Change in TCC") +
#   theme_minimal() +
#   theme(axis.text.x = element_blank(),
#         axis.ticks.x = element_blank())
# 
# ggsave("output/vis_tcc_histogram_all_20251030.png",
#        dpi = 300,
#        width = 7.5,
#        height = 7.5)

# Annual histograms of TCC change for each year for each notification

# dat_join_tcc %>% 
#   rename(tcc = TCC_D) %>% 
#   group_by(year) %>% 
#   mutate(tcc_decile = tcc %>% ntile(100)) %>% 
#   ungroup %>% 
#   filter(tcc_decile %in% 6:96) %>% 
#   mutate(bin = cut_interval(tcc, n = 20)) %>% # cut_width(tcc, width = 25.5, boundary = -255)
#   group_by(year, bin) %>% 
#   summarize(count = n()) %>% 
#   ggplot() +
#   geom_col(aes(x = count,
#                y = bin)) +
#   facet_wrap(~ year) +
#   labs(x = "Notifications", 
#        y = "Binned Interannual Change in TCC") +
#   theme_minimal() +
#   theme(axis.text.x = element_blank(),
#         axis.ticks.x = element_blank())
# 
# ggsave("output/vis_tcc_histogram_not_20251030.png",
#        dpi = 300,
#        width = 7.5,
#        height = 7.5)
