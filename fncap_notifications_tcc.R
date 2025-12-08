# TCC
#  Eats up to 76 GB in memory as of 2025/12/08.

crs_tcc = 
  "data/TCC/TCC_Science_2014/science_tcc_conus_wgs84_v2023-5_20140101_20141231.tif" %>% # Replace w/ 2024.
  rast %>% 
  crs

dat_bounds_tcc = dat_bounds %>% project(crs_tcc) # Note busted dependency on notifications_more.

dat_tcc = 
  list.files("data/TCC") %>% 
  tibble(file = .) %>% 
  filter(file %>% str_sub(1, 7) == "TCC_Sci") %>% 
  mutate(year = file %>% str_sub(-4, -1) %>% as.numeric) %>% 
  arrange(year) %>% 
  # filter(year %in% 2015:2016) %>% # Band-Aid for testing.
  rename(folder = file) %>% # Fiddling around.
  mutate(file = paste0("data/TCC/", folder, "/science_tcc_conus_wgs84_v2023-5_", year, "0101_", year, "1231.tif")) %>% # Fragile!
  mutate(data_0 = 
           file %>% 
           map(rast) %>% 
           map(as.numeric) %>% 
           map(crop, dat_bounds_tcc, mask = TRUE) %>% 
           map(project, "EPSG:2992")) %>% 
  select(year, starts_with("data"))

# A monument to shame.

c(dat_tcc$data_0[[1]] %>% rename(TCC_2014 = category), 
  dat_tcc$data_0[[2]] %>% rename(TCC_2015 = category), 
  dat_tcc$data_0[[3]] %>% rename(TCC_2016 = category), 
  dat_tcc$data_0[[4]] %>% rename(TCC_2017 = category), 
  dat_tcc$data_0[[5]] %>% rename(TCC_2018 = category), 
  dat_tcc$data_0[[6]] %>% rename(TCC_2019 = category), 
  dat_tcc$data_0[[7]] %>% rename(TCC_2020 = category), 
  dat_tcc$data_0[[8]] %>% rename(TCC_2021 = category), 
  dat_tcc$data_0[[9]] %>% rename(TCC_2022 = category), 
  dat_tcc$data_0[[10]] %>% rename(TCC_2023 = category)) %>% 
  writeRaster("output/test.tif", filetype = "GTiff", overwrite = TRUE)

dat_check = "output/test.tif" %>% rast

# End current code.

dat_join_tcc = 
  dat_notifications %>% 
  pull(Year) %>% 
  unique %>% 
  sort %>% 
  tibble(year = .) %>% 
  mutate(notifications = 
           year %>% 
           map(~ filter(dat_notifications, Year == .x))) %>% 
  inner_join(dat_tcc) %>% 
  mutate(notifications_0 = 
           map2(data_0,
                notifications,
                extract,
                fun = mean,
                ID = FALSE,
                bind = TRUE) %>% 
           map(rename,
               TCC_0 = category) %>% 
           map(as_tibble) %>% 
           map(select, UID, TCC_0),
         notifications_1 = 
           map2(data_1,
                notifications,
                extract,
                fun = mean,
                ID = FALSE,
                bind = TRUE) %>% 
           map(rename,
               TCC_1 = category) %>% 
           map(as_tibble) %>% 
           map(select, UID, TCC_1),
         notifications_d = 
           map2(data_difference,
                notifications,
                extract,
                fun = mean,
                ID = FALSE,
                bind = TRUE) %>% 
           map(rename,
               TCC_D = category) %>% 
           map(as_tibble) %>% 
           map(select, UID, TCC_D)) %>% 
  select(year, starts_with("notifications_")) %>% 
  mutate(notifications = map2(notifications_0, notifications_1, full_join),
         notifications = map2(notifications, notifications_d, full_join)) %>% 
  select(year, notifications) %>% 
  unnest(notifications)

# Ad hoc check

# Annual histograms of TCC change for each year for the full study area (inclusive of notifications)

dat_tcc %>% 
  # filter(year %in% 2015:2017) %>% 
  mutate(data_difference_out = 
           data_difference %>% 
           map(as.vector) %>% 
           map(as_tibble) %>% 
           map(drop_na)) %>% 
  select(year, data_difference_out) %>% 
  unnest(data_difference_out) %>% 
  rename(tcc = value) %>% 
  group_by(year) %>% 
  mutate(tcc_decile = tcc %>% ntile(100)) %>% 
  ungroup %>% 
  filter(tcc_decile %in% 6:96) %>% 
  mutate(bin = cut_interval(tcc, n = 20)) %>% # cut_width(tcc, width = 25.5, boundary = -255)
  group_by(year, bin) %>% 
  summarize(count = n()) %>% 
  ggplot() +
  geom_col(aes(x = count,
               y = bin)) +
  facet_wrap(~ year) +
  labs(x = "Pixels", 
       y = "Binned Interannual Change in TCC") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

ggsave("output/vis_tcc_histogram_all_20251030.png",
       dpi = 300,
       width = 7.5,
       height = 7.5)

# Annual histograms of TCC change for each year for each notification

dat_join_tcc %>% 
  rename(tcc = TCC_D) %>% 
  group_by(year) %>% 
  mutate(tcc_decile = tcc %>% ntile(100)) %>% 
  ungroup %>% 
  filter(tcc_decile %in% 6:96) %>% 
  mutate(bin = cut_interval(tcc, n = 20)) %>% # cut_width(tcc, width = 25.5, boundary = -255)
  group_by(year, bin) %>% 
  summarize(count = n()) %>% 
  ggplot() +
  geom_col(aes(x = count,
               y = bin)) +
  facet_wrap(~ year) +
  labs(x = "Notifications", 
       y = "Binned Interannual Change in TCC") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

ggsave("output/vis_tcc_histogram_not_20251030.png",
       dpi = 300,
       width = 7.5,
       height = 7.5)
