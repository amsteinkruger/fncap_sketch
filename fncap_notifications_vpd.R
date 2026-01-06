# VPD

dat_bounds_vpd = dat_bounds %>% project("EPSG:4269")

dat_vpd = 
  list.files("data/VPD/Compressed") %>% 
  tibble(file = .) %>% 
  mutate(path_compressed = paste0("data/VPD/Compressed/", file),
         path_uncompressed = paste0("data/VPD/Uncompressed/", file) %>% str_sub(1, -5),
         year = file %>% str_sub(-10, -7) %>% as.numeric,
         month = file %>% str_sub(-6, -5) %>% as.numeric,
         layer = paste0("VPD_", year, "_", month)) %>% 
  arrange(year, month) %>% 
  filter(month %in% 4:7) %T>% 
  mutate(data = 
           map2(.x = path_compressed, 
                .y = path_uncompressed,
                .f = ~ unzip(.x, exdir = .y))) %>% 
  mutate(data = 
           path_uncompressed %>% 
           paste0(., "/", str_replace(file, ".zip", ".tif")) %>% 
           map(rast) %>% 
           map(crop, dat_bounds_vpd, mask = TRUE) %>% 
           map2(.x = ., 
                .y = layer, 
                ~ {
                  names(.x) <- as.character(.y)
                  .x
                })) %>% 
  select(year, month, data) %>% 
  magrittr::extract2("data") %>% # Equivalent to .$data.
  reduce(c) %>% 
  project("EPSG:2992") %T>% 
  writeRaster("output/data_vpd.tif", filetype = "GTiff", overwrite = TRUE)
