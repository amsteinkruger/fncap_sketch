# Process monthly climate data from PRISM.

#  Set up bounds. 

dat_bounds_prism = "03_intermediate/dat_bounds.gdb" %>% vect %>% project("EPSG:4269")

#  Vapor Pressure Deficit, Maximum (VPD)

dat_vpd = 
  list.files("02_data/0_2_PRISM/VPD_Compressed") %>% 
  tibble(file = .) %>% 
  mutate(path_compressed = paste0("02_data/0_2_PRISM/VPD_Compressed/", file),
         path_uncompressed = paste0("02_data/0_2_PRISM/VPD_Uncompressed/", file) %>% str_sub(1, -5),
         year = file %>% str_sub(-10, -7) %>% as.numeric,
         month = file %>% str_sub(-6, -5) %>% as.numeric,
         layer = paste0("VPD_", year, "_", month)) %>% 
  arrange(year, month) %>% 
  filter(year %in% 2010:2025) %T>% 
  # filter(month %in% 4:7) %T>% 
  mutate(data = 
           map2(.x = path_compressed, 
                .y = path_uncompressed,
                .f = ~ unzip(.x, exdir = .y))) %>% 
  mutate(data = 
           path_uncompressed %>% 
           paste0(., "/", str_replace(file, ".zip", ".tif")) %>% 
           map(rast) %>% 
           map(crop, dat_bounds_prism, mask = TRUE) %>% 
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
  writeRaster("03_intermediate/data_vpd.tif", filetype = "GTiff", overwrite = TRUE)

# Precipitation (PPT)

dat_ppt = 
  list.files("02_data/0_2_PRISM/PPT_Compressed") %>% 
  tibble(file = .) %>% 
  mutate(path_compressed = paste0("02_data/0_2_PRISM/PPT_Compressed/", file),
         path_uncompressed = paste0("02_data/0_2_PRISM/PPT_Uncompressed/", file) %>% str_sub(1, -5),
         year = file %>% str_sub(-10, -7) %>% as.numeric,
         month = file %>% str_sub(-6, -5) %>% as.numeric,
         layer = paste0("PPT_", year, "_", month)) %>% 
  arrange(year, month) %>% 
  filter(year %in% 2010:2025) %T>% 
  # filter(month %in% 4:7) %T>% 
  mutate(data = 
           map2(.x = path_compressed, 
                .y = path_uncompressed,
                .f = ~ unzip(.x, exdir = .y))) %>% 
  mutate(data = 
           path_uncompressed %>% 
           paste0(., "/", str_replace(file, ".zip", ".tif")) %>% 
           map(rast) %>% 
           map(crop, dat_bounds_prism, mask = TRUE) %>% 
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
  writeRaster("03_intermediate/data_ppt.tif", filetype = "GTiff", overwrite = TRUE)

# Temperature, Maximum (TMax)

dat_tmax = 
  list.files("02_data/0_1_2_PRISM/TMax_Compressed") %>% 
  tibble(file = .) %>% 
  mutate(path_compressed = paste0("02_data/0_2_PRISM/TMax_Compressed/", file),
         path_uncompressed = paste0("02_data/0_2_PRISM/TMax_Uncompressed/", file) %>% str_sub(1, -5),
         year = file %>% str_sub(-10, -7) %>% as.numeric,
         month = file %>% str_sub(-6, -5) %>% as.numeric,
         layer = paste0("TMax_", year, "_", month)) %>% 
  arrange(year, month) %>% 
  filter(year %in% 2010:2025) %T>% 
  # filter(month %in% 4:7) %T>% 
  mutate(data = 
           map2(.x = path_compressed, 
                .y = path_uncompressed,
                .f = ~ unzip(.x, exdir = .y))) %>% 
  mutate(data = 
           path_uncompressed %>% 
           paste0(., "/", str_replace(file, ".zip", ".tif")) %>% 
           map(rast) %>% 
           map(crop, dat_bounds_prism, mask = TRUE) %>% 
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
  writeRaster("03_intermediate/data_tmax.tif", filetype = "GTiff", overwrite = TRUE)

# Temperature, Mean (TMean)

dat_tmean = 
  list.files("02_data/0_1_2_PRISM/TMean_Compressed") %>% 
  tibble(file = .) %>% 
  mutate(path_compressed = paste0("02_data/0_2_PRISM/TMean_Compressed/", file),
         path_uncompressed = paste0("02_data/0_2_PRISM/TMean_Uncompressed/", file) %>% str_sub(1, -5),
         year = file %>% str_sub(-10, -7) %>% as.numeric,
         month = file %>% str_sub(-6, -5) %>% as.numeric,
         layer = paste0("TMean_", year, "_", month)) %>% 
  arrange(year, month) %>% 
  filter(year %in% 2010:2025) %T>% 
  # filter(month %in% 4:7) %T>% 
  mutate(data = 
           map2(.x = path_compressed, 
                .y = path_uncompressed,
                .f = ~ unzip(.x, exdir = .y))) %>% 
  mutate(data = 
           path_uncompressed %>% 
           paste0(., "/", str_replace(file, ".zip", ".tif")) %>% 
           map(rast) %>% 
           map(crop, dat_bounds_prism, mask = TRUE) %>% 
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
  writeRaster("03_intermediate/data_tmean.tif", filetype = "GTiff", overwrite = TRUE)
