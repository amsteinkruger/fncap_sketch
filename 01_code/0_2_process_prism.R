# Process monthly climate data from PRISM.

#  Note that compression is handled with /vsizip/ via GDAL. 

#  Set up bounds. 

dat_bounds_prism = "03_intermediate/dat_bounds.gdb" %>% vect %>% project("EPSG:4269")

#  Vapor Pressure Deficit, Maximum (VPD)

dat_vpd = 
  list.files("02_data/0_2_PRISM/VPD") %>% 
  tibble(file = .) %>% 
  mutate(year = file %>% str_sub(-10, -7) %>% as.numeric,
         month = file %>% str_sub(-6, -5) %>% as.numeric,
         layer = paste0("VPD_", year, "_", month),
         path = 
           paste0(
             "/vsizip/02_data/0_2_PRISM/VPD/",
             file,
             "/",
             file %>% str_replace_all(".zip", ".tif")
             )) %>% 
  arrange(year, month) %>% 
  filter(year %in% 2005:2025) %>% 
  mutate(data = 
           path %>% 
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
  list.files("02_data/0_2_PRISM/PPT") %>% 
  tibble(file = .) %>% 
  mutate(year = file %>% str_sub(-10, -7) %>% as.numeric,
         month = file %>% str_sub(-6, -5) %>% as.numeric,
         layer = paste0("PPT_", year, "_", month),
         path = 
           paste0(
             "/vsizip/02_data/0_2_PRISM/PPT/",
             file,
             "/",
             file %>% str_replace_all(".zip", ".tif")
           )) %>% 
  arrange(year, month) %>% 
  filter(year %in% 2005:2025) %>% 
  mutate(data = 
           path %>% 
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
  list.files("02_data/0_2_PRISM/TMax") %>% 
  tibble(file = .) %>% 
  mutate(year = file %>% str_sub(-10, -7) %>% as.numeric,
         month = file %>% str_sub(-6, -5) %>% as.numeric,
         layer = paste0("TMax_", year, "_", month),
         path = 
           paste0(
             "/vsizip/02_data/0_2_PRISM/TMax/",
             file,
             "/",
             file %>% str_replace_all(".zip", ".tif")
           )) %>% 
  arrange(year, month) %>% 
  filter(year %in% 2005:2025) %>% 
  mutate(data = 
           path %>% 
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
  list.files("02_data/0_2_PRISM/TMean") %>% 
  tibble(file = .) %>% 
  mutate(year = file %>% str_sub(-10, -7) %>% as.numeric,
         month = file %>% str_sub(-6, -5) %>% as.numeric,
         layer = paste0("TMean_", year, "_", month),
         path = 
           paste0(
             "/vsizip/02_data/0_2_PRISM/TMean/",
             file,
             "/",
             file %>% str_replace_all(".zip", ".tif")
           )) %>% 
  arrange(year, month) %>% 
  filter(year %in% 2005:2025) %>% 
  mutate(data = 
           path %>% 
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
