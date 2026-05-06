# NDVI
#  93 minutes and 63 GB in memory for 2014-2024 (2026/01/02).
#  103 minutes and 74 GB in memory for 2014-2025 (2026/05/06). 

dat_ndvi = 
  list.files("02_data/0_1_1_NDVI") %>% 
  tibble(file = .) %>% 
  mutate(path = paste0("02_data/0_1_1_NDVI/", file),
         quarter_relative = file %>% str_remove("landsat_quarter_") %>% str_remove(".tif") %>% as.numeric,
         year_relative = quarter_relative %>% `/` (4) %>% floor,
         quarter = quarter_relative - year_relative * 4 + 1, 
         year = year_relative + 2014,
         layer = paste0("NDVI_", year, "_", quarter)) %>% 
  arrange(year, quarter) %>% 
  mutate(data = path %>% map(rast),
         data = 
           data %>% 
           map2(.x = ., 
                .y = layer, 
                ~ {
                  names(.x) <- as.character(.y)
                  .x
                })) %>% 
  select(year, quarter, data) %>% 
  magrittr::extract2("data") %>% # Equivalent to .$data.
  reduce(c) %>% 
  project("EPSG:2992") %T>% 
  writeRaster("03_intermediate/dat_ndvi.tif", filetype = "GTiff", overwrite = TRUE)
