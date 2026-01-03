# NDVI

# Get data.

dat_ndvi = 
  list.files("data/NDVI") %>% 
  tibble(file = .) %>% 
  mutate(path = paste0("data/NDVI/", file),
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
  select(year, quarter, data)

# Export data. 

#  14 minutes and 12 GB in memory for 2015-2016 (2026/01/02).
#  93 minutes and 63 GB in memory for 2014-2024 (2026/01/02).

dat_ndvi_stack = 
  dat_ndvi$data %>% 
  reduce(c) %>% 
  project("EPSG:2992") %T>% 
  writeRaster("output/data_ndvi.tif", filetype = "GTiff", overwrite = TRUE)

# Check export. Delete this after checking once. 

dat_check = "output/data_ndvi.tif" %>% rast
