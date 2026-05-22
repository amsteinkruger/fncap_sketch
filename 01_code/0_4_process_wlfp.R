# Process WLFP.

#  Cutting this off (2026/05/22) for a few reasons:
#   Since the fire data used for WLFP runs from 2000-2017, it's a poor fit to the 2015-2024 panel.
#   Computing this for all years is challenging -- requires another level of nesting with more memory management.
#   Computing this without WFPI doesn't make much sense.

#  One year requires ~20 minutes without additional memory management.

#  Set up bounds. 

dat_bounds_wlfp = "03_intermediate/dat_bounds.gdb" %>% vect %>% project(crs(dat_look))

#  Data?

#   Get down to just one layer of compression. 

dat_wlfp = 
  "02_data/0_4_WLFP/Compressed" %>% 
  list.files %>% 
  tibble(file = .) %>% 
  mutate(path_compressed = paste0("02_data/0_4_WLFP/Compressed/", file),
         path_uncompressed = paste0("02_data/0_4_WLFP/Uncompressed/", file)) %T>% 
  mutate(data = 
           map2(.x = path_compressed, 
                .y = path_uncompressed,
                .f = ~ unzip(.x, exdir = .y)))

#   Get one year processed. This would need to be nested down one level to work for all years. 
#    Note that memory management between years would probably be helpful. 

time_start = Sys.time()

dat_look =
  "02_data/0_4_WLFP/Uncompressed/2010_WFPI-based_Large_Fire_Probability_Forecast_1_DATA.zip" %>%
  list.files %>%
  tibble(file = .) %>%
  filter(file %>% str_sub(-3, -1) == "zip") %>% 
  # Get the data.
  mutate(year = file %>% str_sub(-21, -18) %>% as.numeric,
         month = file %>% str_sub(-17, -16) %>% as.numeric,
         day = file %>% str_sub(-15, -14) %>% as.numeric,
         quarter = month %>% as.numeric %>% multiply_by(1 / 3) %>% ceiling,
         layer = paste0("WLFP_", year, "_", quarter),
         zip = paste0("/vsizip/02_data/0_4_WLFP/Uncompressed/2010_WFPI-based_Large_Fire_Probability_Forecast_1_DATA.zip/", file), 
         tiff = paste0("wlfp_", zip %>% str_sub(-26, -5), ".tiff"),
         path = paste0(zip, "/", tiff),
         data = map(path, rast)) %>% 
  # Drop sentinel values.
  mutate(data = 
           data %>% 
           map(~ rename(.x, WLFP = 1)) %>% 
           map(~ filter(.x, WLFP < 100))) %>% 
  # Stack rasters by quarter.
  group_by(layer) %>% 
  summarize(data = list({data %>% rast})) %>% 
  ungroup %>% 
  # Take means by quarter, then rename results. 
  mutate(data = 
           data %>% 
           map(~ mean(.x, na.rm = TRUE)) %>% 
           map2(.x = ., 
                .y = layer, 
                ~ {
                  names(.x) <- as.character(.y)
                  .x
                })) %>% 
  # Switch from a tibble to a raster stack. 
  magrittr::extract2("data") %>% # Equivalent to .$data.
  reduce(c) %>% 
  # Crop.
  crop(project(vect("03_intermediate/dat_bounds.gdb"), crs(.)), 
       mask = TRUE) %>% 
  # Reproject.
  project("EPSG:2992")

time_end = Sys.time()

time_end - time_start