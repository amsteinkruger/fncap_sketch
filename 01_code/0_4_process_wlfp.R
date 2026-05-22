# Process WLFP.

#  Set up bounds. 

dat_bounds_wlfp = "03_intermediate/dat_bounds.gdb" %>% vect %>% project("EPSG:????")

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
  
dat_look =
  "02_data/0_4_WLFP/Uncompressed/2010_WFPI-based_Large_Fire_Probability_Forecast_1_DATA.zip" %>%
  list.files %>%
  tibble(file = .) %>%
  filter(file %>% str_sub(-3, -1) == "zip") %>%
  slice_head(n = 10) %>%
  # Get the data.
  mutate(year = file %>% str_sub(-21, -18) %>% as.numeric,
         month = file %>% str_sub(-17, -16) %>% as.numeric,
         day = file %>% str_sub(-15, -14) %>% as.numeric,
         quarter = month %>% as.numeric %>% multiply_by(1 / 3) %>% ceiling,
         zip = paste0("/vsizip/02_data/0_4_WLFP/Uncompressed/2010_WFPI-based_Large_Fire_Probability_Forecast_1_DATA.zip/", file), 
         tiff = paste0("wlfp_", zip %>% str_sub(-26, -5), ".tiff"),
         path = paste0(zip, "/", tiff),
         data = map(path, rast)) %>% 
  # Drop sentinel values.
  mutate(data = 
           data %>% 
           map(~ rename(.x, WLFP = 1)) %>% 
           map(~ filter(.x, WLFP < 100))) %>% 
  # Stack rasters by month.
  group_by(year, quarter) %>% 
  summarize(
    data = 
      list({data %>% rast})
  ) %>% 
  ungroup %>% 
  # Take means by quarter. 
  mutate(data = data %>% map(~ mean(.x, na.rm = TRUE))) %>% 
  # Wait maybe stack first? Probably stack first
  # Crop to region of interest.
  # Reproject.
  # Stack for export. 
