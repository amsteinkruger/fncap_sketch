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
  
# dat_look = 
#   "02_data/0_4_WLFP/Uncompressed/2010_WFPI-based_Large_Fire_Probability_Forecast_1_DATA.zip" %>% 
#   list.files %>% 
#   tibble(file = .) %>% 
#   filter(file %>% str_sub(-3, -1) == "zip") %>% 
#   slice_head(n = 10) %>% 
#   mutate(data_zip = paste0("/vsizip/02_data/0_4_WLFP/Uncompressed/2010_WFPI-based_Large_Fire_Probability_Forecast_1_DATA.zip/", file), #,
#          data_tiff = paste0("wlfp_", data_zip %>% str_sub(-26, -1), ".tiff"), 
#          data_rast = 
#            map2(~ unzip(.x, files = )) %>% 
#            map())

# check whether vsizip enables skipping intermediate steps for all raster handling (PRISM)
# then generalize this to get monthly results to match PRISM
# then add to 1_7 and handle PRISM at the same time

rast("/vsizip/02_data/0_4_WLFP/Uncompressed/2010_WFPI-based_Large_Fire_Probability_Forecast_1_DATA.zip/wlfp-forecast-1_data_20100101_20100101.zip/wlfp_data_20100101_20100101.tiff") %>% plot