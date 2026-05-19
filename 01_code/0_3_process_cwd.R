# Process CWD.

#  Data

#   Set up bounds. 

dat_bounds_cwd = "03_intermediate/dat_bounds.gdb" %>% vect %>% project("OGC:CRS84")

#   Get CWD.

dat_cwd = 
  "02_data/0_3_CWD/CWD_1980_2025.nc" %>% 
  rast %>%
  crop(dat_bounds_cwd, mask = TRUE) %T>%
  { names(.) <- (names(.) %>% str_remove_all("CWD_year=")) } %>%
  select(2010:2025 %>% as.character) %>%
  project("EPSG:2992") %T>% 
  writeRaster("03_intermediate/data_cwd.tif", filetype = "GTiff", overwrite = TRUE)
