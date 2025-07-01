# Get probabilities of stand destruction from an FSIM product by way of Matt's napkin sketch.
# 2025/05/12

# Packages

library(tidyverse)
library(terra)
library(tidyterra)

# Data

#  Bounds

data_reference = "data/I_FSim_CONUS_LF2020_270m/CONUS_BP.tif" %>% rast # In epsg:5070.

data_bounds = 
  "data/cb_2023_us_state_500k" %>% 
  vect %>% 
  filter(STUSPS == "OR") %>% 
  project("epsg:5070") %>% 
  select(STATEFP) %>% 
  rasterize(crop(data_reference, .))

#  FSIM

path_fsim = "data/I_FSim_CONUS_LF2020_270m"
epsg_fsim = "epsg:4269"

fun_fsim = 
  function(path, extension, bounds, epsg){
    
    path %>% 
      paste0(., extension) %>% 
      rast %>% 
      crop(bounds) %>% 
      mask(bounds) %>% 
      trim %>% 
      project(epsg) %>% 
      trim
    
  }

data_fsim_bp = fun_fsim(path_fsim, "/CONUS_BP.tif", data_bounds, epsg_fsim)
data_fsim_flp1 = fun_fsim(path_fsim, "/CONUS_FLP1.tif", data_bounds, epsg_fsim)
data_fsim_flp2 = fun_fsim(path_fsim, "/CONUS_FLP2.tif", data_bounds, epsg_fsim)
data_fsim_flp3 = fun_fsim(path_fsim, "/CONUS_FLP3.tif", data_bounds, epsg_fsim)
data_fsim_flp4 = fun_fsim(path_fsim, "/CONUS_FLP4.tif", data_bounds, epsg_fsim)
data_fsim_flp5 = fun_fsim(path_fsim, "/CONUS_FLP5.tif", data_bounds, epsg_fsim)
data_fsim_flp6 = fun_fsim(path_fsim, "/CONUS_FLP6.tif", data_bounds, epsg_fsim)

#  FNCAP

# interact raster w/ polygons (pyromes, maybe FIA plot locations/potential locations) (using OR placeholder)
# handle raster arithmetic
# visualize
  
# get FNCAP in, consider handling stand location fuzz