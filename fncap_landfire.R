# Check out LANDFIRE Existing Vegetation Type.
# 2025/09/11

# Packages

library(tidyverse)
library(terra)
library(tidyterra)
library(viridis)

# Data

#  Boundaries

data_bounds_vector = 
  "data/cb_2023_us_state_500k" %>% 
  vect %>% 
  filter(STUSPS == "OR")

# data_bounds = 
#   "data/cb_2023_us_state_500k" %>% 
#   vect %>% 
#   filter(STUSPS == "OR") %>% 
#   project("EPSG:5070") %>% 
#   select(STATEFP) %>% 
#   rasterize(crop(data_reference, .))

#  LANDFIRE
#   Note shenanigans with plot() recognizing attributes.

data_evt = 
  "data/LF2024_EVT_250_CONUS/LC24_EVT_250.tif" %>% 
  rast %>% 
  project("EPSG:4269")
