# Check out LANDFIRE Existing Vegetation Type.
# 2025/09/15

# Packages

library(tidyverse)
library(terra)
library(tidyterra)
library(viridis)

# Data

#  Boundaries

dat_bounds_vector = 
  "data/cb_2023_us_state_500k" %>% 
  vect %>% 
  filter(STUSPS == "OR") %>% 
  project("EPSG:2992")

# data_bounds = 
#   "data/cb_2023_us_state_500k" %>% 
#   vect %>% 
#   filter(STUSPS == "OR") %>% 
#   project("EPSG:5070") %>% 
#   select(STATEFP) %>% 
#   rasterize(crop(data_reference, .))

# Pyromes

dat_pyrome = 
  "data/USFS Pyromes/Data/Pyromes_CONUS_20200206.shp" %>% 
  vect %>% 
  rename(WHICH = NAME) %>% # Band-Aid for a reserved attribute name.
  filter(WHICH %in% c("Marine Northwest Coast Forest", "Klamath Mountains", "Middle Cascades")) %>% 
  select(Pyrome = WHICH) %>% 
  summarize(Pyrome = "All Pyromes") %>% # This is not great.
  fillHoles %>% 
  project("EPSG:2992")

# Extent

dat_ext = 
  intersect(dat_bounds_vector, dat_pyrome) %>% 
  disagg %>% # Handle island polygons. These are not real islands.
  cbind(., expanse(., unit = "ha")) %>% 
  filter(y == max(y)) %>% 
  select(-y)

#  LANDFIRE
#   Note shenanigans with plot() recognizing attributes.

# data_evt = 
#   "data/LF2024_EVT_250_CONUS/LC24_EVT_250.tif" %>% 
#   rast %>% 
#   project("EPSG:4269")

# 2016

# Problem: either crop() or project() is breaking something dramatically.

# Problem persists w/ small slice of OR. Maybe ext nested in crop needs explicit projection? But dat_ext has one.
# Note that the small slice is grabbing the Gulf Coast (?!), so it's definitely just wrong.

dat_evt_2016 = 
  "data/LF2016_EVT_200_CONUS/Tif/LC16_EVT_200.tif" %>% 
  rast %>% 
  crop(ext(2e+05, 5e+05, 5e+05, 7e+05)) %>% 
  # crop(dat_ext) %>% 
  project("EPSG:2992")

levels(dat_evt_2016)

# Value, EVT_NAME

cats(dat_evt_2016)

# Whole dataframe.

#activeCat()
#catalyze()
  
# 2024

dat_evt_2024 = 
  "data/LF2024_EVT_250_CONUS/Tif/LC24_EVT_250.tif" %>% 
  rast %>% 
  crop(dat_ext) %>% 
  project("EPSG:2992")

# levels
# cats
# problem is application of logical operation to categories of a raster in terra

# test whether pulling project() out of the pipeline helps with runtime (?!?!)

# result is multiplication of simplified (0, 1) rasters for a single result, then extract notifications onto that
