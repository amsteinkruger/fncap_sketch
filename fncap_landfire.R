# Check out LANDFIRE Existing Vegetation Type.
# 2025/10/07

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

dat_keep_detail = 
  intersect(dat_bounds_vector, dat_pyrome) %>% 
  disagg %>% # Handle island polygons. These are not real islands.
  cbind(., expanse(., unit = "ha")) %>% 
  filter(y == max(y)) %>% 
  select(-y) %>% 
  project("EPSG:5070") # From LANDFIRE.

# LANDFIRE

#  2016

dat_evt_2016 = 
  "data/LF2016_EVT_200_CONUS/Tif/LC16_EVT_200.tif" %>% 
  rast %>% 
  crop(dat_keep_detail, mask = TRUE) %>% 
  project("EPSG:2992")

#   Check frequencies.

dat_evt_2016 %>% 
  freq %>% 
  as.data.frame %>% 
  ggplot() +
  geom_col(aes(x = value %>% factor,
               y = count)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#   Get categories. The vector of numeric values is handpicked from metadata (equivalent to categories).

dat_evt_2016_cats = 
  dat_evt_2016 %>% 
  cats %>% 
  magrittr::extract2(1) %>% 
  as_tibble

vec_evt_2016_cats = 
  dat_evt_2016_cats %>% 
  select(Value, EVT_GP) %>% 
  filter(EVT_GP %in% c(614, 615, 625)) %>% 
  magrittr::extract2("Value")

# or

vec_evt_2016_cats_more = 
  dat_evt_2016_cats %>% 
  select(Value, EVT_ORDER) %>% 
  filter(EVT_ORDER == "Tree-dominated") %>% 
  magrittr::extract2("Value")

#   Get a binary raster.

dat_evt_2016_binary <- dat_evt_2016 %in% vec_evt_2016_cats

dat_evt_2016_binary %>% plot

# or

dat_evt_2016_binary_more <- dat_evt_2016 %in% vec_evt_2016_cats_more

dat_evt_2016_binary_more %>% plot

#  2024

dat_evt_2024 = 
  "data/LF2024_EVT_250_CONUS/Tif/LC24_EVT_250.tif" %>% 
  rast %>% 
  crop(dat_keep_detail, mask = TRUE) %>% 
  project("EPSG:2992")

#   Check frequencies.

dat_evt_2024 %>% 
  freq %>% 
  as.data.frame %>% 
  ggplot() +
  geom_col(aes(x = value %>% factor,
               y = count)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#   Get categories. The vector of numeric values is handpicked from metadata (equivalent to categories).

dat_evt_2024_cats = 
  dat_evt_2024 %>% 
  cats %>% 
  magrittr::extract2(1) %>% 
  as_tibble

vec_evt_2024_cats = 
  dat_evt_2024_cats %>% 
  select(Value, EVT_GP) %>% 
  filter(EVT_GP %in% c(614, 615, 625)) %>% 
  magrittr::extract2("Value")

# or

vec_evt_2024_cats_more = 
  dat_evt_2024_cats %>% 
  select(Value, EVT_ORDER) %>% 
  filter(EVT_ORDER == "Tree-dominated") %>% 
  magrittr::extract2("Value")

#   Get a binary raster.

dat_evt_2024_binary <- dat_evt_2024 %in% vec_evt_2024_cats

dat_evt_2024_binary %>% plot

# or

dat_evt_2024_binary_more <- dat_evt_2024 %in% vec_evt_2024_cats_more

dat_evt_2024_binary_more %>% plot

#  Get the product of the two binary rasters.

dat_evt_binary = dat_evt_2016_binary * dat_evt_2024_binary

# or

dat_evt_binary_more = dat_evt_2016_binary_more * dat_evt_2024_binary_more

# Export

writeRaster(dat_evt_binary, "output/dat_evt_binary.tif", overwrite = TRUE)

# or

writeRaster(dat_evt_binary_more, "output/dat_evt_binary_more.tif", overwrite = TRUE)
