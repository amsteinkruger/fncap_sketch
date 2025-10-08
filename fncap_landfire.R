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

# Check frequencies.

dat_evt_2016 %>% 
  freq %>% 
  as.data.frame %>% 
  ggplot() +
  geom_col(aes(x = value %>% factor,
               y = count))

# So, 

# Reduce LANDFIRE to just EVT_GP, which is a numeric code for existing vegetation type group. 

activeCat(dat_evt_2016) <- "EVT_GP"

# Problem: "as.numeric" only keeps EVT_PHYS, which is unhelpful.
# Try swapping out categories.

# newcats = dat_evt_2016 %>% cats %>% magrittr::extract2(1) %>% select(EVT_GP) %>% list

# dat_evt_2016 = dat_evt_2016 %>% as.numeric

# Besides the other reasons this doesn't work, a simple test also doesn't work:
dat_evt_fake = dat_evt_2016
dat_evt_fake %>% plot
dat_evt_fake[dat_evt_fake == 615] <- NA
dat_evt_fake %>% plot

# dat_evt_2016[dat_evt_2016 != 614 & dat_evt_2016 != 615 & dat_evt_2016 != 625] <- 0 # Yields unknown categories in raster values.


# 2024

dat_evt_2024 = 
  "data/LF2024_EVT_250_CONUS/Tif/LC24_EVT_250.tif" %>% 
  rast %>% 
  crop(dat_ext) %>% 
  project("EPSG:2992")

# result is multiplication of simplified (0, 1) rasters for a single result, then extract notifications onto that
