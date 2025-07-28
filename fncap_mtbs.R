# Check out MTBS with reference to Wang and Lewis (2024).

#  Packages

library(tidyverse)
library(terra)
library(tidyterra)

#  MTBS

mtbs_points = "data/mtbs_fod_pts_data" %>% vect

#   30730 observations in lon/lat.

mtbs_perimeters = "data/mtbs_perimeter_data" %>% vect

#   Ditto.

# Check out a subset of perimeters in Oregon.

mtbs_perimeters %>% filter(substr(Event_ID, 1, 2) == "OR") %>% slice_sample(n = 100) %>% plot

# Intersect a point with perimeters.

# Intersect a polygon with perimeters.

# Demo Wang and Lewis' Eq (8): fire risk is the annualized count of distinct fires w/in a radius over 20 years.


