# Find spatial overlaps between notifications. 
#  Beware reused object names. 
#  Beware the sensitivity of exported results to arguments to arrange().

# Packages

library(tidyverse)
library(magrittr)
library(terra)
library(tidyterra)
library(readxl)

# Bounds

#  OR

dat_bounds_or = 
  "data/cb_2023_us_state_500k" %>% 
  vect %>% 
  filter(STUSPS == "OR") %>% 
  project("EPSG:2992")

#  Pyromes

dat_bounds_pyromes = 
  "data/USFS Pyromes/Data/Pyromes_CONUS_20200206.shp" %>% 
  vect %>% 
  rename(WHICH = NAME) %>% # Band-Aid for a reserved attribute name.
  filter(WHICH %in% c("Marine Northwest Coast Forest", "Klamath Mountains", "Middle Cascades")) %>% 
  select(Pyrome = WHICH) %>% 
  summarize(Pyrome = "All Pyromes") %>% 
  fillHoles %>% 
  project("EPSG:2992")

#  Intersection

dat_bounds = 
  intersect(dat_bounds_or, dat_bounds_pyromes) %>% 
  # Handle island polygons. These are not real islands.
  disagg %>% 
  cbind(., expanse(., unit = "ha")) %>% 
  filter(y == max(y)) %>% 
  select(-y)

# Notifications

dat_notifications = 
  "output/dat_notifications_polygons.gdb" %>% 
  vect %>% 
  filter(ActivityType == "Clearcut/Overstory Removal") %>% 
  filter(ActivityUnit == "MBF") %>% 
  filter(LandOwnerType == "Partnership/Corporate Forestland Ownership") %>% 
  rename(LandOwner = LandOwnerName_Right) %>% 
  select(-ends_with("Right")) %>% 
  rename_with(~ sub("_Left$", "", .x), everything()) %>% 
  mutate(Year_Start = year(DateStart),
         Year_Quarter_Start = quarter(DateStart, type = "year.quarter"),
         Quarter_Start = quarter(DateStart),
         Month_Start = month(DateStart),
         Year_End = ifelse(is.na(DateContinuationEnd), year(DateEnd), year(DateContinuationEnd)),
         Quarter_End = ifelse(is.na(DateContinuationEnd), quarter(DateEnd), quarter(DateContinuationEnd)),
         Year_Quarter_End = ifelse(is.na(DateContinuationEnd), quarter(DateEnd, type = "year.quarter"), quarter(DateContinuationEnd, type = "year.quarter")),
         Month_End = ifelse(is.na(DateContinuationEnd), month(DateEnd), month(DateContinuationEnd)),
         MBF = ActivityQuantity %>% as.numeric) %>%
  arrange(desc(Year_Start), desc(Quarter_Start), desc(Month_Start), LandOwnerType, LandOwner, desc(MBF), desc(Acres)) %>% 
  filter(Year_Start > 2014 & Year_End < 2025) %>% 
  select(Year_Start) %>% 
  project("EPSG:2992") %>% 
  # Fix invalid polygons.
  makeValid %>% 
  # Crop.
  crop(dat_bounds) %>% 
  # Assign unique ID.
  mutate(UID = row_number()) %>% 
  relocate(UID) %>% 
  # Reduce to unique ID for intersection.
  select(UID)

dat_notifications_intersect = 
  dat_notifications %>% 
  # filter(UID %in% 1:10) %>% 
  relate(., ., relation = "intersects") %>% 
  rowSums() %>% 
  `-` (1) %>% 
  tibble(UID = seq(1, length(.)), Intersects = .)
