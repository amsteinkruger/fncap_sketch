# Get probabilities of stand destruction from an FSIM product by way of Matt's napkin sketch.
# 2025/07/02

# Packages

library(tidyverse)
library(terra)
library(tidyterra)

# Data

#  Bounds

data_reference = "data/I_FSim_CONUS_LF2020_270m/CONUS_BP.tif" %>% rast # In epsg:5070.

data_bounds_vector = 
  "data/cb_2023_us_state_500k" %>% 
  vect %>% 
  filter(STUSPS == "OR")

data_bounds = 
  "data/cb_2023_us_state_500k" %>% 
  vect %>% 
  filter(STUSPS == "OR") %>% 
  project("EPSG:5070") %>% 
  select(STATEFP) %>% 
  rasterize(crop(data_reference, .))

#  FSIM

path_fsim = "data/I_FSim_CONUS_LF2020_270m"
epsg_fsim = "EPSG:4269"

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

#   Note choice to subset to latest observation for each stand.

data_stands = 
  "data/OR_PLOT.csv" %>% 
  read_csv %>% 
  select(CN, INVYR, LAT, LON) %>% 
  group_by(LAT, LON) %>% 
  filter(INVYR == max(INVYR)) %>% 
  ungroup %>% 
  vect(geom = c("LON", "LAT"))

crs(data_stands) <- epsg_fsim

#  Pyromes

data_pyromes = 
  "data/USFS Pyromes/Data/Pyromes_CONUS_20200206.shp" %>% 
  vect %>% 
  project(epsg_fsim) %>% 
  intersect(data_bounds_vector)

# Processing

#  Transform flame length probabilities into destruction probabilities, sum, and multiply by burn probabilities.

data_fsim_flp1_sd = data_fsim_flp1 * 0.00
data_fsim_flp2_sd = data_fsim_flp2 * 0.10
data_fsim_flp3_sd = data_fsim_flp3 * 0.30
data_fsim_flp4_sd = data_fsim_flp4 * 0.50
data_fsim_flp5_sd = data_fsim_flp5 * 0.70
data_fsim_flp6_sd = data_fsim_flp6 * 1.00

data_fsim_sd_sum = data_fsim_flp1_sd + data_fsim_flp2_sd + data_fsim_flp3_sd + data_fsim_flp4_sd + data_fsim_flp5_sd + data_fsim_flp6_sd

data_fsim_sd_out = data_fsim_sd_sum * data_fsim_bp

rm(data_fsim_flp1, 
   data_fsim_flp2, 
   data_fsim_flp3, 
   data_fsim_flp4, 
   data_fsim_flp5, 
   data_fsim_flp6, 
   data_fsim_flp1_sd,
   data_fsim_flp2_sd,
   data_fsim_flp3_sd,
   data_fsim_flp4_sd,
   data_fsim_flp5_sd,
   data_fsim_flp6_sd,
   data_fsim_sd_sum)

#  Assign stand destruction probabilities to plots. (FSIM to FIA.)

data_sd_stands = extract(data_fsim_sd_out, dat_stands, bind = TRUE)

#  Assign stand destruction probabilities to pyromes (from plots). (FSIM to FIA to . . . pyromes.)

#   First, extract backwards (because that doesn't require critical thinking.)

data_sd_extract = 
  extract(data_pyromes, data_sd_stands) %>% 
  select(PYROME) %>% 
  cbind(data_sd_stands, .) %>% 
  as.data.frame %>% 
  group_by(PYROME) %>% 
  summarize(PSD = mean(Band_1, na.rm = TRUE)) %>% 
  ungroup %>% 
  drop_na

data_sd_pyromes = 
  data_pyromes %>% 
  select(PYROME) %>% 
  left_join(data_sd_extract, by = "PYROME", keep = FALSE)

rm(data_sd_extract)

# Summarize by cell.

data_fsim_sd_out %>% as.data.frame %>% pull(Band_1) %>% log %>% abs %>% hist

# Summarize by stand.

data_sd_stands %>% as.data.frame %>% pull(Band_1) %>% log %>% abs %>% hist

# Summarize by pyrome.

data_sd_pyromes %>% as.data.frame %>% pull(PSD) %>% log %>% abs %>% hist

data_sd_pyromes %>% 
  ggplot() +
  geom_spatvector(aes(fill = PSD), color = NA)
