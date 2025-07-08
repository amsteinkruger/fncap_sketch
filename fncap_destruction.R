# Get probabilities of stand destruction from an FSIM product by way of Matt's napkin sketch.
# 2025/07/08

# Packages

library(tidyverse)
library(terra)
library(tidyterra)
library(viridis)

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

path_fsim = "data/I_FSim_CONUS_LF2020_270m/"
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

data_fsim_bp = fun_fsim(path_fsim, "CONUS_BP.tif", data_bounds, epsg_fsim)
data_fsim_flp1 = fun_fsim(path_fsim, "CONUS_FLP1.tif", data_bounds, epsg_fsim)
data_fsim_flp2 = fun_fsim(path_fsim, "CONUS_FLP2.tif", data_bounds, epsg_fsim)
data_fsim_flp3 = fun_fsim(path_fsim, "CONUS_FLP3.tif", data_bounds, epsg_fsim)
data_fsim_flp4 = fun_fsim(path_fsim, "CONUS_FLP4.tif", data_bounds, epsg_fsim)
data_fsim_flp5 = fun_fsim(path_fsim, "CONUS_FLP5.tif", data_bounds, epsg_fsim)
data_fsim_flp6 = fun_fsim(path_fsim, "CONUS_FLP6.tif", data_bounds, epsg_fsim)

#  Pyromes

data_pyromes = 
  "data/USFS Pyromes/Data/Pyromes_CONUS_20200206.shp" %>% 
  vect %>% 
  rename(WHICH = NAME) %>% # Band-Aid for a reserved attribute name.
  project(epsg_fsim) %>% 
  intersect(data_bounds_vector) %>% 
  filter(WHICH %in% c("Marine Northwest Coast Forest", "Klamath Mountains", "Middle Cascades"))

#  FNCAP

data_stands_keep = 
  "output/dat_or_intermediate.csv" %>% 
  read_csv %>% 
  mutate(PLOT_UID = PLOT_UID %>% str_remove("\\_[^_]*$"), .keep = "none") # Drop conditions.

data_stands = 
  "data/OR_PLOT.csv" %>% 
  read_csv %>% 
  mutate(PLOT_UID = paste0(STATECD, "_", UNITCD, "_", COUNTYCD, "_", PLOT)) %>% 
  select(PLOT_UID, INVYR, LAT, LON) %>% 
  inner_join(data_stands_keep, 
             by = "PLOT_UID",
             relationship = "many-to-many") %>% 
  group_by(PLOT_UID) %>% 
  filter(INVYR == max(INVYR)) %>% # Subset to latest observation of each plot.
  ungroup %>% 
  vect(geom = c("LON", "LAT")) %>% 
  intersect(data_pyromes) %>% 
  select(PLOT_UID)

crs(data_stands) <- epsg_fsim

data_stands %>% plot

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

#  Assign stand destruction probabilities to plots. (FSIM to FIA.)

data_sd_stands = extract(data_fsim_sd_out, data_stands, bind = TRUE)

#   Export for the growth model (?).

data_sd_stands %>% as.data.frame %>% select(PLOT_UID, PSD = Band_1) %>% write_csv("output/data_sd_plots.csv")

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

# Summarize by cell.

ggplot() +
  geom_spatraster(data = data_fsim_sd_out,
                  maxcell = Inf) +
  scale_fill_viridis(na.value = NA,
                     limits = c(0, 0.1),
                     breaks = c(0, 0.05, 0.10)) +
  theme_void() +
  theme(legend.title = element_blank(),
        legend.ticks = element_blank(),
        legend.key.height = unit(3, "lines"))

# Summarize by stand.

ggplot() +
  geom_spatvector(data = data_bounds_vector) +
  geom_spatvector(data = data_pyromes) +
  geom_spatvector(data = data_sd_stands,
                  aes(color = Band_1)) +
  theme_void() +
  theme(legend.title = element_blank(),
        legend.ticks = element_blank(),
        legend.key.height = unit(3, "lines"))

# Summarize by pyrome.

ggplot() +
  geom_spatvector(data = data_bounds_vector) +
  geom_spatvector(data = data_sd_pyromes,
                  aes(fill = PSD), 
                  color = NA) +
  theme_void() +
  theme(legend.title = element_blank(),
        legend.ticks = element_blank(),
        legend.key.height = unit(3, "lines"))

# Bonus (1): find zeros. 

data_fsim_bp %>% 
  filter(Band_1 == 0) %>% 
  `+` (1) %>% 
  global("sum", na.rm = TRUE) %>% 
  `/` (dim(data_fsim_bp)[1] * dim(data_fsim_bp)[2])

#  2.1% of burn probabilities are zeros.

data_fsim_flp1 %>% 
  filter(Band_1 == 1) %>% 
  global("sum", na.rm = TRUE) %>% 
  `/` (dim(data_fsim_flp1)[1] * dim(data_fsim_flp1)[2])

#  0.7% of cells have FLP1 = 1 (and those end up with zeros for SDP).

# Bonus (2): find which zeros are extracted into plots of interest.

data_fsim_bp %>% filter(Band_1 == 0) %>% extract(data_stands) %>% filter(!is.na(Band_1)) %>% nrow

#  0 plots of interest have BP = 0.

data_fsim_flp1 %>% filter(Band_1 == 1) %>% extract(data_stands) %>% filter(!is.na(Band_1)) %>% nrow

#  22 plots of interest have FLP1 = 1

#  Let's suppose the remaining two NAs follow from meaningful NAs in the FLP layers.
