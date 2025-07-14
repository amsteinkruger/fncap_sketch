# Check out harvest notifications from different sources.

library(tidyverse)
library(terra)
library(tidyterra)
library(readxl)

dat_points = "data/Points_Notifications.gdb" %>% vect
dat_lines = "data/Lines_Notifications.gdb" %>% vect
dat_polygons = "data/Polygons_Notifications.gdb" %>% vect
dat_polygons %>% as.data.frame %>% group_by(ActType) %>% summarize(count = n()) %>% ungroup %>% arrange(desc(count))

# So, clearcuts appear in the polygon data and the NoapIdentifier field corresponds to related data of interest.

# Get the flat data for comparison merging.

dat_flat = read_excel("data/Flat_Notifications.xlsx")

# Compile NoapIdentifier values from spatial data to merge.

dat_points_flat = dat_points %>% as.data.frame %>% as_tibble
dat_lines_flat = dat_lines %>% as.data.frame %>% as_tibble
dat_polygons_flat = dat_polygons %>% as.data.frame %>% as_tibble

dat_spatial_flat = bind_rows(dat_points_flat, dat_lines_flat, dat_polygons_flat)

# First, check merges on NoapIdentifier. Note that NoapIdentifiers are nonunique, and/but a lot of rows are nonunique on all fields (???).

dat_noap_check = inner_join(dat_flat %>% select(NoapIdentifier) %>% distinct,
                            dat_spatial_flat %>% select(NoapIdentifier) %>% distinct)

# Nice. This gets all of the flat NoapIdentifier values except two (138556/138558) (?), and 138556/144411 values from the spatial data.

# Then the general problem moving forward is (1) appending 1990-2014 and 2014-2024/5 and (2) interpreting rows
#  where joining spatial and flat data for 2014-2024/5 is subsidiary to (2)

# This might be a good point to work out any spatial operations for the 2014-2025 data.

# Get the replication data from Hashida and Lewis. R4 denotes non-industrial private owners; R5 denotes industrial private owners. Whatever that means.

# load("data/hvst_panel_07_R4.RData")
# load("data/hvst_panel_07_R5.RData")
# 
# write_csv(hvst_panel_07_R4, "data/Replication_HashidaLewis_R4.csv")
# write_csv(hvst_panel_07_R5, "data/Replication_HashidaLewis_R5.csv")

dat_hl_r4 = read_csv("data/Replication_HashidaLewis_R4.csv")
dat_hl_r5 = read_csv("data/Replication_HashidaLewis_R5.csv")

# Picking columns:

