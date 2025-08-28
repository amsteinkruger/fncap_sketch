# Add other geodata to processed notification data.

# Note choice to drop spatial information from each "join" SpatVector after operations.

# Packages

library(tidyverse)
library(terra)
library(tidyterra)
library(readxl)

time_start = Sys.time()

# Notifications

dat_notifications = 
  "output/dat_polygons_20250812.gdb" %>% 
  vect %>% 
  filter(ActivityType == "Clearcut/Overstory Removal") %>% 
  filter(ActivityUnit == "MBF") %>% 
  filter(LandOwnerType == "Partnership/Corporate Forestland Ownership") %>% 
  mutate(Year = year(DateStart_Left),
         Month = month(DateStart_Left),
         YearMonth = paste0(Year, ifelse(str_length(Month) < 2, "0", ""), Month),
         MBF = ActivityQuantity %>% as.numeric) %>%
  arrange(desc(Year), desc(Month), LandOwnerType, LandOwnerName_Right, desc(MBF), desc(Acres)) %>% 
  select(Landowner = LandOwnerName_Right,
         Year, 
         Month, 
         YearMonth, 
         MBF, 
         Acres) %>% 
  project("EPSG:2992") %>% 
  # Check whether polygons are valid.
  cbind(., is.valid(.)) %>% 
  rename(Valid_0 = y) %>% 
  # Try fixing any invalid polygons.
  makeValid %>% 
  # Check again.
  cbind(., is.valid(.)) %>% 
  rename(Valid_1 = y) %>%
  # Drop columns. This would be a nice spot to return counts of valid, invalid, and fixed polygons as a side effect.
  select(-starts_with("Valid")) %>% 
  # Subset for testing.
  # slice_sample(n = 1000) %>%
  # Swap unique ID assignment to this step for convenience.
  mutate(UID = row_number())

# Try filtering on counts of vertices.

# dat_vertices = 
#   dat_notifications %>% 
#   geom %>% 
#   as_tibble %>% 
#   group_by(geom) %>% 
#   summarize(count = n()) %>% 
#   arrange(count) %>% 
#   mutate(rank = percent_rank(count)) %>% 
#   filter(rank < 0.5)

# Use the subset of spatial objects with fewer vertices for a filtering join on notifications.

# dat_notifications =
#   dat_notifications %>%
#   semi_join(dat_vertices, by = c("UID" = "geom"))

# Elevation (Data)

dat_elevation = "data/OR_DEM_10M.gdb.zip" %>% rast

# Slope (Data/Processing)

if (file.exists("output/dat_slope.tif")) {

  dat_slope = "output/dat_slope.tif" %>% rast

} else {

  dat_slope = dat_elevation %>% terrain(v = "slope")
  writeRaster(dat_slope, "output/dat_slope.tif", overwrite = TRUE)

}

# Elevation (Processing)

dat_join_elevation =
  dat_notifications %>%
  extract(x = dat_elevation,
          y = .,
          fun = mean,
          ID = FALSE,
          bind = TRUE) %>%
  rename(Elevation = 2) %>%
  as_tibble

# Slope (Processing)

dat_join_slope =
  dat_notifications %>%
  extract(x = dat_slope,
          y = .,
          fun = mean,
          ID = FALSE,
          bind = TRUE,
          na.rm = TRUE) %>%
  rename(Slope = 2) %>%
  as_tibble

#  Note warning about units and projection in terra documentation.

# Pyromes

dat_pyrome = 
  "data/USFS Pyromes/Data/Pyromes_CONUS_20200206.shp" %>% 
  vect %>% 
  rename(WHICH = NAME) %>% # Band-Aid for a reserved attribute name.
  filter(WHICH %in% c("Marine Northwest Coast Forest", "Klamath Mountains", "Middle Cascades")) %>% 
  select(Pyrome = WHICH) %>% 
  project("EPSG:2992")

dat_join_pyrome = 
  dat_notifications %>% 
  intersect(dat_pyrome) %>% 
  as_tibble %>% 
  select(UID, Pyrome)

# Note that this does not account for notifications falling across pyromes.

# MTBS

# Consider reorganizing for (1) better memory management around buffers (2) better memory management around which MTBS perimeters are worth intersecting

dat_mtbs = 
  "data/mtbs_perimeter_data" %>% 
  vect %>% 
  filter(substr(Event_ID, 1, 2) == "OR") %>% 
  project("EPSG:2992")

# ggplot() +
#   geom_spatvector(data = dat_mtbs, fill = "red", color = NA, alpha = 0.50) + 
#   geom_spatvector(data = dat_notifications, fill = "blue", color = NA, alpha = 0.50) +
#   theme_void()
# 
# ggsave("figures/vis_mtbs.png", dpi = 300, height = 8.5, width = 11)

# 1. No Buffer

dat_join_mtbs_1 = 
  dat_notifications %>% 
  intersect(dat_mtbs) %>% 
  as_tibble %>% 
  mutate(Year_MTBS = Ig_Date %>% year) %>% 
  filter(Year >= Year_MTBS & Year - 20 < Year_MTBS) %>% 
  group_by(UID) %>% 
  summarize(MTBS_1 = n()) %>% 
  ungroup

# 2. 15km Buffer, Difference
#  Note switch back to subset of notifications.

dat_join_mtbs_2 = 
  dat_notifications %>% 
  buffer(width = 15 * 3280.84) %>% # Oregon GIC Lambert is in feet, so convert kilometer buffer width into feet.
  erase(dat_notifications) %>% 
  intersect(dat_mtbs) %>% 
  as_tibble %>% 
  mutate(Year_MTBS = Ig_Date %>% year) %>% 
  filter(Year >= Year_MTBS & Year - 20 < Year_MTBS) %>% 
  group_by(UID) %>% 
  summarize(MTBS_2 = n()) %>% 
  ungroup

# 3. 30km Buffer, Difference

dat_join_mtbs_3 = 
  dat_notifications %>% 
  buffer(width = 30 * 3280.84) %>% 
  erase(dat_notifications %>% buffer(width = 15 * 3280.84)) %>% 
  intersect(dat_mtbs) %>% 
  as_tibble %>% 
  mutate(Year_MTBS = Ig_Date %>% year) %>% 
  filter(Year >= Year_MTBS & Year - 20 < Year_MTBS) %>% 
  group_by(UID) %>% 
  summarize(MTBS_3 = n()) %>% 
  ungroup

# 4. 15km Buffer, Union

dat_join_mtbs_4 = 
  dat_notifications %>% 
  buffer(width = 15 * 3280.84) %>% 
  intersect(dat_mtbs) %>% 
  as_tibble %>% 
  mutate(Year_MTBS = Ig_Date %>% year) %>% 
  filter(Year >= Year_MTBS & Year - 20 < Year_MTBS) %>% 
  group_by(UID) %>% 
  summarize(MTBS_4 = n()) %>% 
  ungroup

# 5. 30km Buffer, Union

dat_join_mtbs_5 = 
  dat_notifications %>% 
  buffer(width = 30 * 3280.84) %>% 
  intersect(dat_mtbs) %>% 
  as_tibble %>% 
  mutate(Year_MTBS = Ig_Date %>% year) %>% 
  filter(Year >= Year_MTBS & Year - 20 < Year_MTBS) %>% 
  group_by(UID) %>% 
  summarize(MTBS_5 = n()) %>% 
  ungroup

# 6. Combine

dat_join_mtbs = 
  dat_join_mtbs_1 %>% 
  full_join(dat_join_mtbs_2, by = "UID") %>% 
  full_join(dat_join_mtbs_3, by = "UID") %>% 
  full_join(dat_join_mtbs_4, by = "UID") %>% 
  full_join(dat_join_mtbs_5, by = "UID") %>%
  select(UID,
         Fire_0 = MTBS_1,
         Fire_15_Difference = MTBS_2,
         Fire_30_Difference = MTBS_3,
         Fire_15_Union = MTBS_4,
         Fire_30_Union = MTBS_5)

# Prices

# Note that this is a placeholder, pending decisions around species, price bins, and regional definitions.

#  Get prices.

dat_prices_19802018 = 
  "data/Prices_ODF/Historic Timber Price Data.xlsx" %>% 
  read_xlsx(sheet = 1) %>% 
  filter(Quarter == 1) %>% 
  select(Year, Region, Price = `DF 2S`)

dat_prices_20192022 =
  "data/Prices_ODF/Historic Timber Price Data.xlsx" %>% 
  read_xlsx(sheet = 2,
            skip = 1) %>% 
  select(YearQuarter = 1, 
         Price_1 = 2, 
         Price_2 = 4,
         Price_3 = 6,
         Price_4 = 8) %>% 
  mutate(Year = YearQuarter %>% str_sub(1, 4),
         Quarter = YearQuarter %>% str_sub(-1, -1)) %>% 
  filter(Quarter == 1) %>% 
  select(Year, starts_with("Price")) %>% 
  pivot_longer(cols = starts_with("Price"),
               names_to = "Region",
               names_prefix = "Price_",
               values_to = "Price") %>% 
  mutate(Year = Year %>% as.numeric,
         Region = Region %>% as.numeric)

dat_prices = bind_rows(dat_prices_19802018, dat_prices_20192022)
  
#  Get price regions.

dat_prices_regions = 
  "data/Boundaries_ODF/StateForestDistricts" %>% 
  vect %>% 
  mutate(Region = 
           case_when(DISTRICT %in% c("Astoria", "Tillamook", "Forest Grove", "North Cascade", "West Oregon", "Western Lane") ~ 1,
                     DISTRICT %in% c("Coos Bay") ~ 2,
                     DISTRICT %in% c("Grants Pass") ~ 4,
                     TRUE ~ NA),
         .keep = "none") %>% 
  drop_na(Region)
  
#  Map regions to notifications, then add prices. 

dat_join_prices = 
  dat_notifications %>% 
  intersect(dat_prices_regions) %>% 
  as_tibble %>% 
  left_join(dat_prices, 
            by = c("Year", "Region")) %>% 
  select(UID, Price)

# Inflation Adjustment?

# Soil Quality

#  Note "Catastrophic Failure" message (!) from gNATSGO download.

# Finale

dat_notifications_out = 
  dat_notifications %>% 
  left_join(dat_join_pyrome, by = "UID") %>% 
  left_join(dat_join_elevation %>% select(UID, Elevation = OR_DEM_10M.gdb), by = "UID") %>% # Kick this up to the elevation block.
  left_join(dat_join_slope %>% select(UID, Slope = slope), by = "UID") %>% # Ditto.
  left_join(dat_join_mtbs, by = "UID") %>% 
  mutate(across(starts_with("Fire"), ~ replace_na(.x, 0))) %>% # (Fix!) Accounting for observations dropped in MTBS intersect/filter steps.
  left_join(dat_join_prices, by = "UID")

# Check reason for additional observations from start to finish.
  
writeVector(dat_notifications_out, "output/data_notifications_demo_20250824.gdb", overwrite = TRUE)

write_csv(dat_notifications_out %>% as_tibble, "output/data_notifications_demo_20250824.csv")

time_end = Sys.time()

time_end - time_start
