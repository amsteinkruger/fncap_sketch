# Add other geodata to processed notification data.

# Note choice to drop spatial information from each "join" SpatVector after operations.

# Packages

library(tidyverse)
library(terra)
library(tidyterra)
library(readxl)

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
  mutate(UID = row_number()) %>% # paste0("B_", row_number())) %>% 
  select(UID, 
         Landowner = LandOwnerName_Right,
         # Landowner_Type = LandOwnerType,
         Year, 
         Month, 
         YearMonth, 
         MBF, 
         Acres) %>% 
  project("EPSG:2992")

# Elevation

dat_elevation = "data/OR_DEM_10M.gdb.zip" %>% rast

#  Subset for benchmarking.

dat_elevation_less = dat_elevation %>% crop(ext(500000, 600000, 500000, 600000))

#  Subset notifications to match.

dat_notifications_less = dat_notifications %>% crop(ext(dat_elevation_less)) %>% select(UID) %>% slice_sample(n = 100)

dat_join_elevation = 
  dat_notifications_less %>% 
  extract(x = dat_elevation_less, 
          y = ., 
          fun = mean, 
          ID = FALSE,
          bind = TRUE) %>% 
  rename(Elevation = 2) %>% 
  as_tibble

# Slope

#  Ideally, (1) check whether there exists a slope file in /output/, then (2) read it if so, (3) else create/write one.

dat_slope = dat_elevation_less %>% terrain(v = "slope")

dat_join_slope = 
  dat_notifications_less %>% 
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
  select(WHICH) %>% 
  project("EPSG:2992")

dat_join_pyrome = 
  dat_notifications_less %>% 
  intersect(dat_pyrome) %>% 
  as_tibble

# Note that this does not account for notifications falling across pyromes.

# MTBS
#  Note use of full notifications, since subsets of notifications tend to return a null.

dat_mtbs = 
  "data/mtbs_perimeter_data" %>% 
  vect %>% 
  filter(substr(Event_ID, 1, 2) == "OR") %>% 
  project("EPSG:2992")

ggplot() +
  geom_spatvector(data = dat_mtbs, fill = "red", color = NA, alpha = 0.50) + 
  geom_spatvector(data = dat_notifications, fill = "blue", color = NA, alpha = 0.50) +
  theme_void()

# 1. No Buffer

dat_join_mtbs_1 = 
  dat_notifications %>% 
  intersect(dat_mtbs) %>% 
  as_tibble %>% 
  group_by(UID) %>% 
  summarize(MTBS_1 = n()) %>% 
  ungroup

# 2. 15km Buffer
#  Note switch back to subset of notifications.

dat_join_mtbs_2 = 
  dat_notifications_less %>% 
  buffer(width = 15 * 3280.84) %>% # Oregon GIC Lambert is in feet, so convert kilometer buffer width into feet.
  erase(dat_notifications_less) %>% 
  intersect(dat_mtbs) %>% 
  as_tibble %>% 
  group_by(UID) %>% 
  summarize(MTBS_2 = n()) %>% 
  ungroup

# 3. 30km Buffer

dat_join_mtbs_3 = 
  dat_notifications_less %>% 
  buffer(width = 30 * 3280.84) %>% # Oregon GIC Lambert is in feet, so convert kilometer buffer width into feet.
  erase(dat_notifications_less %>% buffer(width = 15 * 3280.84)) %>% 
  intersect(dat_mtbs) %>% 
  as_tibble %>% 
  group_by(UID) %>% 
  summarize(MTBS_3 = n()) %>% 
  ungroup

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
  
#  Map regions to notifications, then add prices. Note silliness with the subset of notifications here.

dat_join_prices = 
  dat_notifications_less %>% 
  intersect(dat_prices_regions) %>% 
  as_tibble %>% 
  left_join(dat_notifications %>% as_tibble %>% select(UID, Year),
            by = "UID") %>% 
  left_join(dat_prices, 
            by = c("Year", "Region")) %>% 
  select(UID, Price)

#  Soil Quality

#   Get nGRASGO?
