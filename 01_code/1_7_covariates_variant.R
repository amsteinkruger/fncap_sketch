# Join time-variant covariates to notifications.

#  Goal: data object with time-variant covariates for all quarters of notifications
#  then in 1_8, use those to identify first harvest quarter, then reduce to only harvest quarters
#  (remember one-year lag to get minimal interannual NDVI comparison by quarters)

#  Clear the environment.

rm(list = ls())

#  Start timing. 

time_start = Sys.time()

#  TOC:

#    MTBS (Lag)
#    VPD (Lag)
#    other climate, weather, fire (Lag)
#    Prices (Lag)
#    Effective Federal Funds Rate (Lag)

#  Notifications

dat_notifications = 
  "03_intermediate/dat_notifications_1_7.gdb" %>% 
  vect

dat_notifications_less = 
  dat_notifications %>% 
  select(UID)

dat_notifications_years = 
  dat_notifications %>% 
  mutate(Year = DateStart %>% year) %>% 
  select(UID, Year)

#  Bounds

dat_bounds = "03_intermediate/dat_bounds.gdb" %>% vect

# MTBS

dat_mtbs = 
  "02_data/1_6_2_MTBS/Perimeters" %>% 
  vect %>% 
  project("EPSG:2992") %>% 
  makeValid %>% 
  crop(dat_bounds) %>% 
  mutate(Year_MTBS = ig_date %>% year, 
         .keep = "none")

# 1. No Buffer

dat_join_mtbs_0 = 
  dat_notifications_years %>% 
  intersect(dat_mtbs) %>% 
  as_tibble %>% 
  filter(Year > Year_MTBS & Year - 30 < Year_MTBS) %>% 
  group_by(UID) %>% 
  summarize(Fire_0 = n()) %>% 
  ungroup

# 2. 15km Buffer

dat_join_mtbs_15 = 
  dat_notifications_years %>% 
  buffer(width = 15 * 3280.84) %>% # Kilometers to feet.
  intersect(dat_mtbs) %>% 
  as_tibble %>% 
  filter(Year > Year_MTBS & Year - 30 < Year_MTBS) %>% 
  group_by(UID) %>% 
  summarize(Fire_15 = n()) %>% 
  ungroup

# 3. 30km Buffer

dat_join_mtbs_30 = 
  dat_notifications_years %>% 
  buffer(width = 30 * 3280.84) %>% # Kilometers to feet.
  intersect(dat_mtbs) %>% 
  as_tibble %>% 
  filter(Year >= Year_MTBS & Year - 30 < Year_MTBS) %>% 
  group_by(UID) %>% 
  summarize(Fire_30 = n()) %>% 
  ungroup

# 4. Combine

dat_join_mtbs = 
  dat_notifications_less %>% 
  as_tibble %>% 
  left_join(dat_join_mtbs_0) %>% 
  left_join(dat_join_mtbs_15) %>% 
  left_join(dat_join_mtbs_30) %>% 
  mutate(Fire_0 = Fire_0 %>% replace_na(0),
         Fire_15 = Fire_15 %>% replace_na(0),
         Fire_30 = Fire_30 %>% replace_na(0),
         Fire_15_Doughnut = Fire_15 - Fire_0,
         Fire_30_Doughnut = Fire_30 - Fire_15)

# VPD

dat_vpd = "03_intermediate/dat_vpd.tif" %>% rast

dat_vpd_annual = 
  tibble(year = 1984:2024) %>%
  mutate(string = paste0("VPD_", year)) %>%
  mutate(data = string %>% map( ~ { dat_vpd %>% select(starts_with(.x)) %>% mean(na.rm = TRUE) })) %>% # Get annual means.
  mutate(data =
           data %>%
           map2(.x = .,
                .y = string,
                ~ {
                  names(.x) <- as.character(.y)
                  .x
                })) %>% # Restore names.
  magrittr::extract2("data") %>% # Equivalent to .$data.
  reduce(c) 

dat_join_vpd = 
  tibble(year = 2014:2025) %>% 
  mutate(string = paste0("VPD_", year),
         years = year %>% map(~ seq(.x - 30, .x - 1)) %>% map(as.character),
         data = 
           years %>% 
           map( ~ dat_vpd_annual %>% select(ends_with(.x)) %>% mean(na.rm = TRUE)) %>% 
           map2(.x = .,
                .y = string,
                ~ {
                  names(.x) <- as.character(.y)
                  .x
                })) %>% 
  magrittr::extract2("data") %>% # Equivalent to .$data.
  reduce(c) %>% 
  extract(., 
          dat_notifications_less, 
          mean, 
          na.rm = TRUE) %>% 
  bind_cols((dat_notifications_less$UID %>% tibble(UID = .))) %>% 
  select(-ID) %>% 
  as_tibble %>% 
  left_join((dat_notifications_less %>% as_tibble), ., by = "UID") %>% 
  pivot_longer(cols = starts_with("VPD"),
               names_prefix = "VPD_",
               names_to = "Year",
               values_to = "VPD") %>% 
  group_by(UID) %>% 
  nest(data = c(Year, VPD)) %>% 
  ungroup %>% 
  left_join(dat_notifications %>% 
              as_tibble %>% 
              mutate(Year = DateStart %>% year) %>% 
              select(UID, Year), 
            .) %>% 
  mutate(data = data %>% map2(.x = ., .y = Year, .f = ~ filter(.x, Year == .y))) %>% 
  select(-Year) %>% 
  unnest(data) %>% 
  select(UID, VPD)

# Prices

#  Get notifications by quarter. 

dat_notifications_quarters = 
  dat_notifications %>% 
  mutate(Year_Quarter = 
           paste0(DateStart %>% 
                    year, 
                  "_Q", 
                  DateStart %>% 
                    month %>% 
                    multiply_by(1 / 3) %>% 
                    ceiling)) %>% 
  select(UID, Year_Quarter) %>% 
  as_tibble

#  Indexes

#   PPI (Timber)

dat_ppi_timber = 
  "02_data/1_6_7_BLS/data_ppi_timber.csv" %>% 
  read_csv %>% 
  mutate(Year = observation_date %>% year,
         Month = observation_date %>% month,
         Quarter = Month %>% multiply_by(1 / 3) %>% ceiling,
         Year_Quarter = paste0(Year, "_Q", Quarter)) %>% 
  filter(Year %in% 2012:2025) %>% # Note that this product originates in 2011.
  group_by(Year_Quarter) %>% 
  summarize(PPI_Timber = WPU08510502 %>% mean) %>% 
  ungroup %>% 
  mutate(Check = Year_Quarter == max(Year_Quarter),
         Reference = ifelse(Check, PPI_Timber, NA) %>% max(na.rm = TRUE),
         Factor_PPI_Timber = Reference / PPI_Timber) %>% 
  select(Year_Quarter, Factor_PPI_Timber)

#   PPI (Lumber)

dat_ppi_lumber =
  "02_data/1_6_7_BLS/data_ppi_lumber.csv" %>%
  read_csv %>%
  mutate(Year = observation_date %>% year,
         Month = observation_date %>% month,
         Quarter = Month %>% multiply_by(1 / 3) %>% ceiling,
         Year_Quarter = paste0(Year, "_Q", Quarter)) %>%
  filter(Year %in% 2010:2025) %>%
  group_by(Year_Quarter) %>%
  summarize(PPI_Lumber = WPU0811 %>% max) %>%
  ungroup %>%
  mutate(Check = Year_Quarter == max(Year_Quarter),
         Reference = ifelse(Check, PPI_Lumber, NA) %>% max(na.rm = TRUE),
         Factor_PPI_Lumber = Reference / PPI_Lumber) %>%
  select(Year_Quarter, Factor_PPI_Lumber)

#   Join.

dat_ppi = full_join(dat_ppi_lumber, dat_ppi_timber)

#   Prices

#    Stumpage, LogLines/FastMarkets

dat_price_stumpage = 
  "02_data/1_6_8_FastMarkets/data_stumpage.csv" %>% 
  read_csv %>% 
  rename(Stumpage_Nominal = 2) %>% 
  mutate(Year_Quarter = 
           paste0(str_sub(Quarter, 1, 4), 
                  "_", 
                  str_sub(Quarter, -2, -1))) %>% 
  select(Year_Quarter, Stumpage_Nominal) %>% 
  filter(Year_Quarter > "2011_Q4" & Year_Quarter < "2025_Q1") %>% 
  left_join(dat_ppi) %>% 
  mutate(Stumpage_Real = Stumpage_Nominal * Factor_PPI_Timber) %>% 
  select(Year_Quarter, Stumpage = Stumpage_Real) %>% 
  arrange(Year_Quarter) %>% 
  mutate(across(Stumpage, setNames(lapply(1:20, \(k) ~ lag(.x, k)), paste0("Lag_", 1:20))))

#   Lumber Prices, FastMarkets

dat_price_lumber =
  "02_data/1_6_8_FastMarkets/data_pull_filter.csv" %>%
  read_csv %>%
  select(1:2, Lumber_Nominal = 4) %>% 
  mutate(Quarter = Month %>% multiply_by(1 / 3) %>% ceiling,
         Year_Quarter = paste0(Year, "_Q", Quarter)) %>% 
  select(Year_Quarter, Lumber_Nominal) %>% 
  filter(Year_Quarter > "2009_Q4" & Year_Quarter < "2025_Q1") %>% 
  left_join(dat_ppi) %>% 
  mutate(Lumber_Real = Lumber_Nominal * Factor_PPI_Lumber) %>% 
  select(Year_Quarter, Lumber = Lumber_Real) %>% 
  group_by(Year_Quarter) %>% 
  summarize(across(everything(), ~ mean(.x))) %>% 
  ungroup %>% 
  arrange(Year_Quarter) %>% 
  mutate(across(Lumber, setNames(lapply(1:20, \(k) ~ lag(.x, k)), paste0("Lag_", 1:20))))

#  Join

dat_join_price = 
  dat_notifications %>% 
  select(UID, DateStart) %>% 
  mutate(Year = DateStart %>% year,
         Month = DateStart %>% month,
         Quarter = Month %>% multiply_by(1 / 3) %>% ceiling,
         Year_Quarter = paste0(Year, "_Q", Quarter)) %>% 
  select(UID, Year_Quarter) %>% 
  left_join(dat_price_stumpage) %>% 
  left_join(dat_price_lumber)

#  Effective Federal Funds Rate

dat_join_rate = 
  "02_data/1_6_9_FRED/FEDFUNDS.csv" %>% 
  read_csv %>% 
  mutate(Year = observation_date %>% year,
         Month = observation_date %>% month,
         Quarter = Month %>% multiply_by(1 / 3) %>% ceiling,
         Year_Quarter = paste0(Year, "_Q", Quarter),
         Rate_Fed = FEDFUNDS) %>% 
  group_by(Year_Quarter) %>% 
  summarize(Rate_Fed = Rate_Fed %>% mean) %>% 
  ungroup %>% 
  left_join(dat_notifications_quarters, 
            .) %>% 
  select(UID, Rate_Fed)

#  Export

dat_notifications_out = 
  dat_notifications %>% 
  left_join(dat_join_mtbs) %>% 
  left_join(dat_join_vpd) %>% 
  left_join(dat_join_price) %>% 
  left_join(dat_join_rate) %T>% 
  # Export with spatial data. 
  writeVector("03_intermediate/dat_notifications_1_6.gdb") %>% 
  # Export without spatial data. 
  as_tibble %T>% 
  write_csv("03_intermediate/dat_notifications_1_6.csv")

#  Stop timing. 

time_end = Sys.time()

time_end - time_start
