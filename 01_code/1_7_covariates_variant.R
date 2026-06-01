# Join time-variant covariates to notifications.

#  Clear the environment.

rm(list = ls())

#  Start timing. 

time_start = Sys.time()

#  TOC:

#   Spatial

#    MTBS
#    VPD
#    Temperature
#    Precipitation
#    CWD

#   Not Spatial

#    Prices
#    Effective Federal Funds Rate

#  Notifications

dat_notifications = 
  "03_intermediate/dat_notifications_1_6.gdb" %>% 
  vect %>% 
  makeValid(buffer = TRUE)

dat_notifications_less = 
  dat_notifications %>% 
  select(UID)

dat_notifications_years = 
  dat_notifications %>% 
  mutate(Year = DateStart %>% year) %>% 
  select(UID, Year)

dat_notifications_quarters = 
  dat_notifications %>% 
  as_tibble %>% 
  select(UID, DateStart, DateEnd) %>% 
  # Get year-quarter components. 
  mutate(YearStart = DateStart %>% year,
         MonthStart = DateStart %>% month,
         QuarterStart = MonthStart %>% multiply_by(1 / 3) %>% ceiling,
         YearEnd = DateEnd %>% year,
         MonthEnd = DateEnd %>% month,
         QuarterEnd = MonthEnd %>% multiply_by(1 / 3) %>% ceiling) %>% # ,
  # Year_Quarter = paste0(Year, "_Q", Quarter)) %>% 
  # Get intervening years and quarters. 
  mutate(Years = map2(YearStart, YearEnd, seq),
         Quarters = seq(1, 4) %>% list) %>% 
  unnest(Years) %>% 
  unnest(Quarters) %>% 
  # Get conditions for keeping quarters.
  mutate(CheckStart = (Years == YearStart & Quarters < QuarterStart),
         CheckEnd = (Years == YearEnd & Quarters > QuarterEnd)) %>% 
  # Get year-quarter. 
  mutate(YearQuarter = paste0(Years, "_Q", Quarters)) %>% 
  # Clean up. 
  filter(!CheckStart & !CheckEnd) %>% 
  select(UID,
         YearQuarter,
         Year = Years,
         Quarter = Quarters)

#  Bounds

dat_bounds = "03_intermediate/dat_bounds.gdb" %>% vect

# MTBS

dat_mtbs = 
  "02_data/1_7_1_MTBS/Perimeters" %>% 
  vect %>% 
  project("EPSG:2992") %>% 
  makeValid %>% 
  crop(dat_bounds) %>% 
  mutate(Year_MTBS = ig_date %>% year,
         Month_MTBS = ig_date %>% month,
         Quarter_MTBS = Month_MTBS %>% multiply_by(1 / 3) %>% ceiling, 
         .keep = "none")

#  No Buffer

dat_join_mtbs_0 = 
  dat_notifications_less %>% 
  full_join(dat_notifications_quarters) %>% 
  makeValid(buffer = TRUE) %>% # A handful of polygons become invalid on joining. 
  intersect(dat_mtbs) %>% 
  as_tibble %>% 
  mutate(Quarters = Year * 4 + Quarter,
         Quarters_MTBS = Year_MTBS * 4 + Quarter_MTBS,
         across(Quarters, lapply(0:20, \(k) ~ .x - k))) %>% 
  select(UID, YearQuarter, starts_with("Quarters_")) %>% 
  pivot_longer(cols = starts_with("Quarters_") & !ends_with("MTBS"),
               names_to = "Lag",
               values_to = "Quarters") %>% 
  mutate(Lag = Lag %>% str_split_i("_", 2) %>% as.numeric %>% `-` (1),
         Check = (Quarters - Quarters_MTBS) %in% 0:120) %>% 
  group_by(UID, YearQuarter, Lag) %>% 
  summarize(Fire_0 = sum(Check)) %>% 
  ungroup

#  15km Buffer

dat_join_mtbs_15 = 
  dat_notifications_less %>% 
  buffer(width = 15 * 3280.84) %>% # Kilometers to feet.
  full_join(dat_notifications_quarters) %>% 
  makeValid(buffer = TRUE) %>% # A handful of polygons become invalid on joining. 
  intersect(dat_mtbs) %>% 
  as_tibble %>% 
  mutate(Quarters = Year * 4 + Quarter,
         Quarters_MTBS = Year_MTBS * 4 + Quarter_MTBS,
         across(Quarters, lapply(0:20, \(k) ~ .x - k))) %>% 
  select(UID, YearQuarter, starts_with("Quarters_")) %>% 
  pivot_longer(cols = starts_with("Quarters_") & !ends_with("MTBS"),
               names_to = "Lag",
               values_to = "Quarters") %>% 
  mutate(Lag = Lag %>% str_split_i("_", 2) %>% as.numeric %>% `-` (1),
         Check = (Quarters - Quarters_MTBS) %in% 0:120) %>% 
  group_by(UID, YearQuarter, Lag) %>% 
  summarize(Fire_15 = sum(Check)) %>% 
  ungroup

#  30km Buffer

dat_join_mtbs_30 = 
  dat_notifications_less %>% 
  buffer(width = 30 * 3280.84) %>% # Kilometers to feet.
  full_join(dat_notifications_quarters) %>% 
  makeValid(buffer = TRUE) %>% # A handful of polygons become invalid on joining. 
  intersect(dat_mtbs) %>% 
  as_tibble %>% 
  mutate(Quarters = Year * 4 + Quarter,
         Quarters_MTBS = Year_MTBS * 4 + Quarter_MTBS,
         across(Quarters, lapply(0:20, \(k) ~ .x - k))) %>% 
  select(UID, YearQuarter, starts_with("Quarters_")) %>% 
  pivot_longer(cols = starts_with("Quarters_") & !ends_with("MTBS"),
               names_to = "Lag",
               values_to = "Quarters") %>% 
  mutate(Lag = Lag %>% str_split_i("_", 2) %>% as.numeric %>% `-` (1),
         Check = (Quarters - Quarters_MTBS) %in% 0:120) %>% 
  group_by(UID, YearQuarter, Lag) %>% 
  summarize(Fire_30 = sum(Check)) %>% 
  ungroup

#  Proportion within fire perimeters. Skipping this to avoid geospatial pain. 

# dat_join_mtbs_proportion = 
#   dat_notifications_less %>% 
#   left_join(dat_notifications %>% as_tibble %>% select(UID, Acres_1)) %>% 
#   full_join(dat_notifications_quarters) %>% 
#   makeValid(buffer = TRUE) %>% # A handful of polygons become invalid on joining. 
#   intersect(dat_mtbs) %>% 
#   filter(Year_MTBS > (Year_MTBS - 30)) %>% 
#   filter(Year_MTBS < Year | (Year_MTBS == Year & Quarter_MTBS < Quarter)) %>% 
#   select(UID, YearQuarter, Acres_1) %>% 
#   aggregate(by = c("UID", "YearQuarter", "Acres_1")) %>% 
#   mutate(Acres_Burnt = expanse(., unit = "ha") * 2.47105381,
#          Fire_Proportion = Acres_Burnt / Acres_1) %>% 
#   select(UID, YearQuarter, Fire_Proportion) %>% 
#   as_tibble %>% 
#   # Band-Aid for upstream polygon handling issues. Remember to fix those. 
#   mutate(Fire_Proportion = ifelse(Fire_Proportion > 1, 1, Fire_Proportion))

# Combine

dat_join_mtbs = 
  dat_join_mtbs_0 %>% 
  left_join(dat_join_mtbs_15) %>% 
  left_join(dat_join_mtbs_30) %>% 
  mutate(Fire_15_Doughnut = Fire_15 - Fire_0,
         Fire_30_Doughnut = Fire_30 - Fire_15) %>% 
  pivot_wider(names_from = Lag,
              names_prefix = "Lag_",
              values_from = starts_with("Fire")) %>% 
  left_join(dat_notifications_quarters %>% as_tibble, .) %>% 
  mutate(across(starts_with("Fire"), ~ replace_na(.x, 0)))

# VPD

dat_vpd = "03_intermediate/dat_vpd.tif" %>% rast

dat_notifications_vpd = 
  dat_notifications_less %>% 
  terra::extract(dat_vpd, 
                 ., 
                 fun = mean,
                 na.rm = TRUE) %T>% 
  write_csv("03_intermediate/data_notifications_vpd.csv")

dat_join_vpd = 
  dat_notifications_vpd %>% 
  bind_cols(dat_notifications_less %>% as_tibble,
            .) %>% 
  select(-ID) %>% 
  pivot_longer(cols = starts_with("VPD"),
               names_prefix = "VPD_",
               names_to = "Year_Month",
               values_to = "VPD") %>% 
  mutate(Year = Year_Month %>% str_split_i("_", 1) %>% as.numeric,
         Month = Year_Month %>% str_split_i("_", 2) %>% as.numeric,
         Quarter = Month %>% multiply_by(1 / 3) %>% ceiling,
         Year_Quarter = paste0(Year, "_Q", Quarter)) %>% 
  group_by(UID, Year_Quarter) %>% 
  summarize(VPD = mean(VPD, na.rm = TRUE)) %>% 
  group_by(UID) %>% 
  mutate(across(VPD, setNames(lapply(1:20, \(k) ~ lag(.x, k)), paste0("Lag_", 1:20)))) %>% 
  ungroup

# Temperature

#  Mean

dat_tmean = "03_intermediate/data_tmean.tif" %>% rast

dat_notifications_tmean = 
  dat_notifications_less %>% 
  terra::extract(dat_tmean, 
                 ., 
                 fun = mean,
                 na.rm = TRUE) %T>% 
  write_csv("03_intermediate/data_notifications_tmean.csv")

dat_join_tmean = 
  dat_notifications_tmean %>% 
  bind_cols(dat_notifications_less %>% as_tibble,
            .) %>% 
  select(-ID) %>% 
  pivot_longer(cols = starts_with("TMean"),
               names_prefix = "TMean_",
               names_to = "Year_Month",
               values_to = "TMean") %>% 
  mutate(Year = Year_Month %>% str_split_i("_", 1) %>% as.numeric,
         Month = Year_Month %>% str_split_i("_", 2) %>% as.numeric,
         Quarter = Month %>% multiply_by(1 / 3) %>% ceiling,
         Year_Quarter = paste0(Year, "_Q", Quarter)) %>% 
  group_by(UID, Year_Quarter) %>% 
  summarize(TMean = mean(TMean, na.rm = TRUE)) %>% 
  group_by(UID) %>% 
  mutate(across(TMean, setNames(lapply(1:20, \(k) ~ lag(.x, k)), paste0("Lag_", 1:20)))) %>% 
  ungroup


#  Max

dat_tmax = "03_intermediate/data_tmax.tif" %>% rast

dat_notifications_tmax = 
  dat_notifications_less %>% 
  terra::extract(dat_tmax, 
                 ., 
                 fun = mean,
                 na.rm = TRUE) %T>% 
  write_csv("03_intermediate/data_notifications_tmax.csv")

dat_join_tmax = 
  dat_notifications_tmax %>% 
  bind_cols(dat_notifications_less %>% as_tibble,
            .) %>% 
  select(-ID) %>% 
  pivot_longer(cols = starts_with("TMax"),
               names_prefix = "TMax_",
               names_to = "Year_Month",
               values_to = "TMax") %>% 
  mutate(Year = Year_Month %>% str_split_i("_", 1) %>% as.numeric,
         Month = Year_Month %>% str_split_i("_", 2) %>% as.numeric,
         Quarter = Month %>% multiply_by(1 / 3) %>% ceiling,
         Year_Quarter = paste0(Year, "_Q", Quarter)) %>% 
  group_by(UID, Year_Quarter) %>% 
  summarize(TMax = mean(TMax, na.rm = TRUE)) %>% 
  group_by(UID) %>% 
  mutate(across(TMax, setNames(lapply(1:20, \(k) ~ lag(.x, k)), paste0("Lag_", 1:20)))) %>% 
  ungroup

# Precipitation

dat_ppt = "03_intermediate/data_ppt.tif" %>% rast

dat_notifications_ppt = 
  dat_notifications_less %>% 
  terra::extract(dat_ppt, 
                 ., 
                 fun = mean,
                 na.rm = TRUE) %T>% 
  write_csv("03_intermediate/data_notifications_ppt.csv")

dat_join_ppt = 
  dat_notifications_ppt %>% 
  bind_cols(dat_notifications_less %>% as_tibble,
            .) %>% 
  select(-ID) %>% 
  pivot_longer(cols = starts_with("PPT"),
               names_prefix = "PPT_",
               names_to = "Year_Month",
               values_to = "PPT") %>% 
  mutate(Year = Year_Month %>% str_split_i("_", 1) %>% as.numeric,
         Month = Year_Month %>% str_split_i("_", 2) %>% as.numeric,
         Quarter = Month %>% multiply_by(1 / 3) %>% ceiling,
         Year_Quarter = paste0(Year, "_Q", Quarter)) %>% 
  group_by(UID, Year_Quarter) %>% 
  summarize(PPT = mean(PPT, na.rm = TRUE)) %>% 
  group_by(UID) %>% 
  mutate(across(PPT, setNames(lapply(1:20, \(k) ~ lag(.x, k)), paste0("Lag_", 1:20)))) %>% 
  ungroup

# CWD

dat_cwd = "03_intermediate/data_cwd.tif" %>% rast

dat_notifications_cwd = 
  dat_notifications_less %>% 
  terra::extract(dat_cwd,
                 .,
                 fun = mean,
                 na.rm = TRUE) %T>% 
  write_csv("03_intermediate/data_notifications_cwd.csv")

dat_join_cwd = 
  dat_notifications_cwd %>% 
  bind_cols(dat_notifications_less %>% as_tibble,
            .) %>% 
  select(-ID) %>% 
  pivot_longer(cols = !UID,
               names_to = "Year",
               values_to = "CWD") %>% 
  mutate(Year = Year %>% as.numeric) %>% 
  full_join(tibble(Year = rep(2010:2025, each = 4), 
                   Quarter = rep(1:4, length(2010:2025))),
            relationship = "many-to-many") %>% 
  mutate(Year_Quarter = paste0(Year, "_Q", Quarter)) %>% 
  group_by(UID) %>% 
  mutate(across(CWD, setNames(lapply(1:20, \(k) ~ lag(.x, k)), paste0("Lag_", 1:20)))) %>% 
  ungroup

# Prices

#  Indexes

#   PPI (Timber)

dat_ppi_timber = 
  "02_data/1_7_2_BLS/data_ppi_timber.csv" %>% 
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
  "02_data/1_7_2_BLS/data_ppi_lumber.csv" %>%
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

# add Western hemlock here

dat_price_stumpage = 
  "02_data/1_7_3_FastMarkets/data_stumpage.csv" %>% 
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

# add product forms and/or Western hemlock here

dat_price_lumber =
  "02_data/1_7_3_FastMarkets/data_pull_filter.csv" %>%
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
  dat_notifications_quarters %>% 
  select(UID, Year_Quarter = YearQuarter) %>% 
  left_join(dat_price_stumpage) %>% 
  left_join(dat_price_lumber)

#  Effective Federal Funds Rate

dat_join_rate = 
  "02_data/1_7_4_FRED/FEDFUNDS.csv" %>% 
  read_csv %>% 
  mutate(Year = observation_date %>% year,
         Month = observation_date %>% month,
         Quarter = Month %>% multiply_by(1 / 3) %>% ceiling,
         Year_Quarter = paste0(Year, "_Q", Quarter),
         Rate = FEDFUNDS) %>% 
  group_by(Year_Quarter) %>% 
  summarize(Rate = Rate %>% mean) %>% 
  ungroup %>% 
  mutate(across(Rate, setNames(lapply(1:20, \(k) ~ lag(.x, k)), paste0("Lag_", 1:20)))) %>% 
  filter(Year_Quarter > "2009_Q4" & Year_Quarter < "2025_Q1") %>% 
  left_join(dat_notifications_quarters %>% 
              select(UID, Year_Quarter = YearQuarter), 
            .)

#  Export

dat_notifications_out = 
  dat_join_mtbs %>% 
  left_join(dat_join_vpd %>% rename(YearQuarter = Year_Quarter),
            by = c("UID", "YearQuarter")) %>% 
  left_join(dat_join_ppt %>% rename(YearQuarter = Year_Quarter),
            by = c("UID", "YearQuarter")) %>% 
  left_join(dat_join_tmean %>% rename(YearQuarter = Year_Quarter),
            by = c("UID", "YearQuarter")) %>% 
  left_join(dat_join_tmax %>% rename(YearQuarter = Year_Quarter),
            by = c("UID", "YearQuarter")) %>% 
  left_join(dat_join_price %>% rename(YearQuarter = Year_Quarter), 
            by = c("UID", "YearQuarter")) %>% 
  left_join(dat_join_rate %>% rename(YearQuarter = Year_Quarter), 
            by = c("UID", "YearQuarter")) %T>% 
  # Export without spatial data. 
  write_csv("03_intermediate/dat_notifications_1_7.csv")

#  Stop timing. 

time_end = Sys.time()

time_end - time_start
