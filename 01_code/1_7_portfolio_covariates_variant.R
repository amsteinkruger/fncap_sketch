# Join time-variant covariates to notifications.

#  Clear the environment.

rm(list = ls())

#  Start timing. 

time_start = Sys.time()

#  Set up futures.

par_cores = 16

plan(multisession, workers = par_cores)

#  TOC:

#   Spatial

#    MTBS
#    VPD
#    CWD

#   Not Spatial

#    Prices
#    Effective Federal Funds Rate

#  Parcels

dat_parcels = "03_intermediate/dat_portfolios.gdb" %>% vect

dat_parcels_less = 
  dat_parcels %>% 
  distinct(Parcel) %T>% 
  writeVector("03_intermediate/dat_portfolios_distinct.gdb")
  
dat_parcels_more = 
  dat_parcels_less %>% 
  as_tibble %>% 
  mutate(Row_Parcel = row_number()) %T>% 
  write_csv("03_intermediate/dat_portfolios_distinct.csv")

#  Notifications

# dat_notifications = 
#   "03_intermediate/dat_notifications_1_6.gdb" %>% 
#   vect %>% 
#   makeValid(buffer = TRUE)
# 
# dat_notifications_less = 
#   dat_notifications %>% 
#   select(UID)
# 
# dat_notifications_years = 
#   dat_notifications %>% 
#   mutate(Year = DateStart %>% year) %>% 
#   select(UID, Year)
# 
# dat_notifications_quarters = 
#   dat_notifications %>% 
#   as_tibble %>% 
#   select(UID, DateStart, DateEnd) %>% 
#   # Get year-quarter components. 
#   mutate(YearStart = DateStart %>% year,
#          MonthStart = DateStart %>% month,
#          QuarterStart = MonthStart %>% multiply_by(1 / 3) %>% ceiling,
#          YearEnd = DateEnd %>% year,
#          MonthEnd = DateEnd %>% month,
#          QuarterEnd = MonthEnd %>% multiply_by(1 / 3) %>% ceiling) %>% # ,
#   # Year_Quarter = paste0(Year, "_Q", Quarter)) %>% 
#   # Get intervening years and quarters. 
#   mutate(Years = map2(YearStart, YearEnd, seq),
#          Quarters = seq(1, 4) %>% list) %>% 
#   unnest(Years) %>% 
#   unnest(Quarters) %>% 
#   # Get conditions for keeping quarters.
#   mutate(CheckStart = (Years == YearStart & Quarters < QuarterStart),
#          CheckEnd = (Years == YearEnd & Quarters > QuarterEnd)) %>% 
#   # Get year-quarter. 
#   mutate(YearQuarter = paste0(Years, "_Q", Quarters)) %>% 
#   # Clean up. 
#   filter(!CheckStart & !CheckEnd) %>% 
#   select(UID,
#          YearQuarter,
#          Year = Years,
#          Quarter = Quarters)

#  Bounds

dat_bounds = "03_intermediate/dat_bounds.gdb" %>% vect

# MTBS

dat_mtbs = 
  "02_data/1_7_1_MTBS/Perimeters" %>% 
  vect %>% 
  project("EPSG:2992") %>% 
  makeValid %>% 
  crop(dat_bounds) %>% 
  arrange(ig_date) %>% 
  mutate(
    Year_MTBS = ig_date %>% year,
    Month_MTBS = ig_date %>% month,
    Quarter_MTBS = ceiling(Month_MTBS / 3), 
    Year_Quarter_MTBS = paste0(Year_MTBS, "_", Quarter_MTBS),
    Row_MTBS = row_number()
  ) %>% 
  select(Row_MTBS, Year_Quarter_MTBS) %T>% 
  writeVector("03_intermediate/dat_mtbs.gdb")

fun_parcels_mtbs = function(Chunk){
  
  dat_parcels_less = 
    "03_intermediate/dat_portfolios_distinct.gdb" %>% 
    vect %>% 
    mutate(Row_Chunk = (row_number() * par_cores / nrow(.)) %>% ceiling) %>%
    filter(Row_Chunk == Chunk) %>% 
    select(Parcel)
    
  dat_parcels_more = 
    "03_intermediate/dat_portfolios_distinct.csv" %>% 
    read_csv %>% 
    mutate(Row_Chunk = (row_number() * par_cores / nrow(.)) %>% ceiling) %>%
    filter(Row_Chunk == Chunk) %>% 
    select(Parcel, Row_Parcel)
  
  vec_parcels = dat_parcels_more$Parcel
  
  dat_mtbs = "03_intermediate/dat_mtbs.gdb" %>% vect
  
  result = 
    dat_parcels_less %>% 
    distance(dat_mtbs) %>% 
    round(0) %>% 
    divide_by(3280.84) %>% # ft to km
    as_tibble %>% 
    mutate(Row_Parcel = row_number()) %>% 
    left_join(dat_parcels_more) %>% 
    select(-Row_Parcel) %>% 
    pivot_longer(-Parcel) %>% 
    group_by(Parcel) %>% 
    mutate(Row_MTBS = row_number()) %>% 
    ungroup %>% 
    left_join(dat_mtbs %>% as_tibble) %>% 
    select(Parcel, Distance = value, Year_Quarter_MTBS) %>% 
    group_by(Parcel, Year_Quarter_MTBS) %>% 
    summarize(
      Fire_0 = sum(Distance == 0),
      Fire_15 = sum(Distance <= 15),
      Fire_30 = sum(Distance <= 30)
    ) %>% 
    ungroup %>% 
    mutate(
      Fire_15_Doughnut = Fire_15 - Fire_0,
      Fire_30_Doughnut = Fire_30 - Fire_15
    ) %>% 
    left_join(
      expand_grid(
        Parcel = vec_parcels, 
        Year_Quarter_MTBS = paste0(rep(1985:2024, each = 4), "_", rep(1:4, times = length(1985:2024)))
      ),
      .
    ) %>% 
    arrange(Parcel, Year_Quarter_MTBS) %>% 
    mutate(across(starts_with("Fire"), ~ replace_na(.x, 0))) %>% 
    pivot_longer(starts_with("Fire"), names_to = "Variable", values_to = "Count") %>% 
    group_by(Parcel, Variable) %>% 
    mutate(
      across(
        Count,
        .fns = set_names(
          lapply(1:120, \(k) ~lag(.x, k)),
          paste0("Lag_", 1:120)
        )
      )
    ) %>% 
    ungroup %>% 
    filter(Year_Quarter_MTBS > "2014_4") %>% 
    rename(Count_Lag_0 = Count) %>% 
    rename_with(.cols = starts_with("Count"), ~ str_remove(.x, "Count_")) %>% 
    pivot_wider(names_from = "Variable", values_from = starts_with("Lag"))
  
    return(result)
  
  }

dat_parcels_mtbs = 
  tibble(Chunk = 1:par_cores) %>% 
  mutate(Data_Parcels = Chunk %>% future_map(fun_parcels_mtbs, .options = furrr_options(seed = TRUE))) %>% 
  unnest(Data_Parcels) %>% 
  select(-Chunk) %T>% 
  write_csv("03_intermediate/dat_parcels_mtbs.csv")

dat_portfolios_mtbs = 
  dat_parcels %>% 
  as_tibble %>% 
  select(Owner_Cotality, Parcel, Year_Quarter) %>% 
  left_join(dat_parcels_mtbs, by = c("Parcel", "Year_Quarter" = "Year_Quarter_MTBS")) %T>% 
  write_csv("03_intermediate/dat_portfolios_mtbs.csv")

# VPD

dat_vpd = "03_intermediate/data_vpd.tif" %>% rast

dat_parcels_vpd = 
  tibble(Chunk = 1:par_cores) %>% 
  mutate(
    Data_Parcels = 
      Chunk %>% 
      future_map(
        ~ "03_intermediate/dat_portfolios_distinct.gdb" %>% 
          vect %>% 
          mutate(Chunk = (row_number() * par_cores / nrow(.)) %>% ceiling) %>%
          filter(Chunk == .x) %>% 
          select(Parcel) %>% 
          extract(
            x = "03_intermediate/data_vpd.tif" %>% rast,
            y = .,
            fun = mean,
            ID = FALSE,
            bind = TRUE
          ) %>%
          as_tibble %>% 
          pivot_longer(
            -Parcel, 
            names_to = "Year_Month", 
            names_prefix = "VPD_", 
            values_to = "VPD"
          ) %>% 
          mutate(
            Year = Year_Month %>% str_split_i("_", 1) %>% as.numeric,
            Month = Year_Month %>% str_split_i("_", 2) %>% as.numeric,
            Quarter = Month %>% multiply_by(1 / 3) %>% ceiling,
            Year_Quarter = paste0(Year, "_", Quarter)
          ) %>% 
          group_by(Parcel, Year_Quarter) %>% 
          summarize(VPD = mean(VPD, na.rm = TRUE)) %>% 
          group_by(Parcel) %>% 
          mutate(
            across(
              VPD,
              .fns = set_names(
                lapply(1:40, \(k) ~lag(.x, k)),
                paste0("Lag_", 1:40)
              )
            )
          ) %>% 
          ungroup %>% 
          filter(Year_Quarter > "2014_4") %>% 
          rename(VPD_Lag_0 = VPD), 
        .options = furrr_options(seed = TRUE)
      )
  ) %>% 
  unnest(Data_Parcels) %>%
  select(-Chunk)

dat_portfolios_vpd = 
  dat_parcels %>% 
  as_tibble %>% 
  select(Owner_Cotality, Parcel, Year_Quarter) %>% 
  left_join(dat_parcels_vpd) %T>% 
  write_csv("03_intermediate/dat_portfolios_vpd.csv")

# CWD

dat_cwd = "03_intermediate/data_cwd.tif" %>% rast

dat_parcels_cwd = 
  dat_parcels_less %>% 
  terra::extract(dat_cwd, ., fun = mean, na.rm = TRUE) %>% 
  bind_cols(dat_parcels_less %>% as_tibble, .) %>% 
  select(-ID) %>% 
  pivot_longer(
    cols = !Parcel,
    names_to = "Year",
    values_to = "CWD"
  ) %>% 
  mutate(Year = Year %>% as.numeric) %>% 
  full_join(
    tibble(Year = rep(2005:2025, each = 4), 
           Quarter = rep(1:4, length(2005:2025))),
    relationship = "many-to-many"
  ) %>% 
  mutate(Year_Quarter = paste0(Year, "_", Quarter)) %>% 
  select(Parcel, Year_Quarter, CWD) %>% 
  group_by(Parcel) %>% 
  mutate(across(CWD, setNames(lapply(1:40, \(k) ~ lag(.x, k)), paste0("Lag_", 1:40)))) %>% 
  ungroup %>% 
  rename(CWD_Lag_0 = CWD) %T>% 
  write_csv("03_intermediate/data_parcels_cwd.csv")

dat_parcels_cwd = 
  tibble(Chunk = 1:par_cores) %>% 
  mutate(
    Data_Parcels = 
      Chunk %>% 
      future_map(
        ~ "03_intermediate/dat_portfolios_distinct.gdb" %>% 
          vect %>% 
          mutate(Chunk = (row_number() * par_cores / nrow(.)) %>% ceiling) %>%
          filter(Chunk == .x) %>% 
          select(Parcel) %>% 
          extract(
            x = "03_intermediate/data_cwd.tif" %>% rast,
            y = .,
            fun = mean,
            ID = FALSE,
            bind = TRUE
          ) %>%
          as_tibble %>% 
          rename_with(.cols = starts_with("X"), ~ str_replace(.x, "X", "")) %>% 
          pivot_longer(
            cols = !Parcel,
            names_to = "Year",
            values_to = "CWD"
          ) %>%
          mutate(Year = Year %>% as.numeric) %>%
          full_join(
            tibble(Year = rep(2005:2025, each = 4),
                   Quarter = rep(1:4, length(2005:2025))),
            relationship = "many-to-many"
          ) %>%
          mutate(Year_Quarter = paste0(Year, "_", Quarter)) %>%
          select(Parcel, Year_Quarter, CWD) %>%
          group_by(Parcel) %>%
          mutate(across(CWD, setNames(lapply(1:40, \(k) ~ lag(.x, k)), paste0("Lag_", 1:40)))) %>%
          ungroup %>%
          filter(Year_Quarter > "2014_4") %>% 
          rename(CWD_Lag_0 = CWD),
        .options = furrr_options(seed = TRUE)
      )
  ) %>% 
  unnest(Data_Parcels) %>%
  select(-Chunk)

dat_portfolios_cwd = 
  dat_parcels %>% 
  as_tibble %>% 
  select(Owner_Cotality, Parcel, Year_Quarter) %>% 
  left_join(dat_parcels_cwd) %T>% 
  write_csv("03_intermediate/dat_portfolios_cwd.csv")
  
# Prices

#  Producer Price Index, BLS via FRED

dat_ppi = 
  "02_data/1_7_2_BLS/data_ppi_lumber.csv" %>% 
  read_csv %>% 
  mutate(
    Year = observation_date %>% year,
    Month = observation_date %>% month,
    Quarter = Month %>% multiply_by(1 / 3) %>% ceiling,
    Year_Quarter = paste0(Year, "_Q", Quarter)
  ) %>% 
  filter(Year %in% 2005:2025) %>% 
  group_by(Year_Quarter) %>% 
  summarize(PPI = WPU08 %>% mean) %>% 
  ungroup %>% 
  mutate(
    Check = Year_Quarter == max(Year_Quarter),
    Reference = ifelse(Check, PPI, NA) %>% max(na.rm = TRUE),
    Factor_PPI = Reference / PPI
  ) %>% 
  select(Year_Quarter, Factor_PPI)

#  Stumpage, LogLines/FastMarkets

dat_price_stumpage =
  "03_intermediate/data_stumpage.csv" %>% 
  read_csv %>% 
  left_join(dat_ppi) %>% 
  mutate(Price_Stumpage_DouglasFir = Price_Stumpage_DouglasFir * Factor_PPI,
         Price_Stumpage_WesternHemlock = Price_Stumpage_WesternHemlock * Factor_PPI) %>% 
  select(Year_Quarter, starts_with("Price_Stumpage_")) %>% 
  arrange(Year_Quarter) %>% 
  mutate(across(starts_with("Price_Stumpage_"), setNames(lapply(1:40, \(k) ~ lag(.x, k)), paste0("Lag_", 1:40))))

#  Lumber Prices, FastMarkets

dat_price_lumber = 
  "03_intermediate/data_lumber.csv" %>% 
  read_csv %>% 
  left_join(dat_ppi) %>% 
  pivot_longer(cols = starts_with("Price")) %>% 
  mutate(value = value * Factor_PPI) %>% 
  pivot_wider(names_from = name,
              values_from = value) %>% 
  select(-Factor_PPI) %>% 
  mutate(across(starts_with("Price"), setNames(lapply(1:40, \(k) ~ lag(.x, k)), paste0("Lag_", 1:40))))

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
  mutate(across(Rate, setNames(lapply(1:40, \(k) ~ lag(.x, k)), paste0("Lag_", 1:40)))) %>% 
  filter(Year_Quarter > "2004_Q4" & Year_Quarter < "2025_Q1") %>% 
  left_join(dat_notifications_quarters %>% select(UID, Year_Quarter = YearQuarter), .)

#  Aggregate parcel covariates to portfolios and export. 



#  Stop timing. 

time_end = Sys.time()

time_end - time_start
