# Check out harvest notifications from different sources.

library(tidyverse)
library(terra)
library(tidyterra)
library(readxl)

options(scipen=999)

# Get spatial data. Exclude points and lines for now; clearcuts only appear as polygons.

dat_points = "data/Points_Notifications.gdb" %>% vect
dat_lines = "data/Lines_Notifications.gdb" %>% vect
dat_polygons = "data/Polygons_Notifications.gdb" %>% vect

# Get flat data.

dat_flat = read_excel("data/Flat_Notifications.xlsx")

# Set up joins.

dat_right = 
  dat_flat %>% 
  mutate(across(starts_with("LO"), ~ifelse(. == "NULL", "", .)),
         across(c("LOFirstName", "LOMiddleName"), ~ifelse(. == "", "", paste0(., " "))),
         `LO Company Name` = ifelse(`LO Company Name` == "", "", paste0(", ", `LO Company Name`)),
         LandOwnerName = paste0(LOFirstName, LOMiddleName, LOLastName, `LO Company Name`) %>% str_trim,
         UID_Right = row_number()) %>% 
  select(NOAPID = NoapIdentifier,
         UnitID = Id,
         UnitName = `Unit Name`,
         ActivityType = `Activity Type`,
         UID_Right,
         ActivityUnit_Right = `Activity Units`,
         ActivityQuantity_Right = `Unit Quantity`,
         LandOwnerType_Right = LandOwnerType,
         LandOwnerName_Right = LandOwnerName,
         DateSubmit_Right = SubmitDate,
         DateStart_Right = `Actvity StartDate`,
         DateEnd_Right = `Activity EndDate`)

dat_left_polygons = 
  dat_polygons %>% 
  mutate(LandOwnerName = LandOwners %>% str_trim,
         UID_Left = row_number()) %>% 
  select(NOAPID = NoapIdentifier,
         UnitID,
         UnitName,
         ActivityType = ActType,
         UID_Left,
         LandOwnerName_Left = LandOwnerName,
         DateSubmit_Left = SubmitDate,
         DateStart_Left = StartDate,
         DateEnd_Left = EndDate,
         DateContinuationStart_Left = ContinuationIssueDate,
         DateContinutationEnd_Left = ContinuationExpiryDate,
         Link_Left = PDFLink)

dat_left_lines = 
  dat_lines %>% 
  mutate(LandOwnerName = LandOwners %>% str_trim,
         UID_Left = row_number()) %>% 
  select(NOAPID = NoapIdentifier,
         UnitID,
         UnitName,
         ActivityType = ActType,
         UID_Left,
         LandOwnerName_Left = LandOwnerName,
         DateSubmit_Left = SubmitDate,
         DateStart_Left = StartDate,
         DateEnd_Left = EndDate,
         DateContinuationStart_Left = ContinuationIssueDate,
         DateContinutationEnd_Left = ContinuationExpiryDate,
         Link_Left = PDFLink)

dat_left_points = 
  dat_points %>% 
  mutate(LandOwnerName = LandOwners %>% str_trim,
         UID_Left = row_number()) %>% 
  select(NOAPID = NoapIdentifier,
         UnitID,
         UnitName,
         ActivityType = ActType,
         UID_Left,
         LandOwnerName_Left = LandOwnerName,
         DateSubmit_Left = SubmitDate,
         DateStart_Left = StartDate,
         DateEnd_Left = EndDate,
         DateContinuationStart_Left = ContinuationIssueDate,
         DateContinutationEnd_Left = ContinuationExpiryDate,
         Link_Left = PDFLink)
  
dat_join_polygons = 
  left_join(dat_left_polygons,
            dat_right,
            multiple = "all",
            relationship = "many-to-many") %>% 
  filter(!is.na(UID_Right))

dat_join_lines = 
  left_join(dat_left_lines,
            dat_right,
            multiple = "all",
            relationship = "many-to-many") %>% 
  filter(!is.na(UID_Right))

dat_join_points = 
  left_join(dat_left_points,
            dat_right,
            multiple = "all",
            relationship = "many-to-many") %>% 
  filter(!is.na(UID_Right))

# Count UIDs kept on each side.
#  All observations from the spatial data are kept in the join, with meaningful proportions of points, lines, and polygons filtered out afterward.
#  260140 observations from the flat data correspond to polygons.
#  43712 observations from the flat data correspond to lines.
#  11540 observations from the flat data correspond to points.
#  So, 315392 of 327841 flat observations (96%) correspond to spatial data. Nice.

# Set up centroids and combine spatial datasets.

# 2025/08/12: Band-Aid for export. Note that this breaks subsequent code.

dat_join_polygons_out = 
  dat_join_polygons %>% 
  select(NOAPID,
         UnitID,
         UnitName,
         ActivityType,
         ActivityUnit = ActivityUnit_Right,
         ActivityQuantity = ActivityQuantity_Right,
         LandOwnerName_Left,
         LandOwnerName_Right,
         LandOwnerType = LandOwnerType_Right,
         DateSubmit = DateSubmit_Right,
         DateStart_Left,
         DateStart_Right,
         DateEnd_Left,
         DateEnd_Right,
         DateContinuationStart = DateContinuationStart_Left,
         DateContinuationEnd = DateContinutationEnd_Left,
         Link = Link_Left) %>% 
  # Tidy up a little.
  arrange(NOAPID, UnitID, UnitName, DateSubmit) %>% 
  # Band-Aid for duplication.
  distinct %>% 
  # Get areas.
  cbind(., expanse(., unit = "ha") * 2.47) %>% # This is poor practice, but.
  rename(Acres = y)

writeVector(dat_join_polygons_out, "output/dat_polygons_20250812.gdb")

dat_centroids_polygons = 
  dat_join_polygons_out %>% 
  centroids

# Back to earlier code.

dat_centroids_lines = 
  dat_join_lines %>% 
  select(NOAPID,
         UnitID,
         UnitName,
         ActivityType,
         ActivityUnit = ActivityUnit_Right,
         ActivityQuantity = ActivityQuantity_Right,
         LandOwnerName_Left,
         LandOwnerName_Right,
         LandOwnerType = LandOwnerType_Right,
         DateSubmit = DateSubmit_Right,
         DateStart_Left,
         DateStart_Right,
         DateEnd_Left,
         DateEnd_Right,
         DateContinuationStart = DateContinuationStart_Left,
         DateContinuationEnd = DateContinutationEnd_Left,
         Link = Link_Left) %>% 
  # Tidy up a little.
  arrange(NOAPID, UnitID, UnitName, DateSubmit) %>% 
  # Band-Aid for duplication.
  distinct %>% 
  # Get areas.
  mutate(Acres = 0) %>% 
  # Get centroids.
  centroids

dat_centroids_points = 
  dat_join_points %>% 
  select(NOAPID,
         UnitID,
         UnitName,
         ActivityType,
         ActivityUnit = ActivityUnit_Right,
         ActivityQuantity = ActivityQuantity_Right,
         LandOwnerName_Left,
         LandOwnerName_Right,
         LandOwnerType = LandOwnerType_Right,
         DateSubmit = DateSubmit_Right,
         DateStart_Left,
         DateStart_Right,
         DateEnd_Left,
         DateEnd_Right,
         DateContinuationStart = DateContinuationStart_Left,
         DateContinuationEnd = DateContinutationEnd_Left,
         Link = Link_Left) %>% 
  # Tidy up a little.
  arrange(NOAPID, UnitID, UnitName, DateSubmit) %>% 
  # Band-Aid for duplication.
  distinct %>% 
  # Get areas.
  mutate(Acres = 0)

dat_centroids = 
  dat_centroids_points %>% 
  rbind(dat_centroids_lines) %>% 
  rbind(dat_centroids_polygons)

# Get replication data from Hashida and Fenichel. R4 denotes non-industrial private owners; R5 denotes industrial private owners. 

# load("data/hvst_panel_07_R4.RData")
# load("data/hvst_panel_07_R5.RData")
# 
# write_csv(hvst_panel_07_R4, "data/Replication_HashidaFenichel_R4.csv")
# write_csv(hvst_panel_07_R5, "data/Replication_HashidaFenichel_R5.csv")

dat_hf_r4 = 
  "data/Replication_HashidaFenichel_R4.csv" %>% 
  read_csv %>% 
  mutate(Type_Numeric = 4,
         Type_String = "Non-Industrial Private")


dat_hf_r5 = 
  "data/Replication_HashidaFenichel_R5.csv" %>% 
  read_csv %>% 
  mutate(Type_Numeric = 5,
         Type_String = "Industrial Private")

dat_hf = 
  bind_rows(dat_hf_r4, dat_hf_r5) %>% 
  # filter(Type_String == "Industrial Private") %>% 
  # Keep only observations with harvest (following sitedata.R from Hashida and Fenichel (2021)).
  # filter(hvst == 1) %>%
  # Set up altered columns to keep.
  mutate(Month = ifelse(str_sub(month_yr, 2, 2) == "/", 
                        paste0(0, str_sub(month_yr, 1, 1)), 
                        str_sub(month_yr, 1, 2)),
         Year = str_sub(month_yr, -4, -1),
         YearMonth = paste0(Year, Month),
         UID = row_number()) %>% 
  # Keep columns.
  select(UID,
         Sec_Town_Q,
         Year, 
         Month, 
         YearMonth,
         Landowner = ownername,
         Landowner_Type = Type_String,
         Harvest = hvst,
         Acres = ActAcreage,
         MBF = ActMbf)

# Resolve spatial stuff.

dat_qq = "data/PLSS_QQ_OR.gdb" %>% vect

dat_qq_flat = dat_qq %>% as.data.frame

dat_qq_join = 
  dat_qq_flat %>% 
  select(meridian, qq, sctn, township, township_char, range, range_char) %>% 
  mutate(Meridian = meridian,
         QuarterQuarter_Section_Township_Range = 
           paste0(qq, 
                  "_S", 
                  sctn, 
                  "_T", 
                  township, 
                  township_char, 
                  "_R", 
                  range, 
                  range_char)) %>% 
  distinct

dat_hf_join = 
  dat_hf %>% 
  separate_wider_delim(Sec_Town_Q, 
                       delim = " ", 
                       names = c("QuarterQuarter", 
                                 "Section", 
                                 "Township", 
                                 "Range"),
                       cols_remove = FALSE) %>% 
  mutate(QuarterQuarter = QuarterQuarter %>% str_replace(",", ""),
         QuarterQuarter_Section_Township_Range = 
           paste(QuarterQuarter, 
                 Section, 
                 Township, 
                 Range, 
                 sep = "_")) %>% 
  select(QuarterQuarter_Section_Township_Range,
         Sec_Town_Q) %>% 
  distinct

dat_qq_hf = 
  inner_join(dat_hf_join,
             dat_qq_join)

dat_hf_use = 
  dat_qq %>% 
  inner_join(dat_qq_hf) %>% 
  select(Meridian, QuarterQuarter_Section_Township_Range, Sec_Town_Q) %>% 
  left_join(dat_hf) %>% 
  select(UID, Year, Month, YearMonth, Landowner, Landowner_Type, Harvest, Acres, MBF)

dat_hf_centroids = dat_hf_use %>% centroids

dat_hf_centroids %>% nrow
dat_hf_centroids %>% crop(ext(-14000000, -13400000, 5000000, 6000000)) %>% nrow

# Note that 26675 of 27404 observations have nonproblematic locations (at a glance). So, 729 (2%) appear to be misplaced.

dat_hf_centroids = dat_hf_centroids %>% crop(ext(-14000000, -13400000, 5000000, 6000000)) # Band-Aid for extent in earlier period.

dat_centroids_less = dat_centroids %>% crop(dat_hf_centroids) # Band-Aid for extent in later period.

# Combine. Skip coordinates export (until after figuring out point shenanigans).

dat_20142025_bind = 
  dat_centroids_less %>%
  # project(y = "epsg:4326") %>% 
  # cbind(., crds(., df = TRUE)) %>% 
  as_tibble %>%
  filter(ActivityType == "Clearcut/Overstory Removal") %>% 
  filter(ActivityUnit == "MBF") %>% 
  mutate(Year = year(DateStart_Left),
         Month = month(DateStart_Left),
         YearMonth = paste0(Year, ifelse(str_length(Month) < 2, "0", ""), Month),
         MBF = ActivityQuantity %>% as.numeric) %>%
  arrange(desc(Year), desc(Month), LandOwnerType, LandOwnerName_Right, desc(MBF), desc(Acres)) %>% 
  mutate(UID = paste0("B_", row_number())) %>% 
  select(UID, 
         Landowner = LandOwnerName_Right,
         Landowner_Type = LandOwnerType,
         Year, 
         Month, 
         YearMonth, 
         MBF, 
         Acres)

dat_19902014_bind = 
  dat_hf_centroids %>% 
  as_tibble %>% 
  mutate(Year = Year %>% as.numeric,
         Month = Month %>% as.numeric) %>% 
  arrange(desc(Year), desc(Month), Landowner_Type, Landowner, desc(MBF), desc(Acres)) %>% 
  mutate(UID = paste0("A_", row_number())) %>% 
  select(UID, 
         Landowner,
         Landowner_Type,
         Year, 
         Month, 
         YearMonth, 
         Harvest,
         MBF, 
         Acres)

dat_bind = 
  bind_rows(dat_20142025_bind, dat_19902014_bind) %>% 
  mutate(Period = UID %>% str_sub(1, 1))

write_csv(dat_bind, "output/dat_notifications_20250804.csv")

# Code breaks on names from here on.

dat_bind_use = 
  dat_bind %>% 
  filter(Landowner_Type %in% c("Partnership/Corporate Forestland Ownership", "Industrial Private")) %>% 
  filter(is.na(Harvest) | Harvest == 1)

# Check out time series of aggregate values for counts of notifications, acres of notifications, and MBF.

dat_bind_use %>% group_by(Year, Period) %>% summarize(Count = n()) %>% ungroup %>% ggplot() + geom_col(aes(x = Year, y = Count, fill = Period))
dat_bind_use %>% group_by(Year, Period) %>% summarize(Acres = sum(Acres)) %>% ungroup %>% ggplot() + geom_col(aes(x = Year, y = Acres, fill = Period))
dat_bind_use %>% group_by(Year, Period) %>% summarize(MBF = sum(MBF)) %>% ungroup %>% ggplot() + geom_col(aes(x = Year, y = MBF, fill = Period))

# Check a distributional feature of MBF in the later period.

dat_bind_use %>% 
  group_by(Year, Period) %>% 
  mutate(Percentile = percent_rank(MBF)) %>% 
  filter(Percentile < 0.95) %>%
  summarize(MBF = sum(MBF)) %>% 
  ungroup %>% 
  ggplot() + 
  geom_col(aes(x = Year, 
               y = MBF, 
               fill = Period))

# Point being, outliers on MBF in the later period distort aggregate results (so, lose those).

# Check out time series of distributions of values for acres, MBF, and MBF/acre (by notification).

library(ggridges)

dat_bind %>% 
  ggplot() + 
  geom_density_ridges(aes(x = Acres %>% log,
                          y = Year %>% factor,
                          fill = Period,
                          color = Period),
                      scale = 1)

dat_bind %>% 
  ggplot() + 
  geom_density_ridges(aes(x = MBF %>% log,
                          y = Year %>% factor,
                          fill = Period,
                          color = Period),
                      scale = 1)

dat_bind %>% 
  mutate(MBFAcre = MBF / Acres) %>% 
  ggplot() + 
  geom_density_ridges(aes(x = MBFAcre %>% log,
                          y = Year %>% factor,
                          fill = Period,
                          color = Period),
                      scale = 1)

dat_bind %>% 
  mutate(MBFAcre = MBF / Acres) %>% 
  filter(log(MBFAcre) > 0 & log(MBFAcre) < 5) %>% 
  ggplot() + 
  geom_density_ridges(aes(x = MBFAcre %>% log,
                          y = Year %>% factor,
                          fill = Period,
                          color = Period),
                      scale = 1) +
  scale_x_continuous(limits = c(0, 5))

# Check out a time series of distributions of notifications over months.

dat_bind %>% 
  group_by(Period, Year, Month) %>% 
  summarize(Count = n()) %>% 
  ggplot() + 
  geom_density_ridges(aes(x = Count,
                          y = Month %>% factor,
                          fill = Period,
                          color = Period),
                      alpha = 0.50,
                      scale = 1)

dat_bind %>% 
  filter(Acres < 150) %>% 
  ggplot() + 
  geom_density_ridges(aes(x = Acres,
                          y = Month %>% factor,
                          fill = Period,
                          color = Period),
                      alpha = 0.50,
                      scale = 1)

dat_bind %>% 
  filter(MBF < 10000) %>%
  ggplot() + 
  geom_density_ridges(aes(x = MBF,
                          y = Month %>% factor,
                          fill = Period,
                          color = Period),
                      alpha = 0.50,
                      scale = 1)

# Compare w/ aggregate harvest statistics from ODF. Mind absence of metadata.

"data/dat_harvest.csv" %>% 
  read_csv %>% 
  filter(Year %in% 1990:2025) %>% 
  ggplot() + 
  geom_col(aes(x = Year,
               y = `Total Private`))

# Export for reference.

write_csv(dat_bind, "output/notifications_20250721.csv")
