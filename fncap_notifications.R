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

dat_points_flat = dat_points %>% as.data.frame %>% as_tibble %>% mutate(SpatialType = "Point", UID = row_number())
dat_lines_flat = dat_lines %>% as.data.frame %>% as_tibble %>% mutate(SpatialType = "Line", UID = row_number())
dat_polygons_flat = dat_polygons %>% as.data.frame %>% as_tibble %>% mutate(SpatialType = "Polygon", UID = row_number())

dat_spatial_flat = bind_rows(dat_points_flat, dat_lines_flat, dat_polygons_flat)

# First, check merges on NoapIdentifier. Note that NoapIdentifiers are nonunique, and/but a lot of rows are nonunique on all fields (???).

dat_noap_check = inner_join(dat_flat %>% select(NoapIdentifier) %>% distinct,
                            dat_spatial_flat %>% select(NoapIdentifier) %>% distinct)

# Nice. This gets all of the flat NoapIdentifier values except two (138556/138558) (?), and 138556/144411 values from the spatial data.

# Next, work out rows and columns to (1) keep and (2) join on. In particular, join flat data to spatial data.

dat_flat_join = 
  dat_flat %>% 
  # Filter to clearcuts.
  filter(`Activity Type` == "Clearcut/Overstory Removal") %>% 
  # Filter to private (?) owners.
  # filter(LandOwnerType %in% c("Partnership/Corporate Forestland Ownership", 
  #                             "Individual/Private Forestland Ownership", 
  #                             "Private/Non-Profit")) %>% 
  filter(LandOwnerType == "Partnership/Corporate Forestland Ownership") %>% 
  # Filter to observations in MBF. Note other observations might be recoverable, but they're annoying.
  filter(`Activity Units` == "MBF") %>% 
  # Manipulate columns.
  #  Consolidate landowner names into a single column.
  mutate(across(starts_with("LO"), ~ifelse(. == "NULL", "", .)),
         across(c("LOFirstName", "LOMiddleName"), ~ifelse(. == "", "", paste0(., " "))),
         `LO Company Name` = ifelse(`LO Company Name` == "", "", paste0(", ", `LO Company Name`)),
         LOName = paste0(LOFirstName, LOMiddleName, LOLastName, `LO Company Name`) %>% str_trim) %>% 
  #  Handle MBF.
  mutate(MBF = `Unit Quantity` %>% as.numeric) %>% 
  # Pick columns.
  select(NoapIdentifier, 
         LOName, 
         # DateSubmit = SubmitDate, 
         DateStart = `Actvity StartDate`, 
         DateEnd = `Activity EndDate`, MBF) %>% 
  # Reduce to unique observations. Note that this is problematic (41630 to 37921).
  distinct %>% 
  group_by(across(-MBF)) %>% 
  summarize(MBF = sum(MBF, na.rm = TRUE)) %>% 
  ungroup

dat_spatial_flat_join = 
  dat_spatial_flat %>% 
  # Filter to clearcuts.
  filter(ActType == "Clearcut/Overstory Removal") %>% 
  # Filter to private (?) owners? Nope, can't.
  # Filter to observations in MBF? Can't.
  # No other filters of interest?
  # Clean up strings.
  mutate(LandOwners = LandOwners %>% str_trim) %>% 
  # Pick columns.
  select(NoapIdentifier, 
         LONameSpatial = LandOwners, 
         # DateSubmit = SubmitDate, 
         DateStart = StartDate, 
         DateEnd = EndDate, 
         DateContinuationStart = ContinuationIssueDate,
         DateContinuationEnd = ContinuationExpiryDate,
         Link = PDFLink,
         SpatialType,
         UID) %>% 
  # Reduce to unique observations. This is also problematic.
  distinct %>% 
  # Add a flag for joins.
  mutate(Join = 1)

dat_join_1 = 
  left_join(dat_flat_join, 
            dat_spatial_flat_join, 
            by = c("NoapIdentifier", "DateStart", "DateEnd")) %>% 
  filter(Join == 1) %>% 
  group_by(UID) %>% 
  filter(n() == 1) %>% # Cut many-many matches.
  ungroup

# Note that many-many matches are appearing as of 2025/07/21?

dat_join_index = dat_join_1$UID %>% unique

dat_join_2 = 
  dat_polygons %>% 
  mutate(UID = row_number()) %>% 
  filter(UID %in% dat_join_index) %>% 
  select(UID) %>% 
  left_join(dat_join_1, by = "UID") %>% 
  select(NoapIdentifier, LOName, DateStart, DateEnd, DateContinuationStart, DateContinuationEnd, Link, MBF) %>% 
  cbind(., expanse(., unit = "ha") * 2.47) %>% # This is poor practice, but.
  rename(Acres = y)

# Check out runtime with a DEM to get elevation and slope.

# dat_elevation_10m = "data/OR_DEM_10M.gdb.zip" %>% rast
# dat_elevation_100m = dat_elevation_10m %>% aggregate(fact = 13, fun = mean) 

# dat_join_try = 
#   dat_join_2 %>% 
#   filter(row_number() %in% 1:25) %>% 
#   project(crs(dat_elevation_10m)) %>% 
#   extract(dat_elevation, ., fun = "mean")

# 5 minutes for 25 obs > 7432 minutes (little bit more than 5 days) for 37162 obs. 
# Recall that output of terra::extract() is a flat dataframe of row IDs and extracted results; here, ID and (mean) elevation.

# Export recent notifications for reference.

writeVector(dat_join_2, 
            "output/dat_or_notifications_join_20142025.gdb",
            overwrite = TRUE)

rm(list = ls())

dat_join_20142025 = "output/dat_or_notifications_join_20142025.gdb" %>% vect

# Get replication data from Hashida and Lewis. R4 denotes non-industrial private owners; R5 denotes industrial private owners. Whatever that means.

# load("data/hvst_panel_07_R4.RData")
# load("data/hvst_panel_07_R5.RData")
# 
# write_csv(hvst_panel_07_R4, "data/Replication_HashidaLewis_R4.csv")
# write_csv(hvst_panel_07_R5, "data/Replication_HashidaLewis_R5.csv")

dat_hl_r4 = 
  "data/Replication_HashidaLewis_R4.csv" %>% 
  read_csv %>% 
  mutate(Type_Numeric = 4,
         Type_String = "Non-Industrial Private")


dat_hl_r5 = 
  "data/Replication_HashidaLewis_R5.csv" %>% 
  read_csv %>% 
  mutate(Type_Numeric = 5,
         Type_String = "Industrial Private")

dat_hl = 
  bind_rows(dat_hl_r4, dat_hl_r5) %>% 
  # Keep only observations with harvest (following sitedata.R from Hashida and Fenichel (2021)).
  filter(hvst == 1) %>% 
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

dat_hl_join = 
  dat_hl %>% 
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

dat_qq_hl = 
  inner_join(dat_hl_join,
             dat_qq_join)

dat_hl_use = 
  dat_qq %>% 
  inner_join(dat_qq_hl) %>% 
  select(Meridian, QuarterQuarter_Section_Township_Range, Sec_Town_Q) %>% 
  left_join(dat_hl) %>% 
  select(UID, Year, Month, YearMonth, Acres, MBF)

# Combine.

dat_20142025_bind = 
  dat_join_20142025 %>% 
  as_tibble %>% 
  arrange(desc(DateStart)) %>% 
  mutate(UID = paste0("A_", row_number()),
         Year = year(DateStart),
         Month = month(DateStart),
         YearMonth = paste0(Year, ifelse(str_length(Month) < 2, "0", ""), Month)) %>% 
  select(UID, Year, Month, YearMonth, MBF, Acres)

dat_19902014_bind = 
  dat_hl_use %>% 
  as_tibble %>% 
  arrange(desc(YearMonth)) %>% 
  mutate(UID = paste0("B_", row_number()),
         Year = Year %>% as.numeric,
         Month = Month %>% as.numeric) %>% 
  select(UID, Year, Month, YearMonth, MBF, Acres)

dat_bind = bind_rows(dat_20142025_bind, dat_19902014_bind)
  
dat_bind %>% group_by(Year) %>% summarize(count = n()) %>% ungroup %>% ggplot() + geom_col(aes(x = Year, y = count))
dat_bind %>% group_by(Year) %>% summarize(MBF = sum(MBF)) %>% ungroup %>% ggplot() + geom_col(aes(x = Year, y = MBF))

# So counts and MBF sums are both not quite right. Check multicounting of MBF over geometries and units on MBF.
# Export for reference.

write_csv(dat_bind, "output/notifications_20250721.csv")
