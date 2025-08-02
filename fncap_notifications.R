# Check out harvest notifications from different sources.

# Note that Hashida and Lewis should actually be Hashida and Fenichel; get references straight.

library(tidyverse)
library(terra)
library(tidyterra)
library(readxl)

options(scipen=999)

# Get spatial data. Exclude points and lines for now; clearcuts only appear as polygons.

# dat_points = "data/Points_Notifications.gdb" %>% vect
# dat_lines = "data/Lines_Notifications.gdb" %>% vect
dat_polygons = "data/Polygons_Notifications.gdb" %>% vect

# Get flat data.

dat_flat = read_excel("data/Flat_Notifications.xlsx")

# Set up a join.

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

dat_left = 
  dat_polygons %>% 
  mutate(LandOwnerName = LandOwners %>% str_trim,
         UID_Left = row_number()) %>% 
  select(NOAPID = NoapIdentifier,
         UnitID,
         UnitName,
         ActivityType = ActType,
         UID_Left,
         LandOwnerName_Left = LandOwnerName,
         DateStart_Left = StartDate,
         DateEnd_Left = EndDate,
         DateContinuationStart_Left = ContinuationIssueDate,
         DateContinutationEnd_Left = ContinuationExpiryDate,
         Link_Left = PDFLink)

dat = 
  left_join(dat_left,
            dat_right,
            multiple = "all",
            relationship = "many-to-many")

# Count UIDs kept on each side. (Should be all for both, likely isn't.)
#  All 234485 UID values from dat_left remain in dat.
#  Only 260140 of 326841 UID values from dat_right remain in dat. Check whether the missing observations are of interest (after working out filters).

# Work out filters.

dat_filter = 
  dat %>% 
  # as_tibble %>%
  # Sort out columns.
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
  # Filter.
  filter(ActivityType == "Clearcut/Overstory Removal") %>% 
  # filter(LandOwnerType %in% c("Individual/Private Forestland Ownership", 
  #                             "Partnership/Corporate Forestland Ownership",
  #                             "Private/Non-Profit")) %>% 
  filter(LandOwnerType == "Individual/Private Forestland Ownership") %>% 
  filter(ActivityUnit == "MBF") %>% 
  # Sort out columns again.
  select(-c(ActivityType, LandOwnerType, ActivityUnit)) %>% 
  # Tidy up a little.
  arrange(NOAPID, UnitID, UnitName) %>% 
  # Band-Aid for duplication.
  distinct %>% 
  # Get areas.
  cbind(., expanse(., unit = "ha") * 2.47) %>% # This is poor practice, but.
  rename(Acres = y)
  # Get slopes and elevations?

dat_filter_centroids = 
  dat_filter %>% 
  centroids
# Get slopes and elevations for centroids?

# Note duplication issue, e.g.:
dat_polygons %>% filter(NoapIdentifier == "2014-532-07703") # 2
dat_flat %>% filter(NoapIdentifier == "2014-532-07703") # 2
dat_filter %>% filter(NOAPID == "2014-532-07703") # 4
  
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

# writeVector(dat_join_2, 
#             "output/dat_or_notifications_join_20142025.gdb",
#             overwrite = TRUE)
# 
# rm(list = ls())
# 
# dat_join_20142025 = "output/dat_or_notifications_join_20142025.gdb" %>% vect

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

dat_hl_centroids = dat_hl_use %>% centroids

dat_hl_centroids %>% nrow
dat_hl_centroids %>% crop(ext(-14000000, -13400000, 5000000, 6000000)) %>% nrow

# Note that 26675 of 27404 observations have nonproblematic locations (at a glance). So, 729 (2%) appear to be misplaced.

dat_hl_centroids = dat_hl_centroids %>% crop(ext(-14000000, -13400000, 5000000, 6000000)) # Band-Aid for extent in earlier period.

dat_filter_centroids_less = dat_filter_centroids %>% crop(dat_hl_centroids) # Band-Aid for extent in later period.

# Combine.

dat_20142025_bind = 
  dat_filter_centroids_less %>%
  as_tibble %>%
  arrange(DateStart_Left) %>%
  mutate(UID = paste0("A_", row_number()),
         Year = year(DateStart_Left),
         Month = month(DateStart_Left),
         YearMonth = paste0(Year, ifelse(str_length(Month) < 2, "0", ""), Month),
         MBF = ActivityQuantity %>% as.numeric) %>%
  select(UID, Year, Month, YearMonth, MBF, Acres)

dat_19902014_bind = 
  dat_hl_centroids %>% 
  as_tibble %>% 
  arrange(desc(YearMonth)) %>% 
  mutate(UID = paste0("B_", row_number()),
         Year = Year %>% as.numeric,
         Month = Month %>% as.numeric) %>% 
  select(UID, Year, Month, YearMonth, MBF, Acres)

dat_bind = bind_rows(dat_20142025_bind, dat_19902014_bind) %>% mutate(Period = UID %>% str_sub(1, 1))

dat_bind %>% group_by(Year, Period) %>% summarize(count = n()) %>% ungroup %>% ggplot() + geom_col(aes(x = Year, y = count, fill = Period))
dat_bind %>% group_by(Year, Period) %>% summarize(MBF = sum(MBF)) %>% ungroup %>% ggplot() + geom_col(aes(x = Year, y = MBF, fill = Period))
dat_bind %>% group_by(Year, Period) %>% summarize(Acres = sum(Acres)) %>% ungroup %>% ggplot() + geom_col(aes(x = Year, y = Acres, fill = Period))

# So counts and MBF sums are both not quite right. Check multicounting of MBF over geometries and units on MBF. Acres look right!

# The ways in which counts and MBF don't line up (counts are a little high, MBF is a lot high) suggest aggregation matters. Ditto acres.

dat_bind %>% 
  group_by(Year) %>% 
  mutate(MBF_Bin = MBF %>% percent_rank()) %>% 
  ungroup %>% 
  filter((MBF_Bin < 0.95 & Period == "A") | Period == "B") %>% 
  group_by(Year, Period) %>% 
  summarize(MBF = sum(MBF)) %>% 
  ungroup %>% 
  ggplot() + 
  geom_col(aes(x = Year, 
               y = MBF, 
               fill = Period))

dat_bind %>% mutate(MBFAcre = MBF / Acres) %>% pull(MBFAcre) %>% log %>% hist

library(ggridges)

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
                      scale = 1)

dat_bind %>% 
  mutate(MBFAcre = MBF / Acres) %>% 
  filter(log(MBFAcre) > 0 & log(MBFAcre) < 5) %>% 
  ggplot() + 
  geom_density_ridges(aes(x = MBF %>% log,
                          y = Year %>% factor,
                          fill = Period,
                          color = Period),
                      scale = 1)

# So substantial difference in harvest notification MBF is the result of the top 5-10% (by MBF) of later observations being a lot . . . bigger?

# Comment above was w/ fewer plots. Clearly difference appears across the distribution. Note trend and abrupt change in mean, median, skew.

# Note that earlier extent Band-Aids (more or less) eliminate the possibility that non-West Side notifications drive the MBF difference.

# Compare w/ aggregate harvest statistics from ODF. Mind absence of metadata.

"data/dat_harvest.csv" %>% 
  read_csv %>% 
  ggplot() + 
  geom_col(aes(x = year,
               y = `Total Private`))

# Export for reference.

write_csv(dat_bind, "output/notifications_20250721.csv")
