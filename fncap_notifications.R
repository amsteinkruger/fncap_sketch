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
  filter(LandOwnerType %in% c("Partnership/Corporate Forestland Ownership", 
                              "Individual/Private Forestland Ownership", 
                              "Private/Non-Profit")) %>% 
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

dat_join_index = dat_join_1$UID %>% unique

dat_join_2 = 
  dat_polygons %>% 
  mutate(UID = row_number()) %>% 
  filter(UID %in% dat_join_index) %>% 
  select(UID) %>% 
  left_join(dat_join_1, by = "UID") %>% 
  select(NoapIdentifier, LOName, DateStart, DateEnd, DateContinuationStart, DateContinuationEnd, Link, MBF)

# spatial operations go here

# Then the general problem moving forward is (1) appending 1990-2014 and 2014-2024/5 and (2) interpreting rows

# Get the replication data from Hashida and Lewis. R4 denotes non-industrial private owners; R5 denotes industrial private owners. Whatever that means.

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
  "data/Replication_HashidaLewis_R4.csv" %>% 
  read_csv %>% 
  mutate(Type_Numeric = 5,
         Type_String = "Industrial Private")

dat_hl = bind_rows(dat_hl_r4, dat_hl_r5)

# This is a good spot to resolve spatial stuff.

# Picking columns:

dat_bind_hl = 
  dat_hl %>% 
  # Set up altered columns to keep.
  mutate(Month = ifelse(str_sub(month_yr, 2, 2) == "/", 
                        paste0(0, str_sub(month_yr, 1, 1)), 
                        str_sub(month_yr, 1, 2)),
         Year = str_sub(month_yr, -4, -1),
         Year_Month = paste0(Year, Month),
         UID = row_number()) %>% 
  # Keep columns.
  select(UID,
         Year, 
         Month, 
         Year_Month,
         Acres = ActAcreage,
         MBF = ActMbf)

dat_bind_flat = 
  dat_flat %>% 
  # Set up columns to keep.
  mutate(Month = SubmitDate %>% month %>% ifelse(str_length(.) < 2, paste0(0, .), .),
         Year = SubmitDate %>% year,
         Year_Month = paste0(Year, Month),
         UID = row_number() + max(dat_bind_hl$UID),
         MBF = ifelse(`Activity Units` == "MBF", `Unit Quantity`, NA)) %>% 
  # Keep columns.
  select(UID,
         Year, 
         Month, 
         Year_Month, 
         MBF)

# note that acres requires spatial operations
# note that this still requires filtering

# do: compute spatial statistics for newer data; have blank/bonus columns in bind-ed (bound) output; resolve unique ID questions for both datasets

# solving identification problem:
#  - assume all rows are actually unique
#  - next: subset (newer) data to activities and owners of interest (?)
