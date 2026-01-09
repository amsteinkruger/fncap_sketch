# Process notification data.

library(tidyverse)
library(terra)
library(tidyterra)
library(readxl)

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

# Export.

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
  # Tidy up.
  arrange(NOAPID, UnitID, UnitName, DateSubmit) %>% 
  # Band-Aid for duplication.
  distinct %>% 
  # Get areas.
  cbind(., expanse(., unit = "ha") * 2.47) %>% 
  rename(Acres = y)

writeVector(dat_join_polygons_out, "output/dat_notifications_polygons.gdb")

dat_join_centroids_out = 
  dat_join_polygons_out %>% 
  centroids

writeVector(dat_join_centroids_out, "output/dat_notifications_centroids.gdb")

dat_join_flat_out = 
  dat_join_polygons_out %>% 
  as_tibble

write_csv(dat_join_flat_out, "output/dat_notifications_flat.csv")
