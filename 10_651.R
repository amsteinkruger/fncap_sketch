# Process notification data for treatment sketch.

library(tidyverse)
library(terra)
library(tidyterra)
library(readxl)

# Get spatial data. Exclude points and lines for now; clearcuts only appear as polygons.

dat_polygons = "data/Polygons_Notifications.gdb" %>% vect

# Get flat data.

dat_flat = read_excel("data/Flat_Notifications.xlsx")

# Set up joins.

dat_right = 
  dat_flat %>% 
  mutate(across(starts_with("LO"), ~ifelse(. == "NULL", "", .)),
         across(c("LOFirstName", "LOMiddleName"), ~ifelse(. == "", "", paste0(., " "))),
         LOCompany = ifelse(`LO Company Name` == "", "", paste0(", ", `LO Company Name`)),
         LandOwnerName = paste0(LOFirstName, LOMiddleName, LOLastName, LOCompany) %>% str_trim, 
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
         LandOwnerCompany_Right = `LO Company Name`,
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

dat_join_polygons = 
  left_join(dat_left_polygons,
            dat_right,
            multiple = "all",
            relationship = "many-to-many") %>% 
  filter(!is.na(UID_Right))

# Wrap up.

dat_join_polygons_out = 
  dat_join_polygons %>% 
  select(NOAPID,
         UnitID,
         UnitName,
         ActivityType,
         ActivityUnit = ActivityUnit_Right,
         ActivityQuantity = ActivityQuantity_Right,
         LandOwnerName_Right,
         LandOwnerCompany_Right,
         LandOwnerType = LandOwnerType_Right,
         Date = DateStart_Right) %>% 
  # Tidy up.
  arrange(NOAPID, UnitID, UnitName, Date) %>% 
  # Band-Aid for duplication.
  distinct %>% 
  # Get areas.
  cbind(., expanse(., unit = "ha") * 2.47) %>% 
  rename(Acres = y)

# Set up geodata for bounds.

# Bounds

#  OR

dat_bounds_or = 
  "data/cb_2023_us_state_500k" %>% 
  vect %>% 
  filter(STUSPS == "OR") %>% 
  project("EPSG:2992")

#  Pyromes

dat_bounds_pyromes = 
  "data/USFS Pyromes/Data/Pyromes_CONUS_20200206.shp" %>% 
  vect %>% 
  rename(WHICH = NAME) %>% # Band-Aid for a reserved attribute name.
  filter(WHICH %in% c("Marine Northwest Coast Forest", "Klamath Mountains", "Middle Cascades")) %>% 
  select(Pyrome = WHICH) %>% 
  summarize(Pyrome = "All Pyromes") %>% 
  fillHoles %>% 
  project("EPSG:2992")

#  Intersection

dat_bounds = 
  intersect(dat_bounds_or, dat_bounds_pyromes) %>% 
  # Handle island polygons. These are not real islands.
  disagg %>% 
  cbind(., expanse(., unit = "ha")) %>% 
  filter(y == max(y)) %>% 
  select(-y)

# Filter.

vec_activities = 
  c("Site Preparation /Afforestation", 
    "Animal Repellent Application",
    "Fertilizer Application",
    "Fungicide Application",
    "Herbicide Application (Unit)",
    "Insecticide Application",
    "Rodenticide Application")

vec_activities = c("Site Preparation /Afforestation", "Herbicide Application (Unit)")

dat_effort = 
  dat_join_polygons_out %>% 
  filter(ActivityType %in% vec_activities) %>% 
  filter(LandOwnerType == "Partnership/Corporate Forestland Ownership") %>% 
  select(-ends_with("Right")) %>% 
  rename_with(~ sub("_Left$", "", .x), everything()) %>% 
  mutate(Year = year(Date),
         Month = month(Date)) %>%
  arrange(desc(Year), desc(Month)) %>% 
  filter(Year > 2014 & Year < 2025) %>% 
  select(Year,
         Month,
         ActivityType,
         Acres) %>% 
  project("EPSG:2992") %>% 
  # Fix invalid polygons.
  makeValid %>% 
  # Subset for quick tests.
  # slice_sample(n = 1000) %>%
  # Crop.
  crop(dat_bounds) %>% 
  # Assign unique ID.
  mutate(UID = row_number()) %>% 
  relocate(UID)

dat_effort_plant = 
  dat_effort %>% 
  filter(ActivityType == "Site Preparation /Afforestation") %>% 
  mutate(UID_Plant = row_number()) %>% 
  relocate(UID_Plant)

dat_effort_other = 
  dat_effort %>% 
  filter(ActivityType != "Site Preparation /Afforestation") %>% 
  mutate(UID_Other = row_number()) %>% 
  relocate(UID_Other)

# dat_effort_other = dat_effort_other %>% slice_sample(n = 1000)

# Intersect.

dat_effort_intersect_set = 
  relate(dat_effort_plant, 
         dat_effort_other, 
         relation = "intersects")

dat_effort_intersect = 
  dat_effort_intersect_set %>% 
  as_tibble %>% 
  rownames_to_column("from") %>% 
  pivot_longer(cols = -from,
               names_to = "to",
               values_to = "intersects") %>% 
  mutate(to = to %>% str_sub(2, -1),
         across(c(to, from), ~ as.integer(.x))) %>% 
  filter(intersects == TRUE) %>% 
  # Planting
  left_join(dat_effort_plant %>% as_tibble, by = c("from" = "UID_Plant")) %>% 
  rename(Year_Plant = Year, Month_Plant = Month) %>% 
  select(-ActivityType, -Acres, -UID) %>% 
  # Effort
  left_join(dat_effort_other %>% as_tibble, by = c("to" = "UID_Other")) %>% 
  rename(Year_Effort = Year, Month_Effort = Month) %>% 
  select(-Acres, -UID) %>% 
  # Tidying
  select(-intersects)

# Plot in relative years.

dat_effort_time = 
  dat_effort_intersect %>% 
  mutate(Year_Relative = Year_Plant - Year_Effort) %>% 
  # filter(Year_Relative > -5) %>% 
  group_by(from, Year_Relative, ActivityType) %>% 
  summarize(Count = n()) %>% 
  ungroup %>% 
  group_by(Year_Relative, ActivityType) %>% 
  summarize(Count = n()) %>% 
  ungroup

# library(fishualize)

vis_effort_time = 
  dat_effort_time %>% 
  ggplot() +
  geom_col(aes(x = Year_Relative %>% factor,
               y = Count),
           fill = pal_oranges) + # Note that this is assigned later in the script. 
  labs(x = "Years since afforestation",
       y = "Records") +
  theme_minimal()

ggsave("output/vis_present_time.png",
       vis_effort_time,
       dpi = 300,
       width = 7,
       height = 4)

# Get augmented data set up for regressions.

dat_effort_expand =
  dat_effort_intersect %>% 
  mutate(Year_Relative = Year_Plant - Year_Effort,
         Activity_0 = 0,
         Activity_1 = 1) %>% 
  pivot_longer(c(Activity_0, Activity_1),
               values_to = "ActivityEffort") %>% 
  select(from, 
         Year_Relative, 
         ActivityType, 
         ActivityEffort) %>% 
  expand(from, 
         Year_Relative, 
         ActivityType, 
         ActivityEffort) %>%
  # Subset to relevant choices.
  #  Planting Years
  left_join(dat_effort_intersect %>% select(from, Year_Plant)) %>% 
  filter(Year_Relative %in% -9:9) %>% 
  filter(Year_Plant + Year_Relative < 2025) %>% 
  filter(Year_Plant + Year_Relative > 2014) %>% 
  #  Effort Realizations
  left_join(dat_effort_intersect %>% 
              mutate(Year_Relative = Year_Plant - Year_Effort, Join = 1) %>% 
              select(from, Year_Relative, ActivityType, Join)) %>% 
  filter((ActivityEffort == 0 & is.na(Join)) | (ActivityEffort == 1 & Join == 1)) %>% 
  distinct %>% # Cleaning up after joins. Not the best approach
  select(-Join) %>% 
  # Augment.
  mutate(Year_Effort = Year_Plant + Year_Relative) %>% 
  left_join(dat_price_stumpage %>% select(Year, Stumpage = Stumpage_Real_PPI_Timber),
            by = c("Year_Effort" = "Year")) %>% 
  left_join(dat_join_rate %>% select(Year, Rate = Fed_Rate),
            by = c("Year_Effort" = "Year")) %>% 
  mutate(Rate = Rate * 100)

# Regressions

#  Linear Probability Models

# mod_repellent = 
#   dat_effort_expand %>% 
#   filter(ActivityType == "Animal Repellent Application") %>% 
#   lm(ActivityEffort ~ Year_Relative + Stumpage + Rate, data = .)

# mod_fertilizer = 
#   dat_effort_expand %>% 
#   filter(ActivityType == "Fertilizer Application") %>% 
#   lm(ActivityEffort ~ Year_Relative + Stumpage + Rate, data = .)

mod_herbicide = 
  dat_effort_expand %>% 
  filter(ActivityType == "Herbicide Application (Unit)") %>% 
  mutate(Year_Relative_Sq = Year_Relative^2) %>% 
  lm(ActivityEffort ~ Year_Relative + Year_Relative_Sq + Stumpage + Rate, data = .)

# mod_rodenticide = 
#   dat_effort_expand %>% 
#   filter(ActivityType == "Rodenticide Application") %>% 
#   lm(ActivityEffort ~ Year_Relative + Stumpage + Rate, data = .)

#  Probit

mod_herbicide_probit = 
  dat_effort_expand %>% 
  filter(ActivityType == "Herbicide Application (Unit)") %>% 
  mutate(Year_Relative_Sq = Year_Relative^2) %>% 
  glm(ActivityEffort ~ Year_Relative + Year_Relative_Sq + Stumpage + Rate, data = ., family = binomial(link = "probit"))

#  Output

library(sandwich)

# se_list <- 
#   list(sqrt(diag(vcovHC(mod_repellent, type = "HC1"))), 
#        sqrt(diag(vcovHC(mod_fertilizer, type = "HC1"))),
#        sqrt(diag(vcovHC(mod_herbicide, type = "HC1"))),
#        sqrt(diag(vcovHC(mod_fertilizer, type = "HC1"))))

library(stargazer)

stargazer(mod_herbicide, mod_herbicide_probit, 
          type = "html",
          # se = se_list,
          column.labels = c("LPM", "Probit"),
          keep.stat = c("n", "rsq", "f"),
          out = "output/estimates_20260303.html")

# data tables

library(magrittr)

dat_effort_expand %>% 
  filter(ActivityEffort == 1) %>% 
  group_by(ActivityType) %>% 
  summarize(Count = n()) %>% 
  ungroup %T>% 
  write_csv("output/summary_activities_20260303.csv")

# note that covariate summary statistics were in the console

# map

library(RColorBrewer)

pal_greens = brewer.pal('Greens', n = 9)[8]
pal_oranges = brewer.pal('Oranges', n = 9)[8]

vis_map_plant =  
  ggplot() + 
  geom_spatvector(data = dat_bounds_or,
                  color = NA,
                  fill = "grey90") +
  geom_spatvector(data = dat_bounds,
                  color = NA,
                  fill = "grey80") +
  geom_spatvector(data = dat_effort_plant %>% centroids, 
                  color = pal_greens,
                  fill = NA,
                  size = 0.33) +
  theme_void()

vis_map_other =  
  ggplot() + 
  geom_spatvector(data = dat_bounds_or,
                  color = NA,
                  fill = "grey90") +
  geom_spatvector(data = dat_bounds,
                  color = NA,
                  fill = "grey80") +
  geom_spatvector(data = dat_effort_other %>% centroids, 
                  color = pal_oranges,
                  fill = NA,
                  size = 0.33) +
  theme_void()

library(patchwork)

vis_map = vis_map_plant + vis_map_other + plot_annotation(tag_levels = 'A')

ggsave("output/vis_map.png",
       vis_map,
       dpi = 300,
       width = 6.5,
       height = 2.5)
