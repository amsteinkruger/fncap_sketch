# Aggregate over the panel of notifications by firm-year-quarter.

#  Clear the environment.

rm(list = ls())

#  Get data. 

#   Get acres and yield of (observed) standing timber. This is poorly implemented.

dat_standing = 
  "03_intermediate/dat_notifications_1_9.csv" %>% 
  read_csv %>% 
  group_by(Landowner, QuarterCompletion) %>% 
  summarize(MBF_Standing = sum(MBF_2_DouglasFir) + sum(MBF_2_WesternHemlock),
            Acres_Standing = Acres_1 %>% sum) %>% 
  ungroup %>% 
  arrange(Landowner, desc(QuarterCompletion)) %>% 
  group_by(Landowner) %>% 
  mutate(MBF_Standing_Forward = cumsum(MBF_Standing) - MBF_Standing,
         Acres_Standing_Forward = cumsum(Acres_Standing) - Acres_Standing) %>% 
  ungroup %>% 
  select(Landowner, 
         QuarterCompletion, 
         MBF_Standing_Forward, 
         Acres_Standing_Forward)


fun_standing = 
  function(lastquarter, thisquarter){
    
    ifelse((is.na(lastquarter) | (lastquarter == 0)) & is.na(thisquarter), 
           0, 
           ifelse(!is.na(lastquarter) & is.na(thisquarter), 
                  lastquarter,
                  thisquarter))
    
    }

dat_standing_explicit =
  dat_standing %>%
  select(Landowner, QuarterCompletion) %>%
  complete(Landowner, QuarterCompletion) %>%
  left_join(dat_standing) %>%
  arrange(Landowner, desc(QuarterCompletion)) %>%
  group_by(Landowner) %>%
  mutate(MBF_Standing_Forward = accumulate(MBF_Standing_Forward, ~ fun_standing(.x, .y)) %>% replace_na(0),
         Acres_Standing_Forward = accumulate(Acres_Standing_Forward, ~ fun_standing(.x, .y)) %>% replace_na(0)) %>% 
  ungroup %>% 
  select(Landowner, QuarterCompletion, MBF_Standing_Forward, Acres_Standing_Forward)

#  Get firm decisions with implicit panel structure. 

dat_implicit = 
  "03_intermediate/dat_notifications_1_9.csv" %>% 
  read_csv %>% 
  # Handle pyromes and counties. Assign firms to their modal pyrome or county. Break ties on alphabetical order.
  group_by(Landowner, Pyrome) %>% 
  mutate(Pyrome_Count = n()) %>% 
  group_by(Landowner, County) %>% 
  mutate(County_Count = n()) %>%
  group_by(Landowner) %>% 
  mutate(Pyrome = ifelse(Pyrome_Count == max(Pyrome_Count), Pyrome, NA),
         County = ifelse(County_Count == max(County_Count), County, NA),
         Pyrome = Pyrome %>% max(na.rm = TRUE),
         County = County %>% max(na.rm = TRUE)) %>% 
  ungroup %>% 
  select(-Pyrome_Count, -County_Count) %>% 
  # Handle continuous variables. Assign production-weighted means by firm. 
  rename(MBF_DouglasFir = MBF_2_DouglasFir,
         MBF_WesternHemlock = MBF_2_WesternHemlock,
         Acres = Acres_1) %>% 
  mutate(MBF_Both = MBF_DouglasFir + MBF_WesternHemlock) %>% 
  group_by(Landowner, QuarterCompletion, Pyrome, County) %>% 
  # Note that sums follow means to avoid quiet failure on weighting by a sum. 
  summarize(Count = n(),
            across(
              c(SiteClassMode,
                Elevation,
                Slope,
                Roughness,
                starts_with("Distance"),
                starts_with("Stumpage"),
                starts_with("Lumber"),
                starts_with("Rate"),
                starts_with("Fire"),
                starts_with("VPD")),
              ~ weighted.mean(.x, na.rm = TRUE, w = MBF_Both)),
            across(
              c(MBF_DouglasFir,
                MBF_WesternHemlock,
                MBF_Both,
                Acres),
              ~ sum(.x, na.rm = TRUE))) %>%
  ungroup %>% 
  # Sneak in a quarter variable.
  mutate(Quarter = QuarterCompletion %>% str_split_i("_", 2) %>% as.numeric) %>% 
  relocate(Quarter, .after = "QuarterCompletion")

dat_implicit_mean = 
  dat_implicit %>% 
  # Drop non-lagged variables so that selecting lags is easier. 
  select(-Lumber,
         -Stumpage,
         -Rate,
         -ends_with("Lag_0"), # Fire
         -VPD) %>% 
  # Standardize names between fire and other variables. 
  rename_with(~ gsub("^Fire_(\\d+)_Doughnut_Lag_(\\d+)$", "Fire\\1Doughnut_Lag_\\2", .x),
              matches("^Fire_\\d+_Doughnut_Lag_\\d+$")) %>% 
  rename_with(~ gsub("^Fire_(\\d+)_Lag_(\\d+)$", "Fire\\1_Lag_\\2", .x),
              matches("^Fire_\\d+_Lag_\\d+$")) %>% 
  # Pivot. 
  pivot_longer(starts_with(c("Lumber", "Stumpage", "Rate", "Fire", "VPD")),
               names_sep = "_",
               names_to = c("Which", "Label", "Lag"),
               values_to = "Value") %>% 
  # Get means. 
  mutate(Mean_4 = ifelse(Lag < 5, Value, NA),
         Mean_8 = ifelse(Lag < 9, Value, NA),
         Mean_12 = ifelse(Lag < 13, Value, NA),
         Mean_16 = ifelse(Lag < 17, Value, NA),
         Mean_20 = Value) %>% 
  # Pivot back. 
  select(-Value) %>% 
  group_by(Landowner, QuarterCompletion, Which) %>% 
  summarize(across(starts_with("Mean"), ~ mean(.x, na.rm = TRUE))) %>% 
  ungroup %>% 
  pivot_wider(names_from = "Which",
              values_from = starts_with("Mean"),
              names_glue = "{Which}_{.value}")

dat_implicit_out = 
  dat_implicit %>% 
  left_join(dat_implicit_mean) %>% 
  left_join(dat_standing) %>% 
  select(Landowner,
         Pyrome,
         County,
         QuarterCompletion,
         Quarter,
         Count,
         MBF_DouglasFir,
         MBF_WesternHemlock,
         MBF_Standing_Forward,
         Acres, 
         Acres_Standing_Forward,
         SiteClassMode,
         Elevation,
         Slope,
         Roughness,
         starts_with("Distance"),
         starts_with("Stumpage"),
         starts_with("Lumber"),
         starts_with("Rate"),
         starts_with("VPD"),
         starts_with("Fire")) %T>% 
  # Export.
  write_csv("03_intermediate/dat_firms_implicit_3_1.csv")
  
#  Get firm decisions with explicit panel structure. This is also poorly implemented (covariates in nonharvest years).

dat_explicit = 
  dat_implicit_out %>% 
  select(Landowner, QuarterCompletion) %>%
  complete(Landowner, QuarterCompletion) %>% 
  left_join(dat_implicit_out %>% 
              select(Landowner,
                     Pyrome,
                     County) %>% 
              distinct) %>% 
  left_join(dat_standing_explicit) %>% 
  left_join(dat_implicit_out %>% 
              select(-c(Landowner, Pyrome, County, Count, starts_with("MBF"), starts_with("Acres"), Quarter)) %>% 
              group_by(QuarterCompletion) %>% 
              summarize(across(everything(), ~ mean(.x, na.rm = TRUE)))) %>% 
  anti_join(dat_implicit_out, by = c("Landowner", "QuarterCompletion")) %>% 
  bind_rows(dat_implicit_out, .) %>% 
  arrange(Landowner, QuarterCompletion) %>% 
  mutate(Quarter = QuarterCompletion %>% str_split_i("_", 2) %>% as.numeric,
         across(c("Count", starts_with(c("MBF", "Acres"))), ~ replace_na(.x, 0))) %T>% 
  # Export.
  write_csv("03_intermediate/dat_firms_explicit_3_1.csv")
