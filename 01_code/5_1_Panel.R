# Aggregate over the panel of notifications by firm-year-quarter.

#  Clear the environment.

rm(list = ls())

#  Get data. 

#   Get acres and yield of (observed) standing timber. This is poorly implemented.

dat_standing = 
  "03_intermediate/dat_notifications_1_9.csv" %>% 
  read_csv %>% 
  mutate(Landowner = Owner_Cotality_Frequent) %>% # Patch for modifications to land ownership workflow. 
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
  mutate(Landowner = Owner_Cotality_Frequent) %>% # Patch for modifications to land ownership workflow. 
  group_by(Landowner) %>% 
  mutate(Landowner_ID = cur_group_id()) %>% 
  ungroup %>% 
  relocate(Landowner_ID, .after = "UID") %>% 
  # Handle pyromes and counties. Assign firms to their modal pyrome or county. Break ties on alphabetical order.
  group_by(Landowner, Pyrome) %>% 
  mutate(Pyrome_Count = n()) %>% 
  group_by(Landowner, County) %>% 
  mutate(County_Count = n()) %>%
  group_by(Landowner, District) %>% 
  mutate(District_Count = n()) %>%
  group_by(Landowner) %>% 
  mutate(Pyrome = ifelse(Pyrome_Count == max(Pyrome_Count), Pyrome, NA),
         County = ifelse(County_Count == max(County_Count), County, NA),
         District = ifelse(District_Count == max(District_Count), District, NA),
         Pyrome = Pyrome %>% max(na.rm = TRUE),
         County = County %>% max(na.rm = TRUE),
         District = District %>% max(na.rm = TRUE)) %>% 
  ungroup %>% 
  select(-Pyrome_Count, -County_Count, -District_Count) %>% 
  # Handle continuous variables. Assign production-weighted means by firm. 
  rename(MBF_DouglasFir = MBF_2_DouglasFir,
         MBF_WesternHemlock = MBF_2_WesternHemlock,
         Acres = Acres_1) %>% 
  mutate(MBF_Both = MBF_DouglasFir + MBF_WesternHemlock) %>% 
  group_by(Landowner, Landowner_ID, Owner_Acres, QuarterCompletion, Pyrome, County, District) %>% 
  # Note that sums follow means to avoid quiet failure on weighting by a sum. 
  summarize(Count = n(),
            across(
              c(SiteClassMode,
                Elevation,
                Slope,
                Roughness,
                starts_with("Distance"),
                starts_with("Price_Stumpage"),
                starts_with("Price_Lumber"),
                starts_with("Rate"),
                starts_with("Fire"),
                starts_with("VPD"),
                starts_with("CWD")),
              ~ weighted.mean(.x, na.rm = TRUE, w = MBF_Both)),
            across(
              c(MBF_DouglasFir,
                MBF_WesternHemlock,
                MBF_Both,
                Acres),
              ~ sum(.x, na.rm = TRUE))) %>%
  ungroup %>% 
  # Sneak in quarter and year variables.
  mutate(Year = QuarterCompletion %>% str_split_i("_", 1) %>% as.numeric,
         Quarter = QuarterCompletion %>% str_split_i("_", 2) %>% as.numeric) %>% 
  relocate(c(Year, Quarter), .after = "QuarterCompletion")

dat_implicit_mean =
  dat_implicit %>% 
  # Price_Stumpage_DouglasFir
  mutate(
    Price_Stumpage_DouglasFir_Mean = 
      rowMeans(
        pick(
          starts_with("Price_Stumpage_DouglasFir_Lag_") & ends_with(as.character(21:32))),
        na.rm = TRUE
      )
  ) %>% 
  # Rate
  mutate(
    Rate_Mean = 
      rowMeans(
        pick(
          starts_with("Rate_Lag_") & ends_with(as.character(1:8))),
        na.rm = TRUE
      )
  ) %>% 
  # CWD
  mutate(
    CWD_Mean = 
      rowMeans(
        pick(
          starts_with("CWD_Lag_") & ends_with(as.character(1:24))),
        na.rm = TRUE
      )
  ) %>% 
  # Clean up.
  select(Landowner, QuarterCompletion, ends_with("Mean"))

# The commented-out code preserves code to compute multiannual means. 

# dat_implicit_mean = 
#   dat_implicit %>% 
#   # Drop non-lagged variables so that selecting lags is easier. 
#   select(-Lumber,
#          -Stumpage,
#          -Rate,
#          -ends_with("Lag_0"), # Fire
#          -VPD) %>% 
#   # Standardize names between fire and other variables. 
#   rename_with(~ gsub("^Fire_(\\d+)_Doughnut_Lag_(\\d+)$", "Fire\\1Doughnut_Lag_\\2", .x),
#               matches("^Fire_\\d+_Doughnut_Lag_\\d+$")) %>% 
#   rename_with(~ gsub("^Fire_(\\d+)_Lag_(\\d+)$", "Fire\\1_Lag_\\2", .x),
#               matches("^Fire_\\d+_Lag_\\d+$")) %>% 
#   # Pivot. 
#   pivot_longer(starts_with(c("Lumber", "Stumpage", "Rate", "Fire", "VPD")),
#                names_sep = "_",
#                names_to = c("Which", "Label", "Lag"),
#                values_to = "Value") %>% 
#   # Get means. 
#   mutate(Mean_4 = ifelse(Lag < 5, Value, NA),
#          Mean_8 = ifelse(Lag < 9, Value, NA),
#          Mean_12 = ifelse(Lag < 13, Value, NA),
#          Mean_16 = ifelse(Lag < 17, Value, NA),
#          Mean_20 = Value) %>% 
#   # Pivot back. 
#   select(-Value) %>% 
#   group_by(Landowner, QuarterCompletion, Which) %>% 
#   summarize(across(starts_with("Mean"), ~ mean(.x, na.rm = TRUE))) %>% 
#   ungroup %>% 
#   pivot_wider(names_from = "Which",
#               values_from = starts_with("Mean"),
#               names_glue = "{Which}_{.value}")

dat_implicit_out = 
  dat_implicit %>% 
  left_join(dat_implicit_mean) %>% 
  left_join(dat_standing) %>% 
  select(
    Landowner,
    Landowner_ID, 
    Owner_Acres,
    Pyrome,
    County,
    District,
    QuarterCompletion,
    Year,
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
    Fire_0 = Fire_0_Lag_0,
    Fire_15 = Fire_15_Lag_0,
    Fire_15_Doughnut = Fire_15_Doughnut_Lag_0,
    Fire_30 = Fire_30_Lag_0,
    Fire_30_Doughnut = Fire_30_Doughnut_Lag_0,
    ends_with("Mean")
  ) %T>% 
  # Export.
  write_csv("03_intermediate/dat_firms_implicit_3_1.csv")
  
#  Get firm decisions with explicit panel structure. This is also poorly implemented (covariates in nonharvest years).

dat_explicit = 
  dat_implicit_out %>% 
  select(Landowner, QuarterCompletion) %>%
  complete(Landowner, QuarterCompletion) %>% 
  left_join(
    dat_implicit_out %>% 
      select(
        Landowner,
        Landowner_ID, 
        Pyrome,
        County,
        District
      ) %>% 
      distinct
  ) %>% 
  left_join(dat_standing_explicit) %>% 
  left_join(
    "03_intermediate/dat_owners_join.csv" %>% read_csv %>% select(-Owner_FERNS),
    by = c("Landowner" = "Owner_Cotality_Frequent", "QuarterCompletion" = "Year_Quarter")
  ) %>% 
  left_join(
    dat_implicit_out %>% 
      select(
        -c(
          Landowner, 
          Landowner_ID,
          Owner_Acres,
          Pyrome, 
          County,
          District,
          Count, 
          starts_with("MBF"), 
          starts_with("Acres"), 
          Quarter
        )
      ) %>% 
      group_by(QuarterCompletion) %>% 
      summarize(across(everything(), ~ mean(.x, na.rm = TRUE))) %>% 
      ungroup
  ) %>% 
  anti_join(dat_implicit_out, by = c("Landowner", "QuarterCompletion")) %>% 
  bind_rows(dat_implicit_out, .) %>% 
  arrange(Landowner, QuarterCompletion) %>% 
  mutate(
    Quarter = QuarterCompletion %>% str_split_i("_", 2) %>% as.numeric,
    across(c("Count", starts_with(c("MBF", "Acres"))), ~ replace_na(.x, 0))
  ) %T>% 
  # Export.
  write_csv("03_intermediate/dat_firms_explicit_3_1.csv")
