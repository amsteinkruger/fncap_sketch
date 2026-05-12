# Aggregate over the panel of notifications by firm-year-quarter.

#  Clear the environment.

rm(list = ls())

#  Get data. 

dat_implicit = 
  "03_intermediate/dat_notifications_1_8.csv" %>% 
  read_csv %>% 
  # Handle percentile restrictions. 
  mutate(Restrict_MBF_Lower = MBF_2_DouglasFir > quantile(MBF_2_DouglasFir, 0.01),
         Restrict_MBF_Upper = MBF_2_DouglasFir < quantile(MBF_2_DouglasFir, 0.99),
         Restrict_Acres_Lower = Acres_1 > quantile(Acres_1, 0.01),
         Restrict_Acres_Upper = Acres_1 < quantile(Acres_1, 0.99),
         Restrict_MBFAcre_Lower = MBF_Acre_2_DouglasFir > quantile(MBF_Acre_2_DouglasFir, 0.01),
         Restrict_MBFAcre_Upper = MBF_Acre_2_DouglasFir < quantile(MBF_Acre_2_DouglasFir, 0.99)) %>% 
  filter(if_all(starts_with("Restrict"), ~ .x == TRUE)) %>% 
  # Handle pyromes and counties. Assign firms to their modal pyrome or county. Break ties on alphabetical order.
  group_by(Landowner, Pyrome) %>% 
  mutate(Pyrome_Count = n(),
         County_Count = n()) %>%
  group_by(Landowner) %>% 
  mutate(Pyrome = ifelse(Pyrome_Count == max(Pyrome_Count), Pyrome, NA),
         County = ifelse(County_Count == max(County_Count), County, NA),
         Pyrome = Pyrome %>% max(na.rm = TRUE),
         County = County %>% max(na.rm = TRUE)) %>% 
  # Handle continuous variables. Assign production-weighted means by firm. 
  group_by(Landowner, QuarterCompletion, Pyrome, County) %>% 
  summarize(MBF_Firm = MBF_2_DouglasFir %>% sum(na.rm = TRUE),
            Acres = Acres_1 %>% sum(na.rm = TRUE),
            across(starts_with("Lumber"), 
                   ~ weighted.mean(.x, na.rm = TRUE, w = MBF_2_DouglasFir)),
            across(starts_with("Stumpage"), 
                   ~ weighted.mean(.x, na.rm = TRUE, w = MBF_2_DouglasFir)),
            across(starts_with("Fire"), 
                   ~ weighted.mean(.x, na.rm = TRUE, w = MBF_2_DouglasFir)),
            across(starts_with("VPD"), 
                   ~ weighted.mean(.x, na.rm = TRUE, w = MBF_2_DouglasFir)),
            across(starts_with("Rate"), 
                   ~ weighted.mean(.x, na.rm = TRUE, w = MBF_2_DouglasFir))) %>% 
  ungroup %>% 
  # Sneak in a quarter variable.
  mutate(Quarter = QuarterCompletion %>% str_split_i("_", 2) %>% as.numeric) %>% 
  relocate(Quarter, .after = "QuarterCompletion")

dat_implicit_mean = 
  dat_implicit %>% 
  select(-MBF_Firm, 
         -Acres,
         -starts_with("Fire"),
         -Lumber,
         -Stumpage,
         -Rate,
         -VPD) %>% 
  pivot_longer(starts_with(c("Lumber", "Stumpage", "Rate", "VPD")),
               names_sep = "_",
               names_to = c("Which", "Label", "Lag"),
               values_to = "Value") %>% 
  mutate(Mean_4 = ifelse(Lag < 5, Value, NA),
         Mean_8 = ifelse(Lag < 9, Value, NA),
         Mean_12 = ifelse(Lag < 13, Value, NA),
         Mean_16 = ifelse(Lag < 17, Value, NA),
         Mean_20 = Value) %>% 
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
  select(Landowner,
         Pyrome,
         County,
         QuarterCompletion,
         Quarter,
         MBF = MBF_Firm,
         Acres, 
         starts_with("Stumpage"),
         starts_with("Lumber"),
         starts_with("Rate"),
         starts_with("VPD"),
         starts_with("Fire")) %T>% 
  # Export.
  write_csv("03_intermediate/dat_firms_implicit_3_1.csv")

# Complete the data with implicit zero-production decisions. 

dat_explicit = 
  dat_implicit_out %>% 
  select(Landowner, QuarterCompletion) %>%
  complete(Landowner, QuarterCompletion) %>% 
  left_join(dat_implicit_out %>% 
              select(Landowner,
                     Pyrome,
                     County) %>% 
              distinct) %>% 
  left_join(dat_implicit_out %>% 
              select(-c(Landowner, Pyrome, County, MBF, Acres, Quarter)) %>% 
              group_by(QuarterCompletion) %>% 
              summarize(across(everything(), ~ mean(.x, na.rm = TRUE)))) %>% 
  anti_join(dat_implicit_out, by = c("Landowner", "QuarterCompletion")) %>% 
  bind_rows(dat_implicit_out, .) %>% 
  arrange(Landowner, QuarterCompletion) %>% 
  mutate(Quarter = QuarterCompletion %>% str_split_i("_", 2) %>% as.numeric,
         across(c(MBF, Acres), ~ replace_na(.x, 0))) %T>% 
  # Export.
  write_csv("03_intermediate/dat_firms_explicit_3_1.csv")
