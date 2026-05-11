# Aggregate over the panel of notifications by firm-year-quarter.

#  Clear the environment.

rm(list = ls())

#  Get data. 

dat_implicit = 
  "03_intermediate/dat_notifications_1_8.csv" %>% 
  read_csv %>% 
  mutate(Restrict_MBF_Lower = MBF_2_DouglasFir > quantile(MBF_2_DouglasFir, 0.01),
         Restrict_MBF_Upper = MBF_2_DouglasFir < quantile(MBF_2_DouglasFir, 0.99),
         Restrict_Acres_Lower = Acres_1 > quantile(Acres_1, 0.01),
         Restrict_Acres_Upper = Acres_1 < quantile(Acres_1, 0.99),
         Restrict_MBFAcre_Lower = MBF_Acre_2_DouglasFir > quantile(MBF_Acre_2_DouglasFir, 0.01),
         Restrict_MBFAcre_Upper = MBF_Acre_2_DouglasFir < quantile(MBF_Acre_2_DouglasFir, 0.99)) %>% 
  filter(if_all(starts_with("Restrict"), ~ .x == TRUE)) %>% 
  group_by(Landowner, QuarterCompletion) %>% 
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
  mutate(Quarter = QuarterCompletion %>% str_split_i("_", 2) %>% as.numeric) %>% 
  rename(MBF = MBF_Firm) %T>% 
  # Export.
  write_csv("03_intermediate/dat_firms_implicit_3_1.csv")

# Complete the data with implicit zero-production decisions. 

dat_explicit = 
  dat_implicit %>% 
  select(Landowner, QuarterCompletion) %>%
  complete(Landowner, QuarterCompletion) %>% 
  left_join(dat_implicit %>% 
              select(-c(Landowner, MBF, Acres, Quarter)) %>% 
              group_by(QuarterCompletion) %>% 
              summarize(across(everything(), ~ mean(.x, na.rm = TRUE)))) %>% 
  anti_join(dat_implicit, by = c("Landowner", "QuarterCompletion")) %>% 
  bind_rows(dat_implicit, .) %>% 
  arrange(Landowner, QuarterCompletion) %>% 
  mutate(Quarter = QuarterCompletion %>% str_split_i("_", 2) %>% as.numeric,
         across(c(MBF, Acres), ~ replace_na(.x, 0))) %T>% 
  # Export.
  write_csv("03_intermediate/dat_firms_explicit_3_1.csv")
