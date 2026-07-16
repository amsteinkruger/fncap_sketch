# Add final touches for data quality and coherence. 

dat_clean = 
  "03_intermediate/dat_notifications_1_8.gdb" %>% 
  vect %>% 
  # Handle percentile restrictions. 
  mutate(Restrict_Acres_Lower = Acres_1 > quantile(Acres_1, 0.01),
         Restrict_Acres_Upper = Acres_1 < quantile(Acres_1, 0.99),
         Restrict_MBF_Lower = MBF_1 > quantile(MBF_1, 0.01),
         Restrict_MBF_Upper = MBF_1 < quantile(MBF_1, 0.99),
         Restrict_MBF_Acre_Lower = MBF_1 > quantile(MBF_Acre_1, 0.01),
         Restrict_MBF_Acre_Upper = MBF_1 < quantile(MBF_Acre_1, 0.99)) %>% 
  filter(if_all(starts_with("Restrict"), ~ .x == TRUE)) %>% 
  select(-starts_with("Restrict")) %>% 
  # Move variables around for easier reading. 
  select(-starts_with("Date"), -Completion) %>% 
  rename(YearCompletion = Year) %>% 
  relocate(ends_with("Completion"), .after = Landowner) %T>%
  # Export with spatial data.
  writeVector("03_intermediate/dat_notifications_1_9.gdb") %>% 
  # Export without spatial data.
  as_tibble %T>% 
  write_csv("03_intermediate/dat_notifications_1_9.csv")
