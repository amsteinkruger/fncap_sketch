# Add final touches for data quality and coherence. 

dat_clean = 
  "03_intermediate/dat_notifications_1_8.gdb" %>% 
  vect %>% 
  # Reduce to quarters of completion.
  filter(Year_Quarter == QuarterCompletion) %>% 
  # Handle percentile restrictions. 
  mutate(Restrict_Acres_Lower = Acres_1 > quantile(Acres_1, 0.01),
         Restrict_Acres_Upper = Acres_1 < quantile(Acres_1, 0.99),
         Restrict_MBF_Lower = MBF_1 > quantile(MBF_1, 0.01),
         Restrict_MBF_Upper = MBF_1 < quantile(MBF_1, 0.99),
         Restrict_MBF_DouglasFir_Lower = MBF_2_DouglasFir > quantile(MBF_2_DouglasFir, 0.01),
         Restrict_MBF_DouglasFir_Upper = MBF_2_DouglasFir < quantile(MBF_2_DouglasFir, 0.99),
         Restrict_MBF_WesternHemlock_Lower = MBF_2_WesternHemlock > quantile(MBF_2_WesternHemlock, 0.01),
         Restrict_MBF_WesternHemlock_Upper = MBF_2_WesternHemlock < quantile(MBF_2_WesternHemlock, 0.99),
         Restrict_MBFAcre_DouglasFir_Lower = MBF_Acre_2_DouglasFir > quantile(MBF_Acre_2_DouglasFir, 0.01),
         Restrict_MBFAcre_DouglasFir_Upper = MBF_Acre_2_DouglasFir < quantile(MBF_Acre_2_DouglasFir, 0.99),
         Restrict_MBFAcre_WesternHemlock_Lower = MBF_Acre_2_WesternHemlock > quantile(MBF_Acre_2_WesternHemlock, 0.01),
         Restrict_MBFAcre_WesternHemlock_Upper = MBF_Acre_2_WesternHemlock < quantile(MBF_Acre_2_WesternHemlock, 0.99)) %>% 
  filter(if_all(starts_with("Restrict"), ~ .x == TRUE)) %>% 
  select(-starts_with("Restrict")) %>% 
  # Move variables around for easier reading. 
  relocate(QuarterCompletion, .after = DateCompletion) %T>%
  # Export with spatial data.
  writeVector("03_intermediate/dat_notifications_1_9.gdb") %>% 
  # Export without spatial data.
  as_tibble %T>% 
  write_csv("03_intermediate/dat_notifications_1_9.csv")
