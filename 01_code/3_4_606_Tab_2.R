# Estimate something for 2YP.

#  Get data, then reduce to the descriptive staistics of interest. 

dat = 
  "03_intermediate/dat_notifications_1_8.csv" %>% 
  read_csv %>% 
  mutate(Restrict_MBF_Lower = MBF_2_DouglasFir > quantile(MBF_2_DouglasFir, 0.01),
         Restrict_MBF_Upper = MBF_2_DouglasFir < quantile(MBF_2_DouglasFir, 0.99),
         Restrict_Acres_Lower = Acres_1 > quantile(Acres_1, 0.01),
         Restrict_Acres_Upper = Acres_1 < quantile(Acres_1, 0.99),
         Restrict_MBFAcre_Lower = MBF_Acre_2_DouglasFir > quantile(MBF_Acre_2_DouglasFir, 0.01),
         Restrict_MBFAcre_Upper = MBF_Acre_2_DouglasFir < quantile(MBF_Acre_2_DouglasFir, 0.99)) %>% 
  filter(if_all(starts_with("Restrict"), ~ .x == TRUE)) # %>% 
  # Aggregate as in model estimation. 
  group_by(Landowner, QuarterCompletion) %>% 
  summarize(Count = n(),
            MBF_Firm = MBF_2_DouglasFir %>% sum(na.rm = TRUE),
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
  rename(MBF = MBF_Firm) %>% 
  # Aggregate for tabulation. 
  summarize(Count_Mean = mean(Count),
            Count_SD = sd(Count),
            MBF_Mean = mean(MBF),
            MBF_SD = sd(MBF),
            Acres_Mean = mean(Acres),
            Acres_SD = sd(Acres),
            Lumber_Mean = mean(Lumber),
            Lumber_SD = sd(Lumber),
            Stumpage_Mean = mean(Stumpage),
            Stumpage_SD = sd(Stumpage),
            Rate_Mean = mean(Rate),
            Rate_SD = sd(Rate),
            Fire_0_Mean = mean(Fire_0),
            Fire_0_SD = sd(Fire_0),
            Fire_15_Mean = mean(Fire_15_Doughnut),
            Fire_15_SD = sd(Fire_15_Doughnut),
            Fire_30_Mean = mean(Fire_30_Doughnut),
            Fire_30_SD = sd(Fire_30_Doughnut),
            Fire_Proportion_Mean = mean(Fire_Proportion),
            Fire_Proportion_SD = sd(Fire_Proportion),
            VPD_Mean = mean(VPD),
            VPD_SD = sd(VPD)) %>% 
  pivot_longer(everything()) %>% 
  mutate(Statistic = ifelse(str_sub(name, -4, -1) == "Mean", "Mean", "SD")) %>% 
  mutate(name = name %>% str_remove_all("_Mean") %>% str_remove_all("_SD")) %>% 
  pivot_wider(values_from = value,
              names_from = Statistic) %T>% 
  # Export.
  write_csv("04_out/tab_2.csv")
