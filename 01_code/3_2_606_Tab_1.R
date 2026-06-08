# Estimate something for 2YP.

#  Get data, then reduce to the descriptive statistics of interest. 

dat = 
  "03_intermediate/dat_firms_implicit_3_1.csv" %>% 
  read_csv %>% 
  # Aggregate for tabulation. 
  summarize(Count_Mean = mean(Count),
            Count_SD = sd(Count),
            MBF_DouglasFir_Mean = mean(MBF_DouglasFir),
            MBF_DouglasFir_SD = sd(MBF_DouglasFir),
            MBF_WesternHemlock_Mean = mean(MBF_WesternHemlock),
            MBF_WesternHemlock_SD = sd(MBF_WesternHemlock),
            Acres_Mean = mean(Acres),
            Acres_SD = sd(Acres),
            Lumber_Mean = mean(Lumber),
            Lumber_SD = sd(Lumber),
            Stumpage_Mean = mean(Stumpage),
            Stumpage_SD = sd(Stumpage),
            Rate_Mean = mean(Rate),
            Rate_SD = sd(Rate),
            Fire_0_Mean = mean(Fire_0_Lag_0),
            Fire_0_SD = sd(Fire_0_Lag_0),
            Fire_15_Mean = mean(Fire_15_Doughnut_Lag_0),
            Fire_15_SD = sd(Fire_15_Doughnut_Lag_0),
            Fire_30_Mean = mean(Fire_30_Doughnut_Lag_0),
            Fire_30_SD = sd(Fire_30_Doughnut_Lag_0),
            # Fire_Proportion_Mean = mean(Fire_Proportion_Lag_0),
            # Fire_Proportion_SD = sd(Fire_Proportion_Lag_0),
            VPD_Mean = mean(VPD),
            VPD_SD = sd(VPD)) %>% 
  pivot_longer(everything()) %>% 
  mutate(Statistic = ifelse(str_sub(name, -4, -1) == "Mean", "Mean", "SD")) %>% 
  mutate(name = name %>% str_remove_all("_Mean") %>% str_remove_all("_SD")) %>% 
  pivot_wider(values_from = value,
              names_from = Statistic) %T>% 
  # Export.
  write_csv("04_out/tab_2.csv")
