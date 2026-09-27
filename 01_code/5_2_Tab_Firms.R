# Summarize variables of interest by firm. 

dat = 
  "03_intermediate/dat_firms_implicit_3_1.csv" %>% 
  read_csv %>% 
  # Clean up some names.
  rename_with(~ str_remove_all(.x, "_Mean"), ends_with("_Mean")) %>% 
  # Patch in SFO.
  mutate(SFO = ifelse(Landowner_ID %in% vec_small, 1, 0)) %>% 
  # Aggregate for tabulation. 
  summarize(Count_Mean = mean(Count),
            Count_SD = sd(Count),
            Owner_Acres_Mean = mean(Owner_Acres, na.rm = TRUE),
            Owner_Acres_SD = sd(Owner_Acres, na.rm = TRUE),
            SFO_Mean = mean(SFO),
            SFO_SD = sd(SFO),
            MBF_DouglasFir_Mean = mean(MBF_DouglasFir),
            MBF_DouglasFir_SD = sd(MBF_DouglasFir),
            MBF_WesternHemlock_Mean = mean(MBF_WesternHemlock),
            MBF_WesternHemlock_SD = sd(MBF_WesternHemlock),
            Acres_Mean = mean(Acres),
            Acres_SD = sd(Acres),
            Stumpage_Mean = mean(Price_Stumpage_DouglasFir),
            Stumpage_SD = sd(Price_Stumpage_DouglasFir),
            Rate_Mean = mean(Rate),
            Rate_SD = sd(Rate),
            Fire_30_Mean = mean(Fire_30),
            Fire_30_SD = sd(Fire_30),
            CWD_Mean = mean(CWD),
            CWD_SD = sd(CWD)) %>% 
  pivot_longer(everything()) %>% 
  mutate(Statistic = ifelse(str_sub(name, -4, -1) == "Mean", "Mean", "SD")) %>% 
  mutate(name = name %>% str_remove_all("_Mean") %>% str_remove_all("_SD")) %>% 
  pivot_wider(values_from = value,
              names_from = Statistic) %T>% 
  # Export.
  write_csv("04_out/Paper_FirmSupply/tab_firms.csv")
