# Summarize variables of interest by notification.  

dat = 
  "03_intermediate/dat_notifications_1_9.csv" %>% 
  read_csv %>% 
  # Obtain means over lags.
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
  # Aggregate for tabulation. 
  summarize(MBF_DouglasFir_Mean = mean(MBF_2_DouglasFir),
            MBF_DouglasFir_SD = sd(MBF_2_DouglasFir),
            MBF_WesternHemlock_Mean = mean(MBF_2_WesternHemlock),
            MBF_WesternHemlock_SD = sd(MBF_2_WesternHemlock),
            Acres_Mean = mean(Acres_1),
            Acres_SD = sd(Acres_1),
            Stumpage_Mean = mean(Price_Stumpage_DouglasFir_Mean),
            Stumpage_SD = sd(Price_Stumpage_DouglasFir_Mean),
            Rate_Mean_Mean = mean(Rate_Mean),
            Rate_SD = sd(Rate_Mean),
            Fire_30_Mean = mean(Fire_30_Lag_0),
            Fire_30_SD = sd(Fire_30_Lag_0),
            CWD_Mean_Mean = mean(CWD),
            CWD_Mean_SD = sd(CWD),
            SiteClassMode_Mean = mean(SiteClassMode), 
            SiteClassMode_SD = sd(SiteClassMode),
            Elevation_Mean = mean(Elevation), 
            Elevation_SD = sd(Elevation), 
            Slope_Mean = mean(Slope), 
            Slope_SD = sd(Slope), 
            Distance_Place_Mean = mean(Distance_Place), 
            Distance_Place_SD = sd(Distance_Place)) %>% 
  rename(Rate_Mean = Rate_Mean_Mean, CWD_Mean = CWD_Mean_Mean) %>% 
  pivot_longer(everything()) %>% 
  mutate(Statistic = ifelse(str_sub(name, -4, -1) == "Mean", "Mean", "SD")) %>% 
  mutate(name = name %>% str_remove_all("_Mean") %>% str_remove_all("_SD")) %>% 
  pivot_wider(values_from = value,
              names_from = Statistic) %T>% 
  # Export.
  write_csv("04_out/Paper_FirmSupply/tab_notifications.csv")
