# Estimate simple linear models of MBF/Acre on a kitchen sink of variables.

dat = 
  "03_intermediate/dat_notifications_1_9.csv" %>% 
  read_csv %>% 
  # Get a species-agnostic yield variable.
  mutate(MBF_Acre_Both = MBF_Acre_2_DouglasFir + MBF_Acre_2_WesternHemlock) %>% 
  # Get landowner percentiles by total production of both species.
  group_by(Landowner) %>% 
  mutate(MBF_Both_Total = sum(MBF_2_DouglasFir) + sum(MBF_2_WesternHemlock)) %>% 
  ungroup %>% 
  mutate(Landowner_MBF_Percentile = ntile(MBF_Both_Total, 100)) %>% 
  # Get factors for some categorical variables.
  mutate(Activity = Activity %>% factor,
         Pyrome = Pyrome %>% factor,
         County = County %>% factor,
         District = District %>% factor)

mod_1 = 
  dat %>% 
  feols(MBF_Acre_Both ~ 
          # Landowner-varying
          Landowner_MBF_Percentile +
          # Notification-varying?
          Activity +
          # Space-varying
          SiteClassMode +
          Elevation +
          Slope + 
          # Roughness +
          Distance_Road +
          Distance_Mill +
          Distance_Place +
          ProportionDouglasFirTree +
          # Time-varying
          Stumpage_Lag_1 +
          Lumber_Lag_1 +
          Rate_Lag_1 +
          # Time- and space-varying
          Fire_0_Lag_1 +
          Fire_15_Doughnut_Lag_1 +
          Fire_30_Doughnut_Lag_1 +
          VPD_Lag_1)

mod_2 = 
  dat %>% 
  feols(MBF_Acre_Both ~ 
          # Landowner-varying
          Landowner_MBF_Percentile +
          # Notification-varying?
          Activity +
          # Space-varying
          SiteClassMode +
          Elevation +
          Slope + 
          # Roughness +
          Distance_Road +
          Distance_Mill +
          Distance_Place +
          ProportionDouglasFirTree +
          # Time-varying
          Stumpage_Lag_1 +
          Lumber_Lag_1 +
          Rate_Lag_1 +
          # Time- and space-varying
          Fire_0_Lag_1 +
          Fire_15_Doughnut_Lag_1 +
          Fire_30_Doughnut_Lag_1 +
          VPD_Lag_1 |
          County)


mod_3 = 
  dat %>% 
  feols(MBF_Acre_Both ~ 
          # Landowner-varying
          Landowner_MBF_Percentile +
          # Notification-varying?
          Activity +
          # Space-varying
          SiteClassMode +
          Elevation +
          Slope + 
          # Roughness +
          Distance_Road +
          Distance_Mill +
          Distance_Place +
          ProportionDouglasFirTree +
          # Time-varying
          Stumpage_Lag_1 +
          Lumber_Lag_1 +
          Rate_Lag_1 +
          # Time- and space-varying
          Fire_0_Lag_1 +
          Fire_15_Doughnut_Lag_1 +
          Fire_30_Doughnut_Lag_1 +
          VPD_Lag_1 |
          District)

modelsummary(
  list("No FE" = mod_1,
       "County FE" = mod_2,
       "District FE" = mod_3),
  stars = TRUE, 
  output = "flextable") |> 
  autofit() |> 
  save_as_docx(path = "04_out/Smorgasbord/tab_general.docx")
