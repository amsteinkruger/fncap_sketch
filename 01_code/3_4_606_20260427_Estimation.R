# Estimate something for 2YP.

#  Clear the environment.

rm(list = ls())

#  Start timing. 

time_start = Sys.time()

#  Get data. 

dat = 
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
  rename(MBF = MBF_Firm)

# Linear Models

mod_1 = 
  feols(log(MBF) ~ log(Stumpage_Lag_1) + log(Lumber_Lag_1) + Fire_15_Doughnut + Fire_30_Doughnut + Fire_Proportion + log(VPD_Lag_1) + Rate_Lag_1,
        cluster = ~ Landowner,
        data = dat)

mod_2 = 
  feols(log(MBF) ~ log(Stumpage_Lag_1) + log(Lumber_Lag_1) + Fire_15_Doughnut + Fire_30_Doughnut + Fire_Proportion + log(VPD_Lag_1) + Rate_Lag_1 | Quarter,
        cluster = ~ Landowner,
        data = dat)

mod_3 = 
  feols(log(MBF) ~ log(Stumpage_Lag_1) + log(Lumber_Lag_1) + Fire_15_Doughnut + Fire_30_Doughnut + Fire_Proportion + log(VPD_Lag_1) + Rate_Lag_1 | Landowner,
        cluster = ~ Landowner,
        data = dat)

mod_4 = 
  feols(log(MBF) ~ log(Stumpage_Lag_1) + log(Lumber_Lag_1) + Fire_15_Doughnut + Fire_30_Doughnut + Fire_Proportion + log(VPD_Lag_1) + Rate_Lag_1 | Landowner + Quarter,
        cluster = ~ Landowner,
        data = dat)

etable(mod_1, mod_2, mod_3, mod_4, tex = FALSE)

# Hurdle Models?