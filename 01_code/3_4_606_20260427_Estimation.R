# Estimate something for 2YP.

#  This neglects harvest detection and uses start dates for harvest quarters. 

dat = 
  "03_intermediate/dat_notifications_1_6.csv" %>% 
  read_csv %>% 
  mutate(Year = DateStart %>% year,
         Month = DateStart %>% month,
         Quarter = Month %>% multiply_by(1 / 3) %>% ceiling,
         Year_Quarter = paste0(Year, "_Q", Quarter)) %>% 
  filter(DateStart %>% year > 2014 & DateEnd %>% year < 2025) %>% 
  filter(ProportionDouglasFir > 0.50) %>% 
  filter(MBF > quantile(MBF, 0.01) & MBF < quantile(MBF, 0.99) &
           Acres > quantile(Acres, 0.01) & Acres < quantile(Acres, 0.99) &
           MBF_Acre > quantile(MBF_Acre, 0.01) & MBF_Acre < quantile(MBF_Acre, 0.99)) %>% 
  group_by(Landowner, Year_Quarter, Year, Quarter) %>% 
  summarize(MBF_Firm = MBF %>% sum(na.rm = TRUE),
            Acres = Acres %>% sum(na.rm = TRUE),
            Price = Stumpage_Real %>% mean(na.rm = TRUE),
            Fire_0 = Fire_0 %>% weighted.mean(na.rm = TRUE, w = MBF),
            Fire_15 = Fire_15 %>% weighted.mean(na.rm = TRUE, w = MBF),
            Fire_30 = Fire_30 %>% weighted.mean(na.rm = TRUE, w = MBF),
            VPD = VPD %>% weighted.mean(na.rm = TRUE, w = MBF),
            Rate_Fed = Rate_Fed %>% mean(na.rm = TRUE)) %>% 
  ungroup %>% 
  rename(MBF = MBF_Firm)

# Key Plots

vis_results_left = 
  
  
  # Linear Models
  
  library(fixest)

mod_1 = 
  feols(log(MBF) ~ log(Price) + log(Fire_0 + 1) + log(Fire_15 + 1) + log(Fire_30 + 1) + log(VPD) + Rate_Fed,
        cluster = ~ Landowner,
        data = dat)

mod_2 = 
  feols(log(MBF) ~ log(Price) + log(Fire_0 + 1) + log(Fire_15 + 1) + log(Fire_30 + 1) + log(VPD) + Rate_Fed | Quarter,
        cluster = ~ Landowner,
        data = dat)

mod_3 = 
  feols(log(MBF) ~ log(Price) + log(Fire_0 + 1) + log(Fire_15 + 1) + log(Fire_30 + 1) + log(VPD) + Rate_Fed | Landowner,
        cluster = ~ Landowner,
        data = dat)

mod_4 = 
  feols(log(MBF) ~ log(Price) + log(Fire_0 + 1) + log(Fire_15 + 1) + log(Fire_30 + 1) + log(VPD) + Rate_Fed | Landowner + Quarter,
        cluster = ~ Landowner,
        data = dat)

etable(mod_1, mod_2, mod_3, mod_4)