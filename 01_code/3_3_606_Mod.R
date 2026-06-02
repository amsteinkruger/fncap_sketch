# Estimate something for 2YP.

#  Clear the environment.

rm(list = ls())

#  Start timing. 

time_start = Sys.time()

#  Get data. 

dat = 
  "03_intermediate/dat_firms_implicit_3_1.csv" %>% 
  read_csv %>% 
  mutate(MBF_Both = MBF_DouglasFir + MBF_WesternHemlock)

dat_explicit = 
  "03_intermediate/dat_firms_explicit_3_1.csv" %>% 
  read_csv %>% 
  mutate(MBF_Both = MBF_DouglasFir + MBF_WesternHemlock) %>% 
  mutate(MBF_Bin = ifelse(MBF_Both > 0, 1, 0)) %>% 
  relocate(MBF_Bin, MBF_Both, .after = "Count")

# Do:

# (1) Hurdle, No Inventory
# (2) Hurdle, Inventory
# (3) Hurdle, Inventory, Small Firms Only
# (4) Hurdle, Inventory, Large Firms Only (Preferred)

# AME, SE via Delta

# (1) Linear
# (2) Tobit
# (3) Heckit
# (4) Craggit

# AME, SE via Delta

# More specifications with more/fewer covariates?

# Demo

mod_1 = 
  feols(
    log(MBF_Both) ~ 
      SiteClassMode +
      Elevation + 
      Distance_Mill +
      # MBF_Standing_Forward +
      Stumpage_Lag_.[c(1, 5, 9)] +
      Stumpage_Mean_20 +
      Rate_Lag_.[c(1, 5, 9)] +
      Rate_Mean_20 +
      VPD_Lag_.[c(1, 5, 9)] +
      VPD_Mean_20 +
      Fire_30_Doughnut_Lag_1 # +
    # Fire30Doughnut_Mean_20
    | Landowner + Year,
    cluster = ~ Landowner,
    data = dat)

etable(mod_1)

# Hurdle Models?

#  First Stage

mod_hurdle_first_1 = 
  feglm(
    MBF_Bin ~
      SiteClassMode +
      Elevation + 
      Distance_Mill +
      # MBF_Standing_Forward +
      Stumpage_Lag_.[c(1, 5, 9)] +
      Stumpage_Mean_20 +
      Rate_Lag_.[c(1, 5, 9)] +
      Rate_Mean_20 +
      VPD_Lag_.[c(1, 5, 9)] +
      VPD_Mean_20 +
      Fire_30_Doughnut_Lag_1 # +
    # Fire30Doughnut_Mean_20
    | Landowner + Year,
    cluster = ~ Landowner,
    family = binomial(link = "logit"),
    data = dat_explicit
  )

#  Second Stage

mod_hurdle_second_1 = 
  feols(
    log(MBF_Both) ~ 
      SiteClassMode +
      Elevation +
      Distance_Mill +
      # MBF_Standing_Forward +
      Stumpage_Lag_.[c(1, 5, 9)] +
      Stumpage_Mean_20 +
      Rate_Lag_.[c(1, 5, 9)] +
      Rate_Mean_20 +
      VPD_Lag_.[c(1, 5, 9)] +
      VPD_Mean_20 +
      Fire_30_Doughnut_Lag_1 # +
    # Fire30Doughnut_Mean_20
    | Landowner + Year,
    cluster = ~ Landowner,
    data = dat)

#  AME

#  SE

# Checking out mhurdle: appears to throw computational errors pretty often.

# library(mhurdle)
# 
# mod_hurdle_first_1_mhurdle = 
#   mhurdle(
#     MBF_Both ~ 
#       Stumpage_Mean_20 +
#       Rate_Mean_20 +
#       VPD_Mean_20 + 
#       Fire_30_Doughnut_Lag_1 | 
#       SiteClassMode +
#       Elevation +
#       Distance_Mill +
#       Stumpage_Mean_20 +
#       Rate_Mean_20 +
#       VPD_Mean_20 + 
#       Fire_30_Doughnut_Lag_1,
#     data = dat_explicit)

# Figure out tex installation on this machine. 

# Exports?

# etable(mod_hurdle_first_1, 
#        mod_hurdle_second_1,
#        tex = TRUE,
#        style.tex = style.tex("aer"),
#        file = "04_out/tab_test.tex")

write_csv(mod_hurdle_first_1 %>% tidy, "04_out/tab_mod_first.csv")
write_csv(mod_hurdle_second_1 %>% tidy, "04_out/tab_mod_second.csv")

# write_csv(dat_ame, "04_out/tab_ame.csv")
