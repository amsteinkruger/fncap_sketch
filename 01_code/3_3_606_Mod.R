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

dat_small_explicit = 
  dat_explicit %>% 
  group_by(Landowner) %>% 
  mutate(MBF_All = sum(MBF_Both)) %>% 
  ungroup %>% 
  filter(MBF_All <= quantile(MBF_All, 0.50))

dat_large_explicit = 
  dat_explicit %>% 
  group_by(Landowner) %>% 
  mutate(MBF_All = sum(MBF_Both)) %>% 
  ungroup %>% 
  filter(MBF_All > quantile(MBF_All, 0.50))

vec_small = dat_small_explicit$Landowner %>% unique
vec_large = dat_large_explicit$Landowner %>% unique

dat_small = dat %>% filter(Landowner %in% vec_small)
dat_large = dat %>% filter(Landowner %in% vec_large)
  
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
      MBF_Standing_Forward +
      Stumpage_Lag_.[c(1, 5, 9)] +
      Stumpage_Mean_20 +
      Rate_Lag_.[c(1, 5, 9)] +
      Rate_Mean_20 +
      VPD_Lag_.[c(1, 5, 9)] +
      VPD_Mean_20 +
      Fire_30_Doughnut_Lag_1
    | Year,
    cluster = ~ Landowner,
    family = binomial(link = "logit"),
    data = dat_explicit
  )

mod_hurdle_first_2 = 
  feglm(
    MBF_Bin ~
      SiteClassMode +
      Elevation + 
      Distance_Mill +
      MBF_Standing_Forward +
      Stumpage_Lag_.[c(1, 5, 9)] +
      Stumpage_Mean_20 +
      Rate_Lag_.[c(1, 5, 9)] +
      Rate_Mean_20 +
      VPD_Lag_.[c(1, 5, 9)] +
      VPD_Mean_20 +
      Fire_30_Doughnut_Lag_1
    | Landowner + Year,
    cluster = ~ Landowner,
    family = binomial(link = "logit"),
    data = dat_explicit
  )

mod_hurdle_first_3 = 
  feglm(
    MBF_Bin ~
      SiteClassMode +
      Elevation + 
      Distance_Mill +
      MBF_Standing_Forward +
      Stumpage_Lag_.[c(1, 5, 9)] +
      Stumpage_Mean_20 +
      Rate_Lag_.[c(1, 5, 9)] +
      Rate_Mean_20 +
      VPD_Lag_.[c(1, 5, 9)] +
      VPD_Mean_20 +
      Fire_30_Doughnut_Lag_1
    | Landowner + Year,
    cluster = ~ Landowner,
    family = binomial(link = "logit"),
    data = dat_small_explicit
  )

mod_hurdle_first_4 = 
  feglm(
    MBF_Bin ~
      SiteClassMode +
      Elevation + 
      Distance_Mill +
      MBF_Standing_Forward +
      Stumpage_Lag_.[c(1, 5, 9)] +
      Stumpage_Mean_20 +
      Rate_Lag_.[c(1, 5, 9)] +
      Rate_Mean_20 +
      VPD_Lag_.[c(1, 5, 9)] +
      VPD_Mean_20 +
      Fire_30_Doughnut_Lag_1
    | Landowner + Year,
    cluster = ~ Landowner,
    family = binomial(link = "logit"),
    data = dat_large_explicit
  )

etable(mod_hurdle_first_1, mod_hurdle_first_2, mod_hurdle_first_3, mod_hurdle_first_4)

#  Second Stage

mod_hurdle_second_1 = 
  feols(
    log(MBF_Both) ~ 
      SiteClassMode +
      Elevation +
      Distance_Mill +
      MBF_Standing_Forward +
      Stumpage_Lag_.[c(1, 5, 9)] +
      Stumpage_Mean_20 +
      Rate_Lag_.[c(1, 5, 9)] +
      Rate_Mean_20 +
      VPD_Lag_.[c(1, 5, 9)] +
      VPD_Mean_20 +
      Fire_30_Doughnut_Lag_1
    | Year,
    cluster = ~ Landowner,
    data = dat)

mod_hurdle_second_2 = 
  feols(
    log(MBF_Both) ~ 
      SiteClassMode +
      Elevation +
      Distance_Mill +
      MBF_Standing_Forward +
      Stumpage_Lag_.[c(1, 5, 9)] +
      Stumpage_Mean_20 +
      Rate_Lag_.[c(1, 5, 9)] +
      Rate_Mean_20 +
      VPD_Lag_.[c(1, 5, 9)] +
      VPD_Mean_20 +
      Fire_30_Doughnut_Lag_1
    | Landowner + Year,
    cluster = ~ Landowner,
    data = dat)

mod_hurdle_second_3 = 
  feols(
    log(MBF_Both) ~ 
      SiteClassMode +
      Elevation +
      Distance_Mill +
      MBF_Standing_Forward +
      Stumpage_Lag_.[c(1, 5, 9)] +
      Stumpage_Mean_20 +
      Rate_Lag_.[c(1, 5, 9)] +
      Rate_Mean_20 +
      VPD_Lag_.[c(1, 5, 9)] +
      VPD_Mean_20 +
      Fire_30_Doughnut_Lag_1
    | Landowner + Year,
    cluster = ~ Landowner,
    data = dat_small)

mod_hurdle_second_4 = 
  feols(
    log(MBF_Both) ~ 
      SiteClassMode +
      Elevation +
      Distance_Mill +
      MBF_Standing_Forward +
      Stumpage_Lag_.[c(1, 5, 9)] +
      Stumpage_Mean_20 +
      Rate_Lag_.[c(1, 5, 9)] +
      Rate_Mean_20 +
      VPD_Lag_.[c(1, 5, 9)] +
      VPD_Mean_20 +
      Fire_30_Doughnut_Lag_1
    | Landowner + Year,
    cluster = ~ Landowner,
    data = dat_large)


etable(mod_hurdle_second_1, mod_hurdle_second_2, mod_hurdle_second_3, mod_hurdle_second_4)

#  AME

#  SE

# Exports

library(modelsummary)
library(flextable)

modelsummary(
  list("A" = mod_hurdle_first_1,
       "B" = mod_hurdle_first_2,
       "C" = mod_hurdle_first_3,
       "D" = mod_hurdle_first_4),
       stars = TRUE, 
       output = "flextable") |> 
  autofit() |> 
  save_as_docx(path = "04_out/tab_first.docx")

modelsummary(
  list("A" = mod_hurdle_second_1,
       "B" = mod_hurdle_second_2,
       "C" = mod_hurdle_second_3,
       "D" = mod_hurdle_second_4),
  stars = TRUE, 
  output = "flextable") |> 
  autofit() |> 
  save_as_docx(path = "04_out/tab_second.docx")

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

# write_csv(mod_hurdle_first_1 %>% tidy, "04_out/tab_mod_first.csv")
# write_csv(mod_hurdle_second_1 %>% tidy, "04_out/tab_mod_second.csv")

# write_csv(dat_ame, "04_out/tab_ame.csv")
