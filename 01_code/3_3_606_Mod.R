# Estimate something for 2YP.

#  Clear the environment.

rm(list = ls())

#  Start timing. 

time_start = Sys.time()

#  Get data. 

dat = 
  "03_intermediate/dat_firms_implicit_3_1.csv" %>% 
  read_csv

dat_explicit = 
  "03_intermediate/dat_firms_explicit_3_1.csv" %>% 
  read_csv %>% 
  mutate(MBF_Bin = ifelse(MBF > 0, 1, 0)) %>% 
  relocate(MBF_Bin, .after = "MBF")

# Linear Models

#  Simple

mod_1 = 
  feols(log(MBF) ~ log(Stumpage_Lag_1) + log(Lumber_Lag_1) + Fire_15_Doughnut + Fire_30 + Fire_Proportion + log(VPD_Lag_1) + Rate_Lag_1,
        cluster = ~ Landowner,
        data = dat)

mod_2 = 
  feols(log(MBF) ~ log(Stumpage_Lag_1) + log(Lumber_Lag_1) + Fire_15_Doughnut + Fire_30 + Fire_Proportion + log(VPD_Lag_1) + Rate_Lag_1 | Landowner,
        cluster = ~ Landowner,
        data = dat)

mod_3 = 
  feols(log(MBF) ~ log(Stumpage_Lag_1) + log(Lumber_Lag_1) + Fire_15_Doughnut + Fire_30 + Fire_Proportion + log(VPD_Lag_1) + Rate_Lag_1 | Quarter,
        cluster = ~ Landowner,
        data = dat)

mod_4 = 
  feols(log(MBF) ~ log(Stumpage_Lag_1) + log(Lumber_Lag_1) + Fire_15_Doughnut + Fire_30 + Fire_Proportion + log(VPD_Lag_1) + Rate_Lag_1 | Landowner + Quarter,
        cluster = ~ Landowner,
        data = dat)

etable(mod_1, mod_2, mod_3, mod_4, tex = FALSE)

#  Lags

mod_5 = feols(log(MBF) ~ Stumpage_Lag_.[1:20] + Lumber_Lag_.[1:20] + Rate_Lag_.[1:20] + VPD_Lag_.[1:20] + Fire_30 + Fire_Proportion,
              cluster = ~ Landowner,
              data = dat)

mod_6 = feols(log(MBF) ~ Stumpage_Lag_.[1:20] + Lumber_Lag_.[1:20] + Rate_Lag_.[1:20] + VPD_Lag_.[1:20] + Fire_30 + Fire_Proportion | Landowner,
              cluster = ~ Landowner,
              data = dat)

mod_7 = feols(log(MBF) ~ Stumpage_Lag_.[1:20] + Lumber_Lag_.[1:20] + Rate_Lag_.[1:20] + VPD_Lag_.[1:20] + Fire_30 + Fire_Proportion | Quarter,
              cluster = ~ Landowner,
              data = dat)

mod_8 = feols(log(MBF) ~ Stumpage_Lag_.[1:20] + Lumber_Lag_.[1:20] + Rate_Lag_.[1:20] + VPD_Lag_.[1:20] + Fire_30 + Fire_Proportion | Landowner + Quarter,
              cluster = ~ Landowner,
              data = dat)

etable(mod_5, mod_6, mod_7, mod_8, tex = FALSE)

# Means

mod_9 = feols(log(MBF) ~ Stumpage_Mean_.[c(4, 8, 12, 16, 20)] + Lumber_Mean_.[c(4, 8, 12, 16, 20)] + Rate_Mean_.[c(4, 8, 12, 16, 20)] + VPD_Mean_.[c(4, 8, 12, 16, 20)] + Fire_30 + Fire_Proportion,
              cluster = ~ Landowner,
              data = dat)

mod_10 = feols(log(MBF) ~ Stumpage_Mean_.[c(4, 8, 12, 16, 20)] + Lumber_Mean_.[c(4, 8, 12, 16, 20)] + Rate_Mean_.[c(4, 8, 12, 16, 20)] + VPD_Mean_.[c(4, 8, 12, 16, 20)] + Fire_30 + Fire_Proportion | Landowner,
               cluster = ~ Landowner,
               data = dat)

mod_11 = feols(log(MBF) ~ Stumpage_Mean_.[c(4, 8, 12, 16, 20)] + Lumber_Mean_.[c(4, 8, 12, 16, 20)] + Rate_Mean_.[c(4, 8, 12, 16, 20)] + VPD_Mean_.[c(4, 8, 12, 16, 20)] + Fire_30 + Fire_Proportion | Quarter,
               cluster = ~ Landowner,
               data = dat)

mod_12 = feols(log(MBF) ~ Stumpage_Mean_.[c(4, 8, 12, 16, 20)] + Lumber_Mean_.[c(4, 8, 12, 16, 20)] + Rate_Mean_.[c(4, 8, 12, 16, 20)] + VPD_Mean_.[c(4, 8, 12, 16, 20)] + Fire_30 + Fire_Proportion | Landowner + Quarter,
               cluster = ~ Landowner,
               data = dat)

etable(mod_9, mod_10, mod_11, mod_12, tex = FALSE)

# Hurdle Models?

#  First Stage

mod_hurdle_first_1 = 
  feglm(
    MBF_Bin ~ log(Stumpage_Lag_1) + log(Lumber_Lag_1) + Fire_30 + Fire_Proportion + log(VPD_Lag_1) + Rate_Lag_1,
    family = binomial(link = "logit"),
    data = dat_explicit
  )

mod_hurdle_first_2 = 
  feglm(
    MBF_Bin ~ Stumpage_Lag_.[1:20] + Lumber_Lag_.[1:20] + Rate_Lag_.[1:20] + VPD_Lag_.[1:20] + Fire_30 + Fire_Proportion,
    family = binomial(link = "logit"),
    data = dat_explicit
  )

mod_hurdle_first_3 = 
  feglm(
    MBF_Bin ~ Stumpage_Mean_.[c(4, 8, 12, 16, 20)] + Lumber_Mean_.[c(4, 8, 12, 16, 20)] + Rate_Mean_.[c(4, 8, 12, 16, 20)] + VPD_Mean_.[c(4, 8, 12, 16, 20)] + Fire_30 + Fire_Proportion,
    family = binomial(link = "logit"),
    data = dat_explicit
  )

#   Cherrypicking for easier handling. 

mod_hurdle_first_cherry = 
  feglm(
    MBF_Bin ~ log(Stumpage_Mean_20) + log(VPD_Mean_4) + Fire_30 + Fire_Proportion,
    family = binomial(link = "logit"),
    # cluster ~ Landowner,
    data = dat_explicit # %>% group_by(Landowner) %>% filter(sum(MBF_Bin) > 1) %>% ungroup
  )

#  Second Stage

mod_hurdle_second_cherry = 
  feols(
    log(MBF) ~ log(Stumpage_Mean_20) + log(VPD_Mean_4) + Fire_30 + Fire_Proportion,
    # cluster ~ Landowner,
    data = dat
  )

#  AME

#   Set up another dataframe.

dat_predict = dat_explicit

#   Proceeding from generated test code. 

# predicted probabilities
p <- predict(mod_hurdle_first_cherry, type = "response")

# predicted conditional mean (log-linear case)
mu <- exp(predict(mod_hurdle_second_cherry, newdata = dat_predict))

# derivatives

#  first stage

beta <- coef(mod_hurdle_first_cherry)
X <- model.matrix(mod_hurdle_first_cherry)

dp_dx_1 <- p * (1 - p) * beta["log(Stumpage_Mean_20)"]
dp_dx_2 <- p * (1 - p) * beta["log(VPD_Mean_4)"]
dp_dx_3 <- p * (1 - p) * beta["Fire_30"]
dp_dx_4 <- p * (1 - p) * beta["Fire_Proportion"]

#  second stage

gamma <- coef(mod_hurdle_second_cherry)

dmu_dx_1 <- mu * gamma["log(Stumpage_Mean_20)"]
dmu_dx_2 <- mu * gamma["log(VPD_Mean_4)"]
dmu_dx_3 <- mu * gamma["Fire_30"]
dmu_dx_4 <- mu * gamma["Fire_Proportion"]

#  combine

me_i_1 <- dp_dx_1 * mu + p * dmu_dx_1
me_i_2 <- dp_dx_2 * mu + p * dmu_dx_2
me_i_3 <- dp_dx_3 * mu + p * dmu_dx_3
me_i_4 <- dp_dx_4 * mu + p * dmu_dx_4

dat_ame = 
  tibble(var = c("log(Stumpage_Mean_20)", "log(VPD_Mean_4)", "Fire_30", "Fire_Proportion"),
         ame = c(mean(me_i_1), mean(me_i_2), mean(me_i_3), mean(me_i_4)))

# See Wu et al. and Greene to work this out from scratch. Can it for now. 

# Figure out tex installation on this machine. 

# Exports?

etable(mod_hurdle_first_cherry, 
       mod_hurdle_second_cherry,
       tex = TRUE,
       style.tex = style.tex("aer"),
       file = "04_out/tab_test.tex")

# etable(mod_hurdle_first_cherry, 
#        mod_hurdle_second_cherry,
#        export = "04_output/tab_test.png")

write_csv(mod_hurdle_first_cherry %>% tidy, "04_out/tab_mod_first.csv")
write_csv(mod_hurdle_second_cherry %>% tidy, "04_out/tab_mod_second.csv")

write_csv(dat_ame, "04_out/tab_ame.csv")
