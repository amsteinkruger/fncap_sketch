# 0. Get packages.
# 1. Get FIA data in. Check gaps. Subset to an interesting locale. 
# 2. Visualize distribution of plots. 
# 3. Visualize growth at age.
# 4. Estimate growth at age with a linear model.
# 5. Estimate growth at age with a nonlinear model.

# 0. 

library(tidyverse)
library(ggpubr)

# 1.

dat_or_plot = "data/OR_PLOT.csv" %>% read_csv # 9 MB

dat_or_tree = "data/OR_TREE.csv" %>% read_csv # 429 MB

#  Check whether data support growth models from one-off age estimates and/or repeated observations.

#   TOTAGE describes one-off age estimates.

dat_totage_check = dat_or_tree %>% drop_na(TOTAGE) %>% nrow

#   Relating CN to PREV_TRE_CN identifies repeated observations.

dat_cn_check = 
  dat_or_tree %>% 
  select(CN, PREV_TRE_CN)

dat_id_check = 
  dat_cn_check %>% 
  left_join(dat_cn_check, by = c("PREV_TRE_CN" = "CN")) %>% 
  left_join(dat_cn_check, by = c("PREV_TRE_CN.y" = "CN"))

dat_id_check %>% drop_na(PREV_TRE_CN.x) %>% nrow
dat_id_check %>% drop_na(PREV_TRE_CN.y) %>% nrow
dat_id_check %>% drop_na(PREV_TRE_CN.y.y) %>% nrow

# problem: there aren't a lot of trees with totage or repeated observations (of 749793 total observations)
#  totage: 31835; these are almost all from 1999, which is unhelpful
#  cn: 325895 to 27136 to 0 for t-1, t-2, t-3

# so, lacking a perfect option, let's go with totage as the simpler imperfect option.

#  Subset to Benton County for local interest.

dat_benton = 
  dat_or_tree %>% 
  filter(COUNTYCD == 3) %>% 
  filter(!is.na(TOTAGE)) %>% 
  left_join(dat_or_plot %>% 
              filter(COUNTYCD == 3) %>% 
              select(INVYR,
                     MEASYEAR,
                     CN,
                     LON,
                     LAT,
                     ELEV),
            by = c("PLT_CN" = "CN",
                   "INVYR")) %>% 
  select(CN,
         INVYR,
         MEASYEAR,
         TOTAGE,
         DIA,
         HT,
         LON,
         LAT,
         ELEV) %>% 
  # Cut outliers for easier model fitting.
  mutate(rank = TOTAGE %>% percent_rank) %>% 
  filter(rank <= 0.90) %>% 
  select(-rank)

# 2.

library(terra)
library(tidyterra)

#  Get boundaries. These geodata originate with the Forest Service. See /data or /documentation for details.

dat_spa_boundaries = 
  "data/S_USA.ALPGeopoliticalUnit.gdb" %>% 
  vect %>% 
  subset(TYPENAMEREFERENCE == "County" & NAME == "Benton" & STATENAME == "Oregon", NSE = TRUE)

dat_spa_plots = 
  dat_benton %>% 
  select(LON, LAT) %>% 
  distinct %>% 
  as.matrix %>% 
  vect("points",
       crs = crs(dat_spa_boundaries))

vis_spa_plots = 
  ggplot() + 
  geom_spatvector(data = dat_spa_boundaries) +
  geom_spatvector(data = dat_spa_plots) +
  theme_void()

vis_spa_plots %>% print

# 3.

#  Check out the data.

dat_benton %>% slice_head(n = 5)

vis_mod_scatter = 
  dat_benton %>% 
  ggplot() +
  geom_point(aes(x = TOTAGE,
                 y = HT),
             alpha = 0.50) +
  scale_x_continuous(limits = c(0, NA)) +
  scale_y_continuous(limits = c(0, NA)) +
  theme_pubr()

vis_mod_scatter %>% print

#  Fit a linear model, just in case that does the trick.

mod_linear_benton = 
  dat_benton %>% 
  lm(formula = HT ~ TOTAGE, 
     data = .)

mod_linear_benton %>% summary

vis_mod_linear = 
  ggplot() + 
  geom_point(data = dat_benton,
             aes(x = TOTAGE,
                 y = HT),
             alpha = 0.50) +
  geom_line(data = dat_benton %>% filter(TOTAGE == TOTAGE %>% max | TOTAGE == TOTAGE %>% min),
            aes(x = TOTAGE,
                y = mod_linear_benton$coefficients[[1]] + mod_linear_benton$coefficients[[2]] * TOTAGE),
            alpha = 1.00,
            color = "#D73F09",
            linewidth = 1.00) +
  scale_x_continuous(limits = c(0, NA)) +
  scale_y_continuous(limits = c(0, NA)) +
  theme_pubr()

vis_mod_linear %>% print

#  Fit a nonlinear model (VBGF) for a more credible attempt.

library(gslnls) 

alpha_start = 200 # Asymptotic height.
beta_start = 1 # Inverse of annual growth rate.
gamma_start = 0 # Modeling artifact analogous to y-intercept.

mod_nonlinear_benton = 
  dat_benton %>% 
  filter(TOTAGE < 100) %>% 
  gsl_nls(fn = HT ~ alpha * (1 - exp(-(beta / 100) * (TOTAGE - gamma))),
          data = .,
          algorithm = "dogleg",
          start = c(alpha = alpha_start, beta = beta_start, gamma = gamma_start))

m_n_b_coef = mod_nonlinear_benton$m$getPars()

mod_nonlinear_benton %>% summary

fun_nonlinear_benton = function(alpha, beta, gamma, age){alpha * (1 - exp(-(beta / 100) * (age - gamma)))}

vis_mod_nonlinear = 
  dat_benton %>% 
  ggplot() + 
  geom_point(aes(x = TOTAGE,
                 y = HT),
             alpha = 0.50) +
  geom_function(fun = fun_nonlinear_benton,
                args = list(alpha = m_n_b_coef[["alpha"]], beta = m_n_b_coef[["beta"]], gamma = m_n_b_coef[["gamma"]]),
                alpha = 1.00,
                color = "#D73F09",
                linewidth = 1.00) +
  scale_x_continuous(limits = c(0, NA)) +
  scale_y_continuous(limits = c(0, NA)) +
  theme_pubr()

vis_mod_nonlinear %>% print
