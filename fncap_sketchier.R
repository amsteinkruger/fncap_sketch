# note from FIA Guide:
# Based on the procedures described in Bechtold and Patterson
# (2005), this attribute must be adjusted using factors stored in the POP_STRATUM table to
# derive population estimates. Examples of estimating population totals are shown in The
# Forest Inventory and Analysis Database: Population Estimation User Guide.

# 0. Get packages.
# 1. Get FIA data in. 
# join ownership data from COND to PLOT on PLT_CN to CN (but check against potential alternative join keys)
# group PLOT records across years on CN, etc
# filter to plots that are private the entire time, include douglas fir (???), and are west of the cascades
#  - so jump into TREE, filter to douglas fir (and maybe on location while in there?), then antijoin onto PLOT or COND (???)
# get volume and acres for plot or subplot or cond or tree measurements (???) -- actually, tree measurements by plot, so back into TREE
# compute volume/acre at intervals of measurement years by plot for private douglas fir in OR west of the cascades following H&S
# visualize results to see if they're following H&S's appendix plots
# figure out site class piece

# 0. 

library(tidyverse)
library(ggpubr)

options(scipen = 999)

# 1.

dat_or_plot = "data/OR_PLOT.csv" %>% read_csv # 9 MB

dat_or_cond = "data/OR_COND.csv" %>% read_csv # 18 MB

dat_or_tree = "data/OR_TREE.csv" %>% read_csv # 429 MB

# ?. Filter observations plots to each first pair in western Oregon.
# (This needs explaining.)

dat_or_plot_less = 
  dat_or_plot %>% 
  filter(LON < -120) %>% # This is a placeholder filter for western Oregon.
  mutate(MATCH_CN = ifelse(is.na(PREV_PLT_CN), CN, PREV_PLT_CN)) %>% 
  group_by(MATCH_CN) %>% 
  filter(n() > 1) %>%
  ungroup %>% 
  select(STATECD, 
         UNITCD, 
         COUNTYCD, 
         PLOT, 
         MATCH_CN,
         INVYR, 
         MEASYEAR, 
         LON, 
         LAT)

# ?. Filter conditions to private Douglas fir.

dat_or_cond_less = 
  dat_or_cond %>% 
  filter(FORTYPCD %in% 201:203 & OWNGRPCD == 40) %>% 
  select(STATECD, 
         UNITCD, 
         COUNTYCD, 
         PLOT, 
         CONDID, 
         CONDPROP_UNADJ,
         INVYR, 
         STDAGE,
         SITECLCD,
         DSTRBCD1, 
         DSTRBYR1, 
         TRTCD1, 
         TRTYR1)

# ?. Join filters on plot and condition.

dat_or_keep = 
  dat_or_cond_less %>% 
  left_join(dat_or_plot_less) %>% 
  semi_join(dat_or_plot_less)

# ?. Filter trees on plot and condition, then on being Douglas fir.

dat_or_tree_less = 
  dat_or_tree %>% 
  select(ends_with("CN"),
         STATECD,
         UNITCD,
         COUNTYCD,
         PLOT,
         CONDID,
         TREE,
         INVYR,
         # STATUSCD,
         SPGRPCD,
         SPCD,
         # DIA,
         # DIAHTCD,
         # HT,
         # HTCD,
         # ACTUALHT,
         # SAWHT,
         # BOLEHT,
         # HTCALC,
         # STOCKING,
         # TOTAGE,
         # BHAGE,
         # starts_with("VOL"),
         VOLCFNET,
         # starts_with("DRYBIO"),
         # DRYBIO_AG,
         # DRYBIO_BG,
         # starts_with("CARBON"),
         # CARBON_AG,
         # CARBON_BG,
         TPA_UNADJ) %>% 
  # Get plot and condition information.
  left_join(dat_or_keep,
            by = c("STATECD", "UNITCD", "COUNTYCD", "PLOT", "CONDID", "INVYR")) %>% 
  # Filter on plot and condition.
  semi_join(dat_or_keep,
            by = c("STATECD", "UNITCD", "COUNTYCD", "PLOT", "CONDID", "INVYR")) %>% 
  # Filter on species group (down to Douglas firs). This is equivalent to SPCD == 202.
  filter(SPGRPCD == 10) 

# Aggregate to plot-acres (?!), then pivot so that each plot is an observation with columns for different timesteps.

dat_or_tree_wide = 
  dat_or_tree_less %>% 
  group_by(STATECD, # Mind implicit drops.
           UNITCD, 
           COUNTYCD, 
           PLOT, 
           # CONDID, 
           MATCH_CN, 
           INVYR, 
           MEASYEAR, 
           # STDAGE, 
           LON, 
           LAT) %>% 
  summarize(VOLCFNET = sum(VOLCFNET * TPA_UNADJ, na.rm = TRUE)) %>% # Aggregate to condition.
  ungroup %>% 
  group_by(STATECD, UNITCD, COUNTYCD, PLOT) %>% # , CONDID) %>% 
  filter(n() == 2) %>% # Drop stands without multiple observations. (This has some implicit problems if conditions change.)
  mutate(STAND = cur_group_id()) %>% # Get a persistent ID for stands.
  ungroup %>% 
  arrange(STAND) %>% 
  group_by(STAND) %>% 
  mutate(WHICH = ifelse(MEASYEAR == max(MEASYEAR), 1, 0)) %>% # Get an ID for first/second observations.
  ungroup %>% 
  pivot_wider(names_from = WHICH,
              values_from = c(INVYR, MEASYEAR, VOLCFNET)) %>% # STDAGE, 
  mutate(MEASYEAR_D = MEASYEAR_1 - MEASYEAR_0,
         # STDAGE_D = STDAGE_1 - STDAGE_0,
         VOLCFNET_D = VOLCFNET_1 - VOLCFNET_0,
         VOLCFNET_P = VOLCFNET_1 / VOLCFNET_0 - 1) %>% # %>% 
  filter(VOLCFNET_D > 0) # STDAGE_D > 0 & 

# Visualize:
#  Change in Volume ~ Time
#  % Change in Volume ~ Time
#  Plots on Pyromes
#  Growth Models (?!)

# plot(dat_or_tree_wide$VOLCFNET_0, dat_or_tree_wide$VOLCFNET_D)

vis_change_absolute = 
  dat_or_tree_wide %>% 
  ggplot() +
  geom_point(aes(x = VOLCFNET_0,
                 y = VOLCFNET_D),
             shape = 21) +
  theme_pubr()

# plot(dat_or_tree_wide$VOLCFNET_0, dat_or_tree_wide$VOLCFNET_P)
# plot(dat_or_tree_wide$VOLCFNET_0, log(dat_or_tree_wide$VOLCFNET_P))

vis_change_percent = 
  dat_or_tree_wide %>% 
  ggplot() +
  geom_point(aes(x = VOLCFNET_0,
                 y = log(VOLCFNET_P)),
             shape = 21) +
  theme_pubr()

# Following code is for reference only.

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
