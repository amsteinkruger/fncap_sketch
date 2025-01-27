# Useful question: Hashida and Fenichel use FLDAGE from condition, but at the plot level. It's not clear how they're aggregating. Is FLDAGE useful for us?
# (other questions: aggregation to wall-to-wall; role for site class; role for pyromes, other fire data; role for harvest, other disturbances)

# 0. Get packages.
# 1. Get data.
# 2. Filter plots to those (a) in western Oregon (b) with at least one pair of observations.
# 3. Filter conditions to private Douglas fir.
# 4. Join filters on plot and condition.
# 5. Use the result of (4) to filter trees, then filter trees to Douglas fir.
# 6. Aggregate to plot-acres (?!), then pivot so that rows are plots with timesteps in columns.

# 7. Visualize:
#     histograms for fun:
#      measurement years and their difference
#      volumes and their differences
#     straightforward objectives from last conversation over Zoom:
#      Change in Volume ~ Time
#      % Change in Volume ~ Time
#      Change in Volume ~ Year
#      % Change in Volume ~ Year
#     less straightforward objectives from last conversation over Zoom:
#      visualize site class
#      visualize pyromes
#      Growth Models (?!)

# 0. Get packages.

library(tidyverse)
library(ggpubr)

options(scipen = 999)

# 1. Get data.

dat_or_plot = "data/OR_PLOT.csv" %>% read_csv # 9 MB

dat_or_cond = "data/OR_COND.csv" %>% read_csv # 18 MB

dat_or_tree = "data/OR_TREE.csv" %>% read_csv # 429 MB

# 2. Filter plots to those (a) in western Oregon (b) with at least one pair of observations.

dat_or_plot_less = 
  dat_or_plot %>% 
  # Filter to western Oregon.
  filter(LON < -120) %>% 
  # Filter to pairs of observations.
  #  This drops plots without multiple observations.
  #  This also implicitly drops observations 3 through n of plots with multiple observations.
  mutate(MATCH_CN = ifelse(is.na(PREV_PLT_CN), CN, PREV_PLT_CN)) %>% 
  group_by(MATCH_CN) %>% 
  filter(n() > 1) %>%
  ungroup %>% 
  # Select columns to keep for joins.
  select(STATECD, 
         UNITCD, 
         COUNTYCD, 
         PLOT, 
         MATCH_CN,
         INVYR, 
         MEASYEAR, 
         LON, 
         LAT)

# 3. Filter conditions to private Douglas fir.

dat_or_cond_less = 
  dat_or_cond %>% 
  # Filter.
  filter(FORTYPCD %in% 201:203 & OWNGRPCD == 40) %>% 
  # Select columns to keep for joins. 
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

# 4. Join filters on plot and condition.

dat_or_keep = 
  dat_or_cond_less %>% 
  left_join(dat_or_plot_less) %>% 
  semi_join(dat_or_plot_less)

# 5. Use the result of (4) to filter trees, then filter trees to Douglas fir.

dat_or_tree_less = 
  dat_or_tree %>% 
  # Select columns that we might use.
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

# 6. Aggregate to plot-acres (?!), then pivot so that rows are plots with timesteps in columns.

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
  filter(n() == 2) %>% # Drop stands without multiple observations. 
  mutate(PLOT_UID = paste(STATECD, UNITCD, COUNTYCD, PLOT, sep = "_")) %>% 
  ungroup %>% 
  select(-c(STATECD, UNITCD, COUNTYCD, PLOT)) %>%
  relocate(PLOT_UID) %>% 
  arrange(PLOT_UID) %>% 
  group_by(PLOT_UID) %>% 
  mutate(WHICH = ifelse(MEASYEAR == max(MEASYEAR), 1, 0)) %>% # Get an ID for first/second observations.
  ungroup %>% 
  pivot_wider(names_from = WHICH,
              values_from = c(INVYR, MEASYEAR, VOLCFNET)) %>% # STDAGE, 
  mutate(MEASYEAR_D = MEASYEAR_1 - MEASYEAR_0,
         # STDAGE_D = STDAGE_1 - STDAGE_0,
         VOLCFNET_D = VOLCFNET_1 - VOLCFNET_0,
         VOLCFNET_P = VOLCFNET_1 / VOLCFNET_0 - 1)

# Visualize:
#  histograms for fun:
#   measurement years and their difference
#   volumes and their differences
#  actual objectives from last conversation over Zoom:
#   Change in Volume ~ Time
#   % Change in Volume ~ Time
#  maybe objectives from last conversation over Zoom?:
#   Plots on Pyromes
#   Growth Models (?!)

# measurement years

# hist(dat_or_tree_wide$MEASYEAR_0)
# hist(dat_or_tree_wide$MEASYEAR_1)
# hist(dat_or_tree_wide$MEASYEAR_D)

vis_histogram_years = 
  dat_or_tree_wide %>% 
  select(MEASYEAR_0, MEASYEAR_1) %>% 
  pivot_longer(cols = everything(),
               names_to = "which",
               names_prefix = "MEASYEAR",
               values_to = "year") %>% 
  mutate(which = ifelse(which == "_0", "First", "Second")) %>% 
  group_by(which, year) %>% 
  summarize(count = n()) %>% 
  ungroup %>% 
  arrange(year, which) %>% 
  mutate(year = year %>% factor) %>% 
  ggplot() +
  geom_col(aes(x = year,
               y = count,
               fill = which,
               color = which)) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(y = "Observations",
       fill = "Observation",
       color = "Observation") +
  theme_pubr() +
  theme(axis.title.x = element_blank())

# differences

# hist(dat_or_tree_wide$MEASYEAR_1 - dat_or_tree_wide$MEASYEAR_0)

vis_histogram_years_differences = 
  dat_or_tree_wide %>% 
  ggplot() +
  geom_histogram(aes(x = MEASYEAR_D),
                 binwidth = 1) +
  scale_x_continuous(breaks = 8:12) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = "Difference in Measurement Years",
       y = "Observations") +
  theme_pubr()

# volumes

# hist(dat_or_tree_wide$VOLCFNET_0)
# hist(dat_or_tree_wide$VOLCFNET_1)
# hist(dat_or_tree_wide$VOLCFNET_D)

vis_histogram_volumes = 
  dat_or_tree_wide %>% 
  select(VOLCFNET_0, VOLCFNET_1) %>% 
  pivot_longer(cols = everything(),
               names_to = "which",
               names_prefix = "VOLCFNET",
               values_to = "volume") %>% 
  mutate(which = ifelse(which == "_0", "First", "Second"),
         bin = volume %>% cut(breaks = seq(0, 22000, by = 1000),
                              labels = c("1-1000",
                                         paste(seq(1000, 21000, by = 1000),
                                               seq(2000, 22000, by = 1000),
                                               sep = "-"))),
         bin = bin %>% fct_na_value_to_level("0") %>% fct_relevel("0")) %>% 
  group_by(which, bin) %>% 
  summarize(count = n()) %>% 
  ungroup %>% 
  arrange(bin, which) %>% 
  ggplot() +
  geom_col(aes(x = bin,
               y = count,
               fill = which,
               color = which),
           position = "dodge") +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = "Plot Volume/Acre (VOLCFNET, Net Cubic-Foot Stem Wood Volume)",
       y = "Observations",
       fill = "Observation",
       color = "Observation") +
  theme_pubr() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

# differences

# hist(dat_or_tree_wide$MEASYEAR_1 - dat_or_tree_wide$MEASYEAR_0)

vis_histogram_volumes_differences = 
  dat_or_tree_wide %>% 
  select(VOLCFNET_D) %>% 
  mutate(bin = VOLCFNET_D %>% cut(breaks = seq(0, 5000, by = 1000),
                                  labels = paste(seq(0, 4000, by = 1000),
                                                 seq(1000, 5000, by = 1000),
                                                 sep = "-")),
         bin = bin %>% fct_na_value_to_level("Negative")) %>% 
  group_by(bin) %>% 
  summarize(count = n()) %>% 
  ungroup %>% 
  arrange(bin) %>% 
  ggplot() +
  geom_col(aes(x = bin,
               y = count)) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = "Change in Plot Volume/Acre (VOLCFNET)",
       y = "Observations",
       fill = "Observation",
       color = "Observation") +
  theme_pubr() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

# Get into change over time.

# plot(dat_or_tree_wide$VOLCFNET_0, dat_or_tree_wide$VOLCFNET_D)

vis_change_absolute = 
  dat_or_tree_wide %>% 
  ggplot() +
  geom_point(aes(x = VOLCFNET_0,
                 y = VOLCFNET_D),
             shape = 21) +
  theme_pubr()

# plot(dat_or_tree_wide$VOLCFNET_0, dat_or_tree_wide$VOLCFNET_P)

vis_change_percent = 
  dat_or_tree_wide %>% 
  ggplot() +
  geom_point(aes(x = VOLCFNET_0,
                 y = VOLCFNET_P),
             shape = 21) +
  theme_pubr()

# plot(dat_or_tree_wide$VOLCFNET_0, log(dat_or_tree_wide$VOLCFNET_P))

vis_change_percent_log = 
  dat_or_tree_wide %>% 
  ggplot() +
  geom_point(aes(x = VOLCFNET_0,
                 y = log(VOLCFNET_P)),
             shape = 21) +
  theme_pubr()

# Switch to years (in )

dat_or_tree_wider = 
  dat_or_tree_wide %>% 
  mutate(VOLCFNET_D_ANN = VOLCFNET_D / MEASYEAR_D,
         VOLCFNET_P_ANN = VOLCFNET_D_ANN / VOLCFNET_0) # This seems wrong.

vis_change_absolute_annual = 
  dat_or_tree_wider %>% 
  ggplot() +
  geom_point(aes(x = VOLCFNET_0,
                 y = VOLCFNET_D_ANN),
             shape = 21) +
  theme_pubr()

vis_change_percent_log_annual = 
  dat_or_tree_wider %>% 
  ggplot() +
  geom_point(aes(x = VOLCFNET_0,
                 y = log(VOLCFNET_P_ANN)),
             shape = 21) +
  theme_pubr()

# Filter to non-negative annual growth, then go ahead and slap a couple models on.

dat_or_tree_wider_less = 
  dat_or_tree_wider %>% 
  filter(VOLCFNET_D_ANN > 0)

# dropping scratch models until FLDAGE is pulled through from condition to support Hashida and Fenichel's approach
