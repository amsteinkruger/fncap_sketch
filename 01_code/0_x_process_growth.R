# Wrangle FIA and estimate growth models.

#  Set up as a standalone, needs to be folded in. Note FIA format. 

#  Packages

library(tidyverse) # General
library(gt) # Tables
library(stargazer) # Regression Tables
library(magrittr) # Pipes

color_beav = "#D73F09" # In the absence of a beav theme.

#  Data

dat_condition =
  "data/OR_COND.csv" %>%
  read_csv %>%
  select(INVYR,
         PLT_CN,
         CONDID,
         OWNGRPCD,
         FORTYPCD,
         SITECLCD)

dat_tree =
  "data/OR_TREE.csv" %>%
  read_csv %>%
  select(INVYR,
         PLT_CN,
         TRE_CN = CN,
         CONDID,
         SPGRPCD)

dat_growth =
  "data/OR_TREE_GRM_ESTN.csv" %>%
  read_csv %>%
  select(INVYR,
         PLT_CN,
         TRE_CN,
         LAND_BASIS,
         ESTIMATE,
         COMPONENT,
         SUBPTYP_GRM,
         REMPER,
         TPAGROW_UNADJ,
         ANN_NET_GROWTH,
         EST_BEGIN,
         EST_END) %>%
  filter(LAND_BASIS == "TIMBERLAND") %>% # Subset to timberland
  filter(SUBPTYP_GRM == 1) %>% # Subset to subplots
  filter(ESTIMATE == "VOLBFNET") %>% # Subset to net board feet
  filter(COMPONENT == "SURVIVOR") %>% # Subset to surviving trees
  select(-ESTIMATE, -COMPONENT, -LAND_BASIS) %>%
  left_join(dat_tree) %>%
  filter(SPGRPCD == 10) %>% # Subset to Douglas fir species
  left_join(dat_condition) %>%
  filter(OWNGRPCD == 40) %>% # Subset to private owners
  filter(FORTYPCD %in% 200:203) %>% # Subset to Douglas fir conditions
  # Board feet/acre
  mutate(EST_BEGIN_ACRE = EST_BEGIN * TPAGROW_UNADJ,
         EST_END_ACRE = EST_END * TPAGROW_UNADJ,
         ANN_NET_GROWTH_ACRE = ANN_NET_GROWTH * TPAGROW_UNADJ) %>% 
  # Board feet/acre by plot
  group_by(INVYR, PLT_CN, REMPER, SITECLCD) %>%
  summarize(EST_BEGIN_ACRE_PLOT = sum(EST_BEGIN_ACRE),
            EST_END_ACRE_PLOT = sum(EST_END_ACRE),
            ANN_NET_GROWTH_ACRE_PLOT = sum(ANN_NET_GROWTH_ACRE)) %>%
  ungroup %>% 
  # Drop outliers
  filter(ntile(ANN_NET_GROWTH_ACRE_PLOT, 100) %in% 2:99) %>% 
  filter(ntile(EST_BEGIN_ACRE_PLOT, 100) %in% 2:99) %>% 
  filter(ntile(EST_END_ACRE_PLOT, 100) %in% 2:99) %>% 
  # Site classes into bins and BF to MBF. 
  mutate(SITECLCD_Bin = ifelse(SITECLCD < 4, 0, 1),
         MBF_0 = EST_BEGIN_ACRE_PLOT / 1000,
         MBF_1 = EST_END_ACRE_PLOT / 1000,
         MBF_Annual = ANN_NET_GROWTH_ACRE_PLOT / 1000) %>% 
  select(-ends_with("_PLOT")) %T>% 
  # Export
  write_csv("data/dat_processed.csv")

dat = dat_growth # Band-Aid for name issues

# Visualization

vis_2 = 
  dat %>% 
  ggplot(aes(x = MBF_0,
             y = MBF_Annual,
             color = SITECLCD_Bin %>% factor(labels = c("1-3", "4-6")))) + 
  geom_point(shape = 21,
             fill = NA) +
  geom_rug() +
  scale_color_manual(values = c("black", color_beav)) +
  labs(x = "Initial MBF/Acre",
       y = "Annualized Growth in MBF/Acre",
       color = "Site Class") +
  theme_minimal() 

ggsave("out/vis_2.png",
       vis_2,
       dpi = 300,
       width = 6,
       height = 4.5)

# Estimation

mod = 
  dat %>% 
  mutate(Y = log(MBF_Annual / MBF_0),
         X_1 = MBF_0,
         X_2 = SITECLCD_Bin, 
         .keep = "none") %>% 
  lm(Y ~ X_1 + X_2,
     data = .)

b_0 = mod$coefficients[[1]]
b_1 = mod$coefficients[[2]]
b_2 = mod$coefficients[[3]]

stargazer(mod, type = "html")

# Results Visualization

vis_4 = 
  dat %>% 
  ggplot() + 
  geom_point(aes(x = MBF_0,
                 y = MBF_Annual,
                 color = SITECLCD_Bin %>% factor(labels = c("1-3", "4-6"))),
             shape = 21,
             alpha = 0.50, 
             fill = NA) +
  geom_function(aes(x = MBF_0,
                    color = "1-3"),
                fun = ~ .x * exp(b_0 + b_1 * .x)) +
  geom_function(aes(x = MBF_0,
                    color = "4-6"),
                fun = ~ .x * exp(b_0 + b_1 * .x + b_2)) +
  scale_color_manual(values = c("black", color_beav)) +
  labs(x = "Initial MBF/Acre",
       y = "Annualized Change in MBF/Acre",
       color = "Site Class") +
  theme_minimal()

ggsave("out/vis_4.png",
       vis_4,
       dpi = 300,
       width = 6,
       height = 4.5)
