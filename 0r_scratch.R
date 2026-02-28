# Estimate growth.

# Packages

library(tidyverse)
library(readxl)

# FIA

dat_growth = "data/FIA/OR_TREE_GRM_ESTN.csv" %>% read_csv

dat_growth_less = 
  dat_growth %>% 
  filter(COMPONENT == "SURVIVOR") %>% 
  filter(ESTIMATE == "VOLBFNET") %>% 
  filter(ESTN_TYPE == "SL") %>% 
  filter(ESTN_UNITS == "BF") %>% 
  mutate(ANN_NET_GROWTH_ACRE = ANN_NET_GROWTH * TPAGROW_UNADJ) %>% 
  filter(ntile(ANN_NET_GROWTH_ACRE, 100) %in% 2:99)

par_growth = mean(dat_growth_less$ANN_NET_GROWTH_ACRE)
