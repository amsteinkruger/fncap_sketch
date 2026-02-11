# Reduce recent output to counts of surviving observations with different sample restrictions.

library(tidyverse)
library(magrittr)

dat = 
  "output/dat_notifications_more_annual.csv" %>% 
  read_csv

dat_less = 
  dat %>% 
  mutate(Restriction_Overlaps = (Intersects == 0 & Equals == 0),
         Restriction_DouglasFir = (ProportionDouglasFir > 0.50),
         Restriction_NDVI = !is.na(NDVI_Change),
         Restriction_PAD = (PAD == 0),
         Restriction_None = TRUE,
         Restriction_Any = (Restriction_Overlaps == TRUE | Restriction_DouglasFir == TRUE | Restriction_NDVI == TRUE | Restriction_PAD == TRUE),
         Restriction_All = (Restriction_Overlaps == TRUE & Restriction_DouglasFir == TRUE & Restriction_NDVI == TRUE & Restriction_PAD == TRUE),
         .keep = "none") %>% 
  summarize(Surviving_None = sum(Restriction_None),
            Surviving_Overlaps = sum(Restriction_Overlaps, na.rm = TRUE),
            Surviving_DouglasFir = sum(Restriction_DouglasFir, na.rm = TRUE),
            Surviving_NDVI = sum(Restriction_NDVI, na.rm = TRUE),
            Surviving_PAD = sum(Restriction_PAD, na.rm = TRUE),
            Surviving_Any = sum(Restriction_Any, na.rm = TRUE),
            Surviving_All = sum(Restriction_All, na.rm = TRUE)) %>% 
  pivot_longer(cols = everything()) %>% 
  rename(Restriction = name,
         Surviving_Observations = value) %>% 
  mutate(Restriction = Restriction %>% str_remove("Surviving_")) %T>% 
  write_csv("output/summary_restrictions.csv")
