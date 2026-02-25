# Estimate simple linear models of MBF/Acre on a kitchen sink of variables.

# Need: 
#  - PAD for ownership type intersection/restriction
#  - owner company string
#  - owner scale (percentile in acres of clearcuts?)
#  - short-term VPD, fires

library(tidyverse)
library(magrittr)

dat = 
  "output/dat_notifications_more_annual.csv" %>% 
  read_csv %>% 
  mutate(Restriction_Overlaps = (Intersects == 0 & Equals == 0),
         Restriction_DouglasFir = (ProportionDouglasFir > 0.50),
         Restriction_NDVI = !is.na(NDVI_Change),
         Restriction_PAD = (PAD == 0),
         Restriction_None = TRUE,
         Restriction_Any = (Restriction_Overlaps == TRUE | Restriction_DouglasFir == TRUE | Restriction_NDVI == TRUE | Restriction_PAD == TRUE),
         Restriction_All = (Restriction_Overlaps == TRUE & Restriction_DouglasFir == TRUE & Restriction_NDVI == TRUE & Restriction_PAD == TRUE)) %>% 
  filter(Restriction_All == TRUE) %>% 
  select(UID, 
         Year_Start, 
         Month_Start, 
         MBF, 
         Acres, 
         Elevation, 
         Slope, 
         Roughness, 
         Pyrome, 
         VPD, 
         starts_with("Fire"),
         ProportionDouglasFir,
         starts_with("SiteClass"),
         starts_with("Distance"),
         starts_with("FPA"),
         Watershed,
         starts_with("Stumpage"),
         starts_with("Delivered")) %>% 
  mutate(MBF_Acre = MBF / Acres) %>% 
  filter(MBF < quantile(MBF, 0.99) & MBF > quantile(MBF, 0.01),
         Acres < quantile(Acres, 0.99) & Acres > quantile(Acres, 0.01),
         MBF_Acre < quantile(MBF_Acre, 0.99) & MBF_Acre > quantile(MBF_Acre, 0.01))

mod_less = dat %>% lm(MBF_Acre ~ Elevation + Slope + VPD + ProportionDouglasFir + SiteClass_Med + Distance_Place + FPA_1 + Stumpage_Real_PPI_Timber, data = .)
