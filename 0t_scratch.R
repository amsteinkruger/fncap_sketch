# Estimate something for 2YP.

# Need: 
#  - real quarterly harvest detection (x) (placeholder)
#  - owner company string (x)
#  - owner scale (percentile in acres of clearcuts?) (x)
#  - short-term VPD, fires (nope)
#  - something for land rents (x) (placeholder)
#  - something for discount rates (x)
#  - something for yield increase in time ( ) (placeholder)
#  - stupid counties
#  - maybe treatment intersections (but not really without more data)
#  - probably proportional intersection for public lands (instead of 1/0)
#  - recover some observations with intersections (x)

# Out:
#  - maps of notifications, MBF, acre by county and HUC8
#  - plots of notifications, MBF, acre in time by firm
#  - plots of notification-MBF-acre relationships (appendix)
#  - table of sample restriction impacts
#  - table of summary statistics by model variable

# Packages

library(tidyverse)
library(magrittr)
library(plm)
library(stargazer)
library(sandwich)

# Data

dat = 
  "output/dat_notifications_more_annual.csv" %>% 
  read_csv %>% 
  mutate(Restriction_Overlaps = (Intersect_Maximum < 0.01),
         Restriction_DouglasFir = (ProportionDouglasFir > 0.50),
         Restriction_NDVI = (NDVI_Detect == 1),
         Restriction_Any = (Restriction_Overlaps == TRUE | Restriction_DouglasFir == TRUE | Restriction_NDVI == TRUE),
         Restriction_All = (Restriction_Overlaps == TRUE & Restriction_DouglasFir == TRUE & Restriction_NDVI == TRUE)) %>% 
  filter(Restriction_All == TRUE) %>% 
  select(UID, 
         Year_Start, 
         Month_Start, 
         Company,
         MBF, 
         Acres, 
         Elevation, 
         Slope, 
         Roughness, 
         Pyrome, 
         VPD, 
         ProportionDouglasFir,
         SiteClass_Mod,
         starts_with("Distance"),
         Watershed,
         County,
         Stumpage_Real_PPI_Timber) %>% 
  mutate(MBF_Acre = MBF / Acres) %>% 
  filter(MBF < quantile(MBF, 0.99) & MBF > quantile(MBF, 0.01),
         Acres < quantile(Acres, 0.99) & Acres > quantile(Acres, 0.01),
         MBF_Acre < quantile(MBF_Acre, 0.99) & MBF_Acre > quantile(MBF_Acre, 0.01)) %>% 
  mutate(MBF_Prime = par_growth * (1 / 1000) * Acres + MBF)

#  - maps of notifications and firms by county

dat_counties = 
  dat %>% 
  group_by(Year_Start, County) %>% 
  summarize(MBF = sum(MBF),
            Firms = n_distinct(Company)) %>% 
  group_by(County) %>% 
  summarize(MBF = mean(MBF),
            Firms = mean(Firms)) %>% 
  left_join("data/TIGER.gdb" %>% 
              vect(layer = "County") %>% 
              select(County = NAMELSAD) %>% 
              project("EPSG:2992"),
            .) %>% 
  crop(dat_pyrome) %>% 
  drop_na

vis_counties_mbf =
  dat_counties %>% 
  ggplot() +
  geom_spatvector(aes(fill = MBF / 1000000),
                  color = "white",
                  linewidth = 0.20) +
  scale_fill_viridis(option = "E",
                     limits = c(0, NA),
                     # breaks = c(0, 1, 2),
                     guide = guide_colorbar(title.position = "top")) +
  labs(fill = "Board Feet (Billions)") +
  theme_void() +
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        legend.ticks = element_blank(),
        legend.key.height = unit(0.25, "lines"),
        legend.key.width = unit(1.5, "lines"),
        legend.title = element_text(hjust = 0.5))


vis_counties_firms =
  dat_counties %>% 
  ggplot() +
  geom_spatvector(aes(fill = Firms),
                  color = "white",
                  linewidth = 0.20) +
  scale_fill_viridis(option = "G",
                     limits = c(0, NA),
                     # breaks = c(0, 40, 80),
                     guide = guide_colorbar(title.position = "top")) +
  labs(fill = "Timberland Owners") +
  theme_void() +
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        legend.ticks = element_blank(),
        legend.key.height = unit(0.25, "lines"),
        legend.key.width = unit(1.5, "lines"),
        legend.title = element_text(hjust = 0.5))

vis_counties = vis_counties_mbf + vis_counties_firms

ggsave("output/paper_vis_counties.png",
       dpi = 300,
       width = 6.5)

#  - plots of MBF in time by firm

dat_time = 
  dat %>% 
  group_by(Year_Start, Company) %>% 
  summarize(MBF = sum(MBF)) %>% 
  ungroup

vis_time = 
  dat_time %>% 
  ggplot() +
  geom_boxplot(aes(x = Year_Start %>% factor,
                   y = log(MBF)))

#  - plots of notification-MBF-acre relationships (appendix)
#  - table of sample restriction impacts
#  - table of summary statistics by model variable

# Yield Models

mod_less = 
  dat %>% 
  lm(MBF_Acre ~ 
       Elevation + 
       Slope + 
       VPD + 
       ProportionDouglasFir + 
       SiteClass_Med + 
       Distance_Place + 
       FPA_1 + 
       Stumpage_Real_PPI_Timber, 
     data = .)

mod_more = 
  dat %>% 
  plm(MBF_Acre ~ 
        Elevation + 
        Slope + 
        VPD + 
        ProportionDouglasFir + 
        SiteClass_Med + 
        Distance_Place + 
        FPA_1 + 
        Stumpage_Real_PPI_Timber, 
      index = c("Pyrome", "Year_Start"),
      data = .)

# Begin tabulation shenanigans.

m1 = mod_less
m2 = mod_more

se_list <- 
  list(sqrt(diag(vcovHC(m1, type = "HC1"))), 
       sqrt(diag(vcovHC(m2, type = "HC1", cluster = "group"))),
       sqrt(diag(vcovHC(m3, type = "HC1", cluster = "group"))))

stargazer(m1, m2, 
          type = "html",
          se = se_list,
          column.labels = c("Less", "More"),
          keep.stat = c("n","rsq","adj.rsq","f"),
          out = "output/estimates_20260225.html")
