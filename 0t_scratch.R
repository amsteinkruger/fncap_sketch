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
library(viridis)
library(patchwork)
library(plm)
library(stargazer)
library(sandwich)

# Data

dat = 
  "output/dat_notifications_more_annual.csv" %>% 
  read_csv %>% 
  mutate(Restriction_Intersects = (Intersect_Maximum < 0.01),
         Restriction_DouglasFir = (ProportionDouglasFir > 0.50),
         Restriction_NDVI = (NDVI_Detect == 1),
         Restriction_Any = (Restriction_Intersects == TRUE | Restriction_DouglasFir == TRUE | Restriction_NDVI == TRUE),
         Restriction_All = (Restriction_Intersects == TRUE & Restriction_DouglasFir == TRUE & Restriction_NDVI == TRUE)) %>% 
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
         Stumpage_Real_PPI_Timber,
         Fed_Rate) %>% 
  mutate(MBF_Acre = MBF / Acres) %>% 
  filter(MBF < quantile(MBF, 0.99) & MBF > quantile(MBF, 0.01),
         Acres < quantile(Acres, 0.99) & Acres > quantile(Acres, 0.01),
         MBF_Acre < quantile(MBF_Acre, 0.99) & MBF_Acre > quantile(MBF_Acre, 0.01)) %>% 
  # mutate(MBF_Prime = par_growth * Acres + MBF) # This really should work, but it doesn't.
  mutate(MBF_Prime = MBF * 1.05)

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
  crop(dat_bounds) %>% 
  drop_na

vis_counties_mbf =
  dat_counties %>% 
  ggplot() +
  geom_spatvector(aes(fill = MBF / 1000),
                  color = "black",
                  linewidth = 0.20) +
  # scale_fill_viridis(option = "E",
  #                    limits = c(0, NA),
  #                    breaks = c(0, 100, 200),
  #                    guide = guide_colorbar(title.position = "top")) +
  scale_fill_distiller(palette = "Greens",
                       direction = 1,
                       limits = c(0, NA),
                       breaks = c(0, 200),
                       guide = guide_colorbar(title.position = "top")) +
  labs(fill = "Mean Annual Board Feet (Millions)") +
  theme_void() +
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        legend.ticks = element_blank(),
        legend.key.height = unit(0.25, "lines"),
        legend.key.width = unit(1.5, "lines"),
        legend.key = element_rect(fill = NA, color = "black"),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 9, hjust = 0.5))


vis_counties_firms =
  dat_counties %>% 
  ggplot() +
  geom_spatvector(aes(fill = Firms),
                  color = "black",
                  linewidth = 0.20) +
  # scale_fill_viridis(option = "G",
  #                    limits = c(0, NA),
  #                    # breaks = c(0, 40, 80),
  #                    guide = guide_colorbar(title.position = "top")) +
  scale_fill_distiller(palette = "Blues",
                       direction = 1,
                       limits = c(0, NA),
                       breaks = c(0, 25),
                       guide = guide_colorbar(title.position = "top")) +
  labs(fill = "Mean Annual Timberland Owners") +
  theme_void() +
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        legend.ticks = element_blank(),
        legend.key.height = unit(0.25, "lines"),
        legend.key.width = unit(1.5, "lines"),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 9, hjust = 0.5))

vis_counties = vis_counties_mbf + vis_counties_firms

ggsave("output/paper_vis_counties.png",
       vis_counties,
       dpi = 300,
       height = 4.33)

#  - plots of MBF and firms in time

pal_time_mbf = RColorBrewer::brewer.pal(9, "Greens")[8]
pal_time_firms = RColorBrewer::brewer.pal(9, "Blues")[8]

dat_time = 
  dat %>% 
  group_by(Year_Start) %>% 
  summarize(MBF = sum(MBF) / 1000,
            Firms = n_distinct(Company)) %>% 
  ungroup

vis_time_mbf = 
  dat_time %>% 
  ggplot() +
  geom_col(aes(x = Year_Start %>% factor,
               y = MBF),
           fill = pal_time_mbf,
           color = NA) +
  labs(x = NULL, y = "Board Feet (Millions)") +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

vis_time_firms = 
  dat_time %>% 
  ggplot() +
  geom_col(aes(x = Year_Start %>% factor,
               y = Firms),
           fill = pal_time_firms,
           color = NA) +
  labs(x = NULL, y = "Timberland Owners") +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

vis_time = vis_time_mbf + vis_time_firms

ggsave("output/paper_vis_time.png",
       vis_time,
       dpi = 300,
       width = 6.50,
       height = 3.25)

#  - table of sample restriction impacts

dat_restrictions = 
  "output/dat_notifications_more_annual.csv" %>% 
  read_csv %>% 
  mutate(Restriction_Intersects = (Intersect_Maximum < 0.01),
         Restriction_DouglasFir = (ProportionDouglasFir > 0.50),
         Restriction_NDVI = (NDVI_Detect == 1),
         Restriction_All = (Restriction_Intersects == TRUE & Restriction_DouglasFir == TRUE & Restriction_NDVI == TRUE)) %>% 
  select(starts_with("Restriction")) %>% 
  dplyr::summarize(across(everything(), ~ sum(.x, na.rm = TRUE))) %>% 
  pivot_longer(everything()) %>% 
  rename(Restriction = name, Keep = value) %>% 
  mutate(Drop = 19612 - Keep)

write_csv(dat_restrictions, "output/paper_tab_restrictions.csv")

#  - table of summary statistics by model variable

dat_summary = 
  dat %>% 
  select(MBF,
         MBF_Prime,
         Price = Stumpage_Real_PPI_Timber,
         Rate = Fed_Rate) %>% 
  mutate(Growth = MBF_Prime - MBF) %>% 
  select(-MBF_Prime) %>% 
  summarize(across(everything(), c(mean, sd))) %>% 
  pivot_longer(everything()) %>% 
  mutate(which = name %>% str_sub(-2, -1),
         name = name %>% str_sub(1, -3),
         value = value %>% round(3)) %>% 
  pivot_wider(names_from = which, 
              values_from = value) %>% 
  rename(Variable = 1,
         Mean = 2,
         SD = 3)

write_csv(dat_summary, "output/paper_tab_summary.csv")

#  - plot of condition balance around 0 (decision framework)?

pal_balance = c(RColorBrewer::brewer.pal(9, "Greens")[4], RColorBrewer::brewer.pal(9, "Greens")[8])

dat_price_stumpage_less = dat_price_stumpage %>% select(Year, Price = Stumpage_Real_PPI_Timber)
dat_join_rate_less = dat_join_rate

lead = dplyr::lead

dat_augment = 
  dat %>% 
  select(UID, Year_Start, Company, MBF, Acres, Pyrome, Elevation, Slope, Roughness, VPD, ProportionDouglasFir, Distance_Mill) %>% 
  mutate(MBF = map(MBF, ~ c(.x / 1.05, .x, .x * 1.05)),
         Year = map(Year_Start, ~ c(.x - 1, .x, .x + 1))) %>% 
  unnest(c(MBF, Year)) %>% 
  left_join(dat_price_stumpage_less) %>% 
  left_join(dat_join_rate_less) %>% 
  drop_na %>% 
  group_by(UID) %>% 
  mutate(MBF_Lead = MBF %>% lead,
         Price_Lead = Price %>% lead,
         Fed_Rate_Lead = Fed_Rate %>% lead) %>% 
  drop_na %>% 
  mutate(Harvest = ifelse(Year == max(Year), 1, 0)) %>% 
  ungroup %>% 
  mutate(Condition = (Price_Lead - Price) * MBF + Price * (MBF_Lead - MBF) - Fed_Rate * Price * MBF - Acres,
         Condition_Round = Condition %>% round(0),
         Condition_Thousands = Condition_Round / 1000,
         Condition_Binary = ifelse(Condition_Round > 0, 1, 0) %>% factor,
         Harvest_Factor = Harvest %>% factor,
         Yield = MBF / Acres) %>% 
  group_by(Company) %>% 
  mutate(Scale = sum(MBF) / n_distinct(Year)) %>% 
  ungroup %>% 
  mutate(Bucket = Condition_Thousands %>% cut_width(50, boundary = 0))

vis_balance = 
  dat_augment %>% 
  group_by(Harvest_Factor, Bucket) %>% 
  summarize(Count = n()) %>% 
  ungroup %>% 
  mutate(Harvest_Names = Harvest_Factor %>% fct_recode("No Harvest" = "0", "Harvest" = "1")) %>% 
  ggplot() +
  geom_col(aes(x = Bucket,
               y = Count,
               fill = Harvest_Names),
           position = position_dodge2(preserve = "single")) +
  labs(x = "Distance from Harvest Threshold (2025 USD, Thousands)",
       y = "Notifications",
       fill = "Harvest") +
  scale_fill_manual(values = pal_balance) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.title = element_blank())

ggsave("output/paper_vis_balance.png",
       vis_balance,
       dpi = 300,
       width = 6.50,
       height = 4.33)

# Harvest Models

#  Threshold, no covariates

mod_1 = 
  dat_augment %>% 
  lm(Harvest ~ Condition_Thousands, 
     data = .)

#  Threshold, covariates

mod_2 = 
  dat_augment %>% 
  lm(Harvest ~ 
       Condition_Thousands + 
       Yield +
       Acres,
     data = .)

#  Threshold, fixed effects

mod_3 = 
  dat_augment %>% 
  plm(Harvest ~ 
        Condition_Thousands + 
        Yield + 
        Acres, 
      data = .,
      index = c("Company", "Year"), 
      model = "within")

se_list <- 
  list(sqrt(diag(vcovHC(mod_1, type = "HC1"))), 
       sqrt(diag(vcovHC(mod_2, type = "HC1"))),
       sqrt(diag(vcovHC(mod_3, type = "HC1", cluster = "group"))))

stargazer(mod_1, mod_2, mod_3, 
          type = "html",
          se = se_list,
          column.labels = c("Yield Only", "Yield, Covariates", "Yield, Covariates, FE"),
          keep.stat = c("n", "rsq", "adj.rsq", "f"),
          out = "output/estimates_20260228.html")
