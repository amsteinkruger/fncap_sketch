# Estimate something for 2YP.

# Out:
#  Maps of production, yield, owners, concentration
#  Columns of the same.
#  Table of sample restriction impacts.
#  Table of results for vars on firm supply.

# Packages

library(tidyverse)
library(terra)
library(tidyterra)
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
  mutate(MBF_Prime = MBF * 1.05) %>% 
  # Annual HHI
  group_by(Company) %>% 
  mutate(Concentration_)

# note spatial HHI problem

# Geodata

dat_polygons = 
  "output/dat_notifications_more_polygons.gdb" %>% 
  vect

# Set up for rasterization.

#  Vectors

dat_join = 
  dat_polygons %>% 
  semi_join(dat %>% select(UID)) %>% 
  left_join(dat %>% select(UID, Year = Year_Start, MBF, MBF_Acre, Company))

#  Raster

dat_rast = dat_join %>% rast(ncol = 50, nrow = 100)

#  Allocate lists for each metric. Remember that iteration can be messy.

dat_list_production <- list()
dat_list_yield <- list()
dat_list_companies <- list()
dat_list_hhi <- list()

# Production

for (i in 1:nrow(dat_join)) {
  # Extract the i-th polygon
  single_poly <- dat_join[i, ]
  
  # Rasterize: assign value 1 to polygon cells, NA elsewhere
  r <- rasterize(single_poly, dat_rast, field = "MBF", background = NA, touches = TRUE)
  
  # Optionally name the layer
  names(r) <- paste0("poly_", i)
  
  # Store in list
  dat_list_production[[i]] <- r
}

dat_raster_production <- rast(dat_list_production)

dat_raster_production_sum <- app(dat_raster_production, fun = sum, na.rm = TRUE)

dat_raster_production_sum_mean = dat_raster_production_sum / 10

# Yield

for (i in 1:nrow(dat_join)) {
  # Extract the i-th polygon
  single_poly <- dat_join[i, ]
  
  # Rasterize: assign value 1 to polygon cells, NA elsewhere
  r <- rasterize(single_poly, dat_rast, field = "MBF_Acre", background = NA, touches = TRUE)
  
  # Optionally name the layer
  names(r) <- paste0("poly_", i)
  
  # Store in list
  dat_list_yield[[i]] <- r
}

dat_raster_yield <- rast(dat_list_yield)

dat_raster_yield_mean <- app(dat_raster_yield, fun = mean, na.rm = TRUE)

# Company Counts

for (i in 1:nrow(dat_join)) {
  # Extract the i-th polygon
  single_poly <- dat_join[i, ]
  
  # Rasterize: assign value 1 to polygon cells, NA elsewhere
  r <- rasterize(single_poly, dat_rast, field = "Company", background = NA, touches = TRUE)
  
  # Optionally name the layer
  names(r) <- paste0("poly_", i)
  
  # Store in list
  dat_list_companies[[i]] <- r
}

dat_raster_companies <- rast(dat_list_yield)

dat_raster_companies_count <- app(dat_raster_yield, fun = function(x) {length(unique(x, na.rm = TRUE))})

dat_raster_companies_count = dat_raster_companies_count - 1
dat_raster_companies_count = dat_raster_companies_count %>% subst(0, NA)

# Company HHI


# Plots

vis_production = 
  ggplot() +
  geom_spatraster(data = dat_raster_production_sum_mean / 1000) +
  geom_spatvector(data = dat_bounds, fill = "NA", color = "black") +
  scale_fill_distiller(palette = "Greens",
                       direction = 1,
                       limits = c(0, NA),
                       breaks = c(0, 5),
                       guide = guide_colorbar(title.position = "top"),
                       na.value = NA) +
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

vis_yield = 
  ggplot() +
  geom_spatvector(data = dat_bounds, fill = "white", color = "black") +
  geom_spatraster(data = dat_raster_yield_mean) +
  scale_fill_distiller(palette = "Blues",
                       direction = 1,
                       limits = c(0, 60),
                       breaks = c(0, 60),
                       guide = guide_colorbar(title.position = "top"),
                       na.value = NA) +
  labs(fill = "Mean Yield (Thousands of Board Feet per Acre)") +
  theme_void() +
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        legend.ticks = element_blank(),
        legend.key.height = unit(0.25, "lines"),
        legend.key.width = unit(1.5, "lines"),
        legend.key = element_rect(fill = NA, color = "black"),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 9, hjust = 0.5))

vis_companies = 
  ggplot() +
  geom_spatvector(data = dat_bounds, fill = "white", color = "black") +
  geom_spatraster(data = dat_raster_companies_count) +
  scale_fill_distiller(palette = "Oranges",
                       direction = 1,
                       limits = c(0, NA),
                       breaks = c(0, 25),
                       guide = guide_colorbar(title.position = "top"),
                       na.value = NA) +
  labs(fill = "Active Firms, 2015-2024") +
  theme_void() +
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        legend.ticks = element_blank(),
        legend.key.height = unit(0.25, "lines"),
        legend.key.width = unit(1.5, "lines"),
        legend.key = element_rect(fill = NA, color = "black"),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 9, hjust = 0.5))

# Combine/Export

vis_space = vis_production + vis_yield + vis_companies

ggsave("output/presentation_vis_space_20260310.png",
       vis_space,
       dpi = 300,
       width = 9,
       height = 6)

# Time Series

pal_time_production = RColorBrewer::brewer.pal(9, "Greens")[7]
pal_time_yield = RColorBrewer::brewer.pal(9, "Blues")[7]
pal_time_companies = RColorBrewer::brewer.pal(9, "Oranges")[7]

#  Production

vis_time_production = 
  dat_join %>% 
  group_by(Year) %>% 
  summarize(Production = MBF %>% sum(na.rm = TRUE) %>% `/` (1000)) %>% 
  ungroup %>% 
  ggplot() +
  geom_col(aes(x = Year %>% factor,
               y = Production),
           fill = pal_time_production,
           color = NA) +
  labs(x = NULL, y = "Board Feet (Millions)") +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#  Yield

vis_time_yield = 
  dat_join %>% 
  # group_by(Year) %>% 
  # summarize(Yield = MBF_Acre %>% mean(na.rm = TRUE)) %>% 
  # ungroup %>% 
  ggplot() +
  geom_boxplot(aes(x = Year %>% factor,
               y = MBF_Acre),
               outlier.shape = 21,
               color = pal_time_yield,
               fill = "white") + 
  labs(x = NULL, y = "Thousands of Board Feet per Acre") +
  scale_x_discrete(expand = c(0, 0)) +
  # scale_y_continuous(expand = c(0, 0)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

vis_time_companies = 
  dat_join %>% 
  group_by(Year) %>% 
  summarize(Companies = n_distinct(Company)) %>% 
  ungroup %>% 
  ggplot() +
  geom_col(aes(x = Year %>% factor,
               y = Companies),
           fill = pal_time_companies,
           color = NA) +
  labs(x = NULL, y = "Harvesting Timberland Owners") +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Combine/Export

vis_time = vis_time_production + vis_time_yield + vis_time_companies

ggsave("output/presentation_vis_time_20260310.png",
       vis_time,
       dpi = 300,
       height = 3,
       width = 9)

# Restrictions Table

dat_restrictions = 
  "output/dat_notifications_more_annual.csv" %>% 
  read_csv %>% 
  mutate(MBF_Acre = MBF / Acres) %>% 
  mutate(Restriction_Intersects = (Intersect_Maximum < 0.01),
         Restriction_DouglasFir = (ProportionDouglasFir > 0.50),
         Restriction_NDVI = (NDVI_Detect == 1),
         Restriction_Perc = (MBF < quantile(MBF, 0.99) & MBF > quantile(MBF, 0.01)) & (Acres < quantile(Acres, 0.99) & Acres > quantile(Acres, 0.01)) & (MBF_Acre < quantile(MBF_Acre, 0.99) & MBF_Acre > quantile(MBF_Acre, 0.01)),
         Restriction_All = (Restriction_Intersects == TRUE & Restriction_DouglasFir == TRUE & Restriction_NDVI == TRUE & Restriction_Perc == TRUE)) %>% 
  select(starts_with("Restriction")) %>% 
  dplyr::summarize(across(everything(), ~ sum(.x, na.rm = TRUE))) %>% 
  pivot_longer(everything()) %>% 
  rename(Restriction = name, Keep = value) %>% 
  mutate(Drop = 19612 - Keep)

write_csv(dat_restrictions, "output/presentation_tab_restrictions.csv")

# Summary Table

# dat_summary = 
#   dat %>% 
#   select(Acres = Acres,
#          MBF,
#          MBF_Prime,
#          Price = Stumpage_Real_PPI_Timber,
#          Rate = Fed_Rate,
#          Rents = Acres) %>% 
#   mutate(Growth = MBF_Prime - MBF,
#          Yield = MBF / Acres) %>% 
#   select(-MBF_Prime) %>% 
#   summarize(across(everything(), c(mean, sd))) %>% 
#   pivot_longer(everything()) %>% 
#   mutate(which = name %>% str_sub(-2, -1),
#          name = name %>% str_sub(1, -3),
#          value = value %>% round(3)) %>% 
#   pivot_wider(names_from = which, 
#               values_from = value) %>% 
#   rename(Variable = 1,
#          Mean = 2,
#          SD = 3)
# 
# write_csv(dat_summary, "output/paper_tab_summary.csv")

# Statistics?

dat_use = 
  dat %>% 
  group_by(Year_Start, Company) %>% 
  summarize(MBF = sum(MBF),
            Acres = sum(Acres), 
            Yield = mean(MBF / Acres),
            VPD = mean(VPD),
            Price = mean(Stumpage_Real_PPI_Timber),
            Interest = mean(Fed_Rate)) %>% 
  ungroup %>% 
  rename(Year = Year_Start) %>% 
  arrange(Company, Year) %>% 
  group_by(Company) %>% 
  mutate(MBF_Lag = dplyr::lag(MBF)) %>% 
  ungroup %>% 
  mutate(MMBF = MBF / 1000,
         MMBF_Lag = MBF_Lag / 1000) %>% 
  mutate(Interest_Integer = Interest * 100)

#  Threshold, no covariates

mod_1 = 
  dat_use %>% 
  lm(MMBF ~ MMBF_Lag, 
     data = .)

#  Threshold, covariates

mod_2 = 
  dat_use %>% 
  lm(MMBF ~ 
       MMBF_Lag +
       VPD +
       Price +
       Interest, 
     data = .)

#  Threshold, fixed effects

mod_3 = 
  dat_use %>% 
  plm(MMBF ~ 
        MMBF_Lag +
        VPD +
        Price +
        Interest, 
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
          column.labels = c("Lag Production Only", "Lag Production and Covariates", "Lag Production, Covariates, TWFE"),
          keep.stat = c("n", "rsq", "adj.rsq", "f"),
          out = "output/presentation_estimates_20260310.html")
