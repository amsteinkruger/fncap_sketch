# Visualize something for 2YP.

dat_bounds = 
  "03_intermediate/dat_bounds.gdb" %>% 
  vect

dat = 
  "03_intermediate/dat_notifications_1_8.gdb" %>% 
  vect %>% 
  mutate(Year = DateStart %>% year,
         Month = DateStart %>% month,
         Quarter = Month %>% multiply_by(1 / 3) %>% ceiling,
         Year_Quarter = paste0(Year, "_Q", Quarter)) %>% 
  mutate(Restrict_MBF_Lower = MBF_2_DouglasFir > quantile(MBF_2_DouglasFir, 0.01),
         Restrict_MBF_Upper = MBF_2_DouglasFir < quantile(MBF_2_DouglasFir, 0.99),
         Restrict_Acres_Lower = Acres_1 > quantile(Acres_1, 0.01),
         Restrict_Acres_Upper = Acres_1 < quantile(Acres_1, 0.99),
         Restrict_MBFAcre_Lower = MBF_Acre_2_DouglasFir > quantile(MBF_Acre_2_DouglasFir, 0.01),
         Restrict_MBFAcre_Upper = MBF_Acre_2_DouglasFir < quantile(MBF_Acre_2_DouglasFir, 0.99)) %>% 
  filter(if_all(starts_with("Restrict"), ~ .x == TRUE)) %>% 
  select(Year, MBF_2_DouglasFir, MBF_Acre_2_DouglasFir, Landowner) %>% 
  mutate(Landowner = Landowner %>% factor %>% as.numeric)
  

# Figure 1: production, yield, firms in space

# Geodata

#  Raster

dat_rast = dat %>% rast(ncol = 50, nrow = 100)

#  Allocate lists for each metric. Remember that iteration can be messy.

dat_list_production <- list()
dat_list_yield <- list()
dat_list_companies <- list()

# Production

for (i in 1:nrow(dat)) {
  # Extract polygon i. 
  polygon <- dat[i, ]
  
  # Assign 1 to cells in polygons.
  r <- rasterize(polygon, dat_rast, field = "MBF_2_DouglasFir", background = NA, touches = TRUE)
  
  # Name layers.
  names(r) <- paste0("polygon_", i)
  
  # Iterate into a pre-allocated list. 
  dat_list_production[[i]] <- r
}

dat_raster_production <- rast(dat_list_production)

dat_raster_production_sum <- app(dat_raster_production, fun = sum, na.rm = TRUE)

dat_raster_production_sum_mean = dat_raster_production_sum / 40

# Yield

for (i in 1:nrow(dat)) {
  # Extract polygon i. 
  polygon <- dat[i, ]
  
  # Assign 1 to cells in polygons.
  r <- rasterize(polygon, dat_rast, field = "MBF_Acre_2_DouglasFir", background = NA, touches = TRUE)
  
  # Name layers.
  names(r) <- paste0("polygon_", i)
  
  # Iterate into a pre-allocated list. 
  dat_list_yield[[i]] <- r
}

dat_raster_yield <- rast(dat_list_yield)

dat_raster_yield_mean <- app(dat_raster_yield, fun = mean, na.rm = TRUE)

# Company Counts

for (i in 1:nrow(dat)) {
  # Extract polygon i. 
  polygon <- dat[i, ]
  
  # Assign 1 to cells in polygons.
  r <- rasterize(polygon, dat_rast, field = "Landowner", background = NA, touches = TRUE)
  
  # Name layers.
  names(r) <- paste0("polygon_", i)
  
  # Iterate into a pre-allocated list. 
  dat_list_companies[[i]] <- r
}

dat_raster_companies <- rast(dat_list_companies)

dat_raster_companies_count <- app(dat_raster_companies, fun = function(x){length(unique(x))}) # function(x) length(unique(as.character(x), na.rm = TRUE))

dat_raster_companies_count = dat_raster_companies_count - 1
dat_raster_companies_count = dat_raster_companies_count %>% subst(0, NA)

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
  # scale_fill_viridis(option = "D", 
  #                    limits = c(0, NA),
  #                    # breaks = c(0, 5),
  #                    guide = guide_colorbar(title.position = "top"),
  #                    na.value = NA) +
  labs(fill = "Mean Annual MMBF") +
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
  labs(fill = "Mean Yield (MBF/Acre)") +
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
                       breaks = c(0, 13),
                       guide = guide_colorbar(title.position = "top"),
                       na.value = NA) +
  labs(fill = "Total Active Firms") +
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

vis_1 = vis_production + vis_yield + vis_companies

ggsave("04_out/vis_1_20260428.png",
       vis_1,
       dpi = 300,
       width = 6.5,
       height = 4.5)
