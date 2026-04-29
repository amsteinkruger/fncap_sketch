# Visualize something for 2YP.

dat_bounds = 
  "03_intermediate/dat_bounds.gdb" %>% 
  vect

dat = 
  "03_intermediate/dat_notifications_1_6.gdb" %>% 
  vect %>% 
  mutate(Year = DateStart %>% year,
         Month = DateStart %>% month,
         Quarter = Month %>% multiply_by(1 / 3) %>% ceiling,
         Year_Quarter = paste0(Year, "_Q", Quarter)) %>% 
  filter(DateStart %>% year > 2014 & DateEnd %>% year < 2025) %>% 
  filter(ProportionDouglasFir > 0.50) %>% 
  filter(MBF > quantile(MBF, 0.01) & MBF < quantile(MBF, 0.99) &
           Acres > quantile(Acres, 0.01) & Acres < quantile(Acres, 0.99) &
           MBF_Acre > quantile(MBF_Acre, 0.01) & MBF_Acre < quantile(MBF_Acre, 0.99)) %>% 
  select(Year, MBF, MBF_Acre, Landowner) %>% 
  mutate(Landowner = Landowner %>% factor %>% as.numeric)
  

# Figure 1: production, yield, firms in space

# Geodata

# dat_polygons = 
#   "output/dat_notifications_more_polygons.gdb" %>% 
#   vect

# Set up for rasterization.

#  Vectors

# dat_join = 
#   dat_polygons %>% 
#   semi_join(dat %>% select(UID)) %>% 
#   left_join(dat %>% select(UID, Year = Year_Start, MBF, MBF_Acre, Company))

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
  r <- rasterize(polygon, dat_rast, field = "MBF", background = NA, touches = TRUE)
  
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
  r <- rasterize(polygon, dat_rast, field = "MBF_Acre", background = NA, touches = TRUE)
  
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

# Figure 2: production by firm, yield by firm, firm counts in time



# Figure 3: stumpage and federal funds rate in time



# Figure 4: wildfires in space and firm exposure to wildfires in time



# Figure 5: VPD in space and firm exposure to VPD in time



# Figure 6: MBF ~ Price, MBF ~ VPD



# Figure 7: MBF ~ Wildfires * 3



