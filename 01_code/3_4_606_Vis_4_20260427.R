# Visualize something for 2YP.

#  MTBS (left) and firm fire exposure as stacked density plots (right)

dat_bounds = 
  "03_intermediate/dat_bounds.gdb" %>% 
  vect

dat_mtbs = 
  "02_data/1_6_2_MTBS/Perimeters" %>% 
  vect %>% 
  project("EPSG:2992") %>% 
  makeValid %>% 
  crop(dat_bounds) %>% 
  mutate(Year_MTBS = ig_date %>% year, 
         .keep = "none")

dat = 
  "03_intermediate/dat_notifications_1_6.csv" %>% 
  read_csv %>% 
  mutate(Year = DateStart %>% year,
         Month = DateStart %>% month,
         Quarter = Month %>% multiply_by(1 / 3) %>% ceiling,
         Year_Quarter = paste0(Year, "_Q", Quarter)) %>% 
  filter(DateStart %>% year > 2014 & DateEnd %>% year < 2025) %>% 
  filter(ProportionDouglasFir > 0.50) %>% 
  filter(MBF > quantile(MBF, 0.01) & MBF < quantile(MBF, 0.99) &
           Acres > quantile(Acres, 0.01) & Acres < quantile(Acres, 0.99) &
           MBF_Acre > quantile(MBF_Acre, 0.01) & MBF_Acre < quantile(MBF_Acre, 0.99)) %>% 
  group_by(Landowner) %>% 
  summarize(Fire_0 = Fire_0 %>% weighted.mean(na.rm = TRUE, w = MBF),
            Fire_15 = Fire_15 %>% weighted.mean(na.rm = TRUE, w = MBF),
            Fire_30 = Fire_30 %>% weighted.mean(na.rm = TRUE, w = MBF)) %>% 
  ungroup %>% 
  pivot_longer(cols = c(Fire_0, Fire_15, Fire_30))
  
# MTBS

vis_mtbs = 
  dat_mtbs %>% 
  arrange(desc(Year_MTBS)) %>% 
  ggplot() + 
  geom_spatvector(data = dat_bounds, fill = "NA", color = "black") +
  geom_spatvector(data = dat_mtbs,
                  aes(fill = Year_MTBS),
                  color = "black",
                  alpha = 1.00) +
  labs(fill = NULL) +
  scale_fill_distiller(palette = "Reds",
                       direction = 1,
                       limits = c(1984, 2024),
                       breaks = c(1984, 2024),
                       guide = guide_colorbar(title.position = "top"),
                       na.value = NA) +
  theme_void() +
  theme(legend.position = "top",
        legend.direction = "horizontal",
        legend.ticks = element_blank(),
        legend.key.height = unit(0.25, "lines"),
        legend.key.width = unit(1.5, "lines"),
        legend.key = element_rect(fill = NA, color = "black"),
        legend.text = element_text(size = 8),
        legend.text.position = "top")
  
# Exposure

vis_exposure = 
  dat %>% 
  filter(value > 0) %>% 
  mutate(value = value %>% log) %>% 
  ggplot() +
  geom_density_ridges(aes(x = value,
                           y = name,
                          fill = name),
                      # stat = "binline",
                      # bins = 26,
                      scale = 0.95) +
  labs(x = "Mean Log(Count) by Landowner",
       y = NULL) +
  scale_y_discrete(position = "right") +
  scale_fill_brewer(palette = "Reds") +
  theme_minimal() + 
  theme(legend.position = "none") 

# Combine/Export

vis = vis_mtbs + vis_exposure + plot_layout(widths = c(3, 2))

ggsave("04_out/vis_4_20260428.png",
       vis,
       dpi = 300,
       height = 4.0,
       width = 6.0)
