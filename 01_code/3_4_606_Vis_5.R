# Visualize something for 2YP.

#  VPD (left) and firm-year-quarter weighted mean VPD over time (right). 

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
         .keep = "none") %>% 
  mutate(Year_Bin = 
           case_when(Year_MTBS %in% 1984:1999 ~ "1984-1999",
                     Year_MTBS %in% 2000:2019 ~ "2000-2019",
                     Year_MTBS %in% 2020:2025 ~ "2020-2025") %>% 
           factor %>% 
           fct_rev)

dat = 
  "03_intermediate/dat_notifications_1_8.csv" %>% 
  read_csv %>% 
  mutate(Restrict_MBF_Lower = MBF_2_DouglasFir > quantile(MBF_2_DouglasFir, 0.01),
         Restrict_MBF_Upper = MBF_2_DouglasFir < quantile(MBF_2_DouglasFir, 0.99),
         Restrict_Acres_Lower = Acres_1 > quantile(Acres_1, 0.01),
         Restrict_Acres_Upper = Acres_1 < quantile(Acres_1, 0.99),
         Restrict_MBFAcre_Lower = MBF_Acre_2_DouglasFir > quantile(MBF_Acre_2_DouglasFir, 0.01),
         Restrict_MBFAcre_Upper = MBF_Acre_2_DouglasFir < quantile(MBF_Acre_2_DouglasFir, 0.99)) %>% 
  filter(if_all(starts_with("Restrict"), ~ .x == TRUE)) %>% 
  group_by(Landowner) %>% 
  summarize(across(starts_with("Fire"), 
                   ~ weighted.mean(.x, na.rm = TRUE, w = MBF_2_DouglasFir))) %>% 
  ungroup %>% 
  select(Landowner, Fire_0, Fire_15, Fire_30) %>% 
  pivot_longer(cols = c(Fire_0, Fire_15, Fire_30))

# MTBS

vis_mtbs = 
  dat_mtbs %>% 
  arrange(desc(Year_MTBS)) %>% 
  ggplot() + 
  geom_spatvector(data = dat_bounds, fill = "NA", color = "black") +
  geom_spatvector(data = dat_mtbs,
                  aes(fill = Year_Bin),
                  color = NA,
                  alpha = 1.00) +
  labs(fill = NULL) +
  # scale_fill_distiller(palette = "Reds",
  #                      direction = 1,
  #                      limits = c(1984, 2024),
  #                      breaks = c(1984, 2024),
  #                      guide = guide_colorbar(title.position = "top"),
  #                      na.value = NA) +
  scale_fill_brewer(palette = "Reds", direction = -1) +
  theme_void() +
  theme(legend.position = "inside",
        legend.position.inside = c(-0.05, 0.50),
        # legend.direction = "vertical",
        legend.key.height = unit(5.00, "lines"),
        legend.key.width = unit(0.50, "lines"),
        # legend.key = element_rect(fill = NA, color = "black"),
        # legend.text = element_text(size = 8),
        legend.text = element_text(angle = 90, hjust = 0.50),
        legend.text.position = "left")

# Exposure

pal_exposure = RColorBrewer::brewer.pal(9, "Reds")[7]

vis_exposure = 
  dat %>%
  filter(value > 0) %>%
  mutate(value = value %>% log) %>% 
  # mutate(name = name %>% str_split_i("_", 2) %>% factor) %>% 
  mutate(name = 
           case_when(name == "Fire_0" ~ "0",
                     name == "Fire_15" ~ "(0, 15]",
                     name == "Fire_30" ~ "(15, 30]") %>% 
           factor %>% 
           fct_relevel("0", "(0, 15]", "(15, 30]")) %>% 
  ggplot() +
  geom_density_ridges(aes(x = value,
                          y = name),
                          # fill = name),
                      # stat = "binline",
                      # bins = 26,
                      fill = NA,
                      scale = 0.95) +
  labs(x = "Mean Log(Count)",
       y = "Kilometers to Wildfire") +
  scale_y_discrete(position = "right") +
  # scale_fill_brewer(palette = "Reds") +
  theme_minimal() + 
  theme(legend.position = "none") 

# Combine/Export

vis = free(vis_mtbs) + vis_exposure + plot_layout(widths = c(2, 2))

ggsave("04_out/vis_4_20260428.png",
       vis,
       dpi = 300,
       height = 4.5,
       width = 6.5)
