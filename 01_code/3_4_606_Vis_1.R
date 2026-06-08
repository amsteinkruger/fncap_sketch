# Visualize the study region.

#  Palette

library(RColorBrewer)

pal_fire = brewer.pal(9, "Reds")[c(4, 6, 8)] %>% rev
pal_timber = brewer.pal(9, "Greens")[8]
pal_both = c(pal_fire, pal_timber)

#  Data

#   Region

dat_bounds = 
  "03_intermediate/dat_bounds.gdb" %>% 
  vect

#   States

dat_states = 
  "02_data/1_2_1_Census_States" %>% 
  vect %>% 
  # filter(STUSPS == "OR") %>% 
  # select(STUSPS) %>% 
  project("EPSG:2992")

#   Pyromes/Ecoregions

dat_pyromes = 
  "02_data/1_2_2_USFS_Pyromes/Data/Pyromes_CONUS_20200206.shp" %>% 
  vect %>% 
  rename(WHICH = NAME) %>% # Band-Aid for a reserved attribute name.
  filter(WHICH %in% c("Marine Northwest Coast Forest", "Klamath Mountains", "Middle Cascades")) %>% 
  select(Pyrome = WHICH) %>% 
  project("EPSG:2992") %>% 
  crop(dat_bounds)

#   Places

#   Private Forest/Timberland

dat_owner = 
  "02_data/3_4_ODF_Ownership/Ownership.gdb" %>% 
  vect %>% 
  select(Owner = LandManager) %>% 
  project("EPSG:2992") %>% 
  crop(dat_bounds) %>% 
  group_by(Owner) %>% 
  summarize %>% 
  ungroup %>% 
  mutate(Fill = "Private\nTimberland")

#   Fires

dat_mtbs = 
  "02_data/1_7_1_MTBS/Perimeters" %>% 
  vect %>% 
  project("EPSG:2992") %>% 
  makeValid %>% 
  crop(dat_bounds) %>% 
  mutate(Year_MTBS = ig_date %>% year, 
         .keep = "none") %>% 
  mutate(Fill = 
           case_when(Year_MTBS %in% 1984:1999 ~ "Wildfire,\n1984-1999",
                     Year_MTBS %in% 2000:2019 ~ "Wildfire,\n2000-2019",
                     Year_MTBS %in% 2020:2025 ~ "Wildfire,\n2020-2025") %>% 
           factor %>% 
           fct_rev)

#    Intersection

# dat_both = intersect(dat_owner, dat_mtbs)

#  Visualizations

#   Main Plot

vis_main = 
  ggplot() + 
  geom_spatvector(data = dat_bounds,
                  fill = NA, 
                  color = "black") + 
  geom_spatvector(data = dat_owner, 
                  aes(fill = Fill),
                  color = NA, 
                  alpha = 1.00) +
  geom_spatvector(data = dat_mtbs, 
                  aes(fill = Fill),
                  color = NA, 
                  alpha = 0.75) +
  scale_fill_manual(values = pal_both) +
  theme_void() +
  theme(legend.position = "inside",
        legend.position.inside = c(-0.05, 0.50),
        # legend.direction = "vertical",
        legend.key.height = unit(5.00, "lines"),
        legend.key.width = unit(0.50, "lines"),
        # legend.key = element_rect(fill = NA, color = "black"),
        # legend.text = element_text(size = 8),
        legend.text = 
          element_text(angle = 90,
                       hjust = 0.00,
                       margin = margin(t = 0, r = 0, b = 0.50, l = 0.50, unit = "lines")),
        legend.text.position = "left",
        legend.title = element_blank())

ggsave("04_out/Presentation/vis_1_map.png",
       vis_main,
       dpi = 300,
       height = 4.5,
       width = 6.5)
