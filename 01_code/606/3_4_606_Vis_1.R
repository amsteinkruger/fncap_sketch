# Visualize the study region.

#  Palette

library(RColorBrewer)

pal_fire = brewer.pal(9, "Reds")[c(4, 6, 8)] %>% rev
pal_timber = brewer.pal(9, "Greens")[8]
pal_both = c(pal_fire, pal_timber)

pal_inset = brewer.pal(9, "Blues")[8]

#  Data

#   Region

dat_bounds = 
  "03_intermediate/dat_bounds.gdb" %>% 
  vect

#   Bounding Box

dat_box = 
  dat_bounds %>% 
  buffer(50000) %>% 
  ext 

dat_box_vect = 
  dat_box %>% 
  as.polygons

crs(dat_box_vect) <- "EPSG:2992"

#   States

dat_states = 
  "02_data/1_2_1_Census_States" %>% 
  vect %>% 
  filter(NAME %!in% 
           c("Alaska",
             "American Samoa",
             "Commonwealth of the Northern Mariana Islands",
             "Guam",
             "Hawaii",
             "Puerto Rico",
             "United States Virgin Islands")) %>% 
  # filter(STUSPS == "OR") %>% 
  # select(STUSPS) %>% 
  project("EPSG:2992")

dat_states_inset = dat_states %>% project("EPSG:5070")

dat_box_vect_inset = dat_box_vect %>% project("EPSG:5070")

dat_states_box = dat_states %>% crop(dat_box)

dat_box_states =
  dat_states_inset %>% 
  aggregate %>% 
  buffer(250000) %>% 
  ext

dat_box_states_vect =
  dat_box_states %>% 
  as.polygons

crs(dat_box_states_vect) <- "EPSG:5070"

#   Oregon

dat_oregon = dat_states %>% filter(NAME == "Oregon")

dat_box_oregon = 
  dat_oregon %>% 
  buffer(100000) %>% 
  ext 

dat_box_oregon_vect = 
  dat_box_oregon %>% 
  as.polygons

crs(dat_box_oregon_vect) <- "EPSG:2992"

dat_states_oregon = dat_states %>% crop(dat_box_oregon)

dat_oregon_inset = dat_states_inset %>% filter(NAME == "Oregon")

dat_oregon_box = dat_oregon %>% crop(dat_box)

#   Ocean

dat_ocean_box = dat_box_vect %>% erase(dat_states)

dat_ocean_oregon = dat_box_oregon_vect %>% erase(dat_states)

dat_ocean_states = dat_box_states_vect %>% erase(dat_states_inset)

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
  geom_spatvector(data = dat_ocean_box,
                  fill = "grey80",
                  color = "black") +
  geom_spatvector(data = dat_states_box,
                  fill = "grey90",
                  color = "black") +
  geom_spatvector(data = dat_oregon_box,
                  fill = "grey95",
                  color = "black") +
  geom_spatvector(data = dat_bounds,
                  fill = "grey100",
                  color = "black") +
  geom_spatvector(data = dat_box_vect,
                  fill = NA,
                  color = pal_inset) +
  geom_spatvector(data = dat_owner,
                  aes(fill = Fill),
                  color = NA,
                  alpha = 1.00) +
  geom_spatvector(data = dat_mtbs,
                  aes(fill = Fill),
                  color = NA,
                  alpha = 0.75) +
  scale_fill_manual(values = pal_both) +
  coord_sf(xlim = dat_box[1:2],
           ylim = dat_box[3:4]) +
  theme_void() +
  theme(legend.position = "inside",
        legend.position.inside = c(-0.05, 0.50),
        legend.key.height = unit(5.00, "lines"),
        legend.key.width = unit(0.50, "lines"),
        legend.text = 
          element_text(angle = 90,
                       hjust = 0.00,
                       margin = margin(t = 0, r = 0, b = 0.50, l = 0.50, unit = "lines")),
        legend.text.position = "left",
        legend.title = element_blank())

#   State Inset

vis_oregon =
  ggplot() + 
  geom_spatvector(data = dat_ocean_oregon,
                  fill = "grey80",
                  color = "black") +
  geom_spatvector(data = dat_states_oregon,
                  fill = "grey90",
                  color = "black") +
  geom_spatvector(data = dat_oregon,
                  fill = "grey95",
                  color = "black") +
  geom_spatvector(data = dat_bounds,
                  fill = "grey100",
                  color = "black") +
  geom_spatvector(data = dat_box_vect,
                  fill = NA,
                  color = pal_inset) +
  geom_spatvector(data = dat_box_oregon_vect,
                  fill = NA,
                  color = pal_inset) +
  geom_spatvector(data = dat_owner,
                  aes(fill = Fill),
                  color = NA,
                  alpha = 1.00) +
  geom_spatvector(data = dat_mtbs,
                  aes(fill = Fill),
                  color = NA,
                  alpha = 0.75) +
  scale_fill_manual(values = pal_both) +
  coord_sf(xlim = dat_box_oregon[1:2],
           ylim = dat_box_oregon[3:4]) +
  theme_void() +
  theme(legend.position = "none")
  
#   Country Inset

vis_states =
  ggplot() + 
  # geom_spatvector(data = dat_ocean_states,
  #                 fill = "grey80",
  #                 color = "black") +
  # NA goes here
  geom_spatvector(data = dat_states_inset,
                  fill = "grey90",
                  color = "black") +
  geom_spatvector(data = dat_oregon,
                  fill = "grey95",
                  color = "black") +
  geom_spatvector(data = dat_bounds,
                  fill = "grey100",
                  color = "black") +
  geom_spatvector(data = dat_box_vect,
                  fill = NA,
                  color = pal_inset) +
  geom_spatvector(data = dat_box_oregon_vect,
                  fill = NA,
                  color = pal_inset) +
  geom_spatvector(data = dat_box_states_vect,
                  fill = NA,
                  color = pal_inset) +
  geom_spatvector(data = dat_owner,
                  aes(fill = Fill),
                  color = NA,
                  alpha = 1.00) +
  geom_spatvector(data = dat_mtbs,
                  aes(fill = Fill),
                  color = NA,
                  alpha = 0.75) +
  scale_fill_manual(values = pal_both) +
  coord_sf(xlim = dat_box_states[1:2],
           ylim = dat_box_states[3:4]) +
  theme_void() +
  theme(legend.position = "none")

# Patchwork

vis_all = vis_main | (vis_oregon / vis_states)

# Export

ggsave("04_out/Paper_FirmSupply/vis_1_map.png",
       vis_main,
       dpi = 300,
       height = 4.5,
       width = 6.5)
