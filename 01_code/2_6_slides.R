# Show off data. 

#  Data

#   Region

dat_bounds = "03_intermediate/dat_bounds.gdb" %>% vect

#   Oregon

dat_oregon = 
  "02_data/1_2_1_Census_States" %>% 
  vect %>% 
  filter(NAME == "Oregon") %>% 
  project("EPSG:2992")

#   Counties

dat_counties = 
  "02_data/1_6_6_TIGER/TIGER.gdb" %>% 
  vect(layer = "County") %>% 
  select(County = NAMELSAD) %>% 
  filter(County == "Lane County") %>% 
  project("EPSG:2992") %>% 
  crop(dat_bounds)

#   Private Forest/Timberland

#    Note that order matters. 

dat_owner = 
  "02_data/3_4_ODF_Ownership/Ownership.gdb" %>% 
  vect %>% 
  select(Owner = LandManager) %>% 
  project("EPSG:2992") %>% 
  crop(dat_bounds) %>% 
  crop(dat_counties) %>% 
  group_by(Owner) %>%
  summarize %>%
  ungroup %>%
  makeValid(buffer = TRUE)

#   Notifications

#    Interacting ownership and notifications returns some invalid geometries that are annoying to deal with. 

dat_notifications = 
  "03_intermediate/dat_notifications_1_9.gdb" %>% 
  vect %>% 
  select(UID)

dat_notifications_lane = 
  dat_notifications %>% 
  crop(dat_counties)

dat_notifications_lane_private =
  dat_notifications_lane %>%
  relate(dat_owner, relation = "intersects", pairs = TRUE, na.rm = TRUE) %>%
  as_tibble %>%
  select(INDEX = id.x) %>%
  semi_join(dat_notifications_lane %>% mutate(INDEX = row_number()), .)

#   NDVI

# dat_ndvi = 
#   "03_intermediate/dat_ndvi_mean.tif" %>% 
#   rast %>% 
#   crop(dat_counties, mask = TRUE)

dat_ndvi = 
  "03_intermediate/dat_ndvi.tif" %>% 
  rast %>% 
  crop(dat_counties, mask = TRUE)

dat_ndvi_difference = min(dat_ndvi, na.rm = TRUE) - max(dat_ndvi, na.rm = TRUE)

#   Fires

dat_mtbs = 
  "02_data/1_7_1_MTBS/Perimeters" %>% 
  vect%>% 
  project("EPSG:2992") %>% 
  crop(dat_counties)

# (1) Study Region

vis_1 = 
  ggplot() +
  geom_spatvector(data = dat_oregon, color = "#000000", fill = NA) +
  geom_spatvector(data = dat_bounds, color = "#000000", fill = "grey75") +
  theme_void()

# (2) Notifications

vis_2 = 
  vis_1 + 
  geom_spatvector(data = dat_notifications %>% centroids,
                  shape = 21,
                  color = "#000000",
                  fill = NA,
                  alpha = 0.20) +
  theme_void()

# (3) Lane County, Study Region

vis_3 = 
  vis_1 + 
  geom_spatvector(data = dat_counties, color = "#000000", fill = "grey50") +
  geom_spatvector(data = dat_notifications_lane_private %>% centroids,
                  shape = 21,
                  color = "#000000",
                  fill = NA,
                  alpha = 0.25) +
  theme_void()

# (4) Notifications, Lane County

vis_4 = 
  ggplot() +
  geom_spatvector(data = dat_counties, color = "#000000", fill = NA) +
  geom_spatvector(data = dat_notifications_lane_private,
                  color = NA,
                  fill = "darkgreen") +
  theme_void()
  
# (5) Land Cover Change

library(RColorBrewer)

pal_red = brewer.pal(9, "Reds")[8]
pal_orange = brewer.pal(9, "Oranges")[8]
pal_blue = brewer.pal(9, "Blues")[8]
pal_green = brewer.pal(9, "Greens")[8]
pal_purple = brewer.pal(9, "Purples")[8]

vis_5 = 
  ggplot() +
  geom_spatraster(data = dat_ndvi_difference, maxcell = 2500000) +
  geom_spatvector(data = dat_counties, color = "#000000", fill = NA) +
  geom_spatvector(data = dat_notifications_lane_private,
                  color = "#000000",
                  fill = NA) + 
  scale_fill_gradient(
    low = pal_orange,
    high = "white",
    limits = c(-1, 0),
    na.value = NA
  ) +
  # scale_fill_gradient2(
  #   low = pal_orange, 
  #   mid = "white",
  #   high = pal_blue, 
  #   midpoint = 0,
  #   limits = c(-1, 1),
  #   na.value = NA
  # ) +
  theme_void() +
  theme(legend.position = "none")
  
# (6) Ownership

vis_6 = 
  ggplot() +
  geom_spatvector(data = dat_counties, color = "#000000", fill = NA) +
  geom_spatvector(data = dat_owner %>% crop(dat_counties),
                  color = NA,
                  fill = pal_green) +
  geom_spatvector(data = dat_notifications_lane_private,
                  color = "#000000",
                  fill = pal_green) +
  theme_void()

# (7) Climate

dat_cwd = 
  "03_intermediate/data_cwd.tif" %>% 
  rast %>%
  mean %>% 
  crop(dat_counties, mask = TRUE)

vis_7 = 
  ggplot() +
  geom_spatraster(data = dat_cwd, maxcell = 2500000) +
  geom_spatvector(data = dat_counties, color = "#000000", fill = NA) +
  geom_spatvector(data = dat_notifications_lane_private,
                  color = "#000000",
                  fill = NA) + 
  scale_fill_gradient(
    low = "white", 
    high = pal_purple,
    na.value = NA
  ) +
  theme_void() +
  theme(legend.position = "none")

# (8) Fires

vis_8 =
  ggplot() + 
  geom_spatvector(data = dat_counties, color = "#000000", fill = NA) + 
  geom_spatvector(data = dat_mtbs %>% crop(dat_counties), color = NA, fill = pal_red) +
  geom_spatvector(data = dat_notifications_lane_private,
                  color = "#000000",
                  fill = NA) +
  theme_void()

# (9) Prices

dat_price_stumpage = "03_intermediate/data_stumpage.csv" %>% read_csv
dat_price_lumber = "03_intermediate/data_lumber.csv" %>% read_csv

vis_9 = 
  dat_price_stumpage %>% 
  left_join(dat_price_lumber) %>% 
  pivot_longer(-Year_Quarter) %>% 
  mutate(DouglasFir = ifelse(str_detect(name, "DouglasFir"), "Douglas fir", "Western hemlock")) %>% 
  filter(Year_Quarter > "2014_Q4") %>% 
  filter(name %in% c("Stumpage_DouglasFir", "Stumpage_WesternHemlock", "Price_Lumber_HemFir_Kiln_RL", "Price_Lumber_DouglasFir_Kiln_RL")) %>% 
  mutate(Year_Quarter = Year_Quarter %>% str_replace_all("_", " ")) %>% 
  mutate(name = 
           case_when(name == "Stumpage_DouglasFir" ~ "Logs, Douglas fir",
                     name == "Stumpage_WesternHemlock" ~ "Logs, Western hemlock",
                     name == "Price_Lumber_DouglasFir_Kiln_RL"~ "Lumber, Douglas fir",
                     name == "Price_Lumber_HemFir_Kiln_RL" ~ "Lumber, Western hemlock")) %>% 
  ggplot() +
  geom_line(aes(x = Year_Quarter,
                y = value,
                group = name,
                color = name),
            linewidth = 1.25) +
  scale_color_brewer(palette = "Set1") +
  scale_x_discrete(breaks = c("2015 Q1", "2020 Q1", "2024 Q4")) +
  scale_y_continuous(limits = c(0, 1500), 
                     breaks = c(500, 1000, 1500),
                     expand = c(0, 0)) +
  labs(x = "Quarter",
       y = "Nominal US$ per thousand board feet",
       color = "Product") +
  theme_pubr() +
  theme(legend.position = "right",
        legend.direction = "vertical")

# Export

ggsave("04_out/Presentation_20260813/vis_1.png",
       vis_1,
       dpi = 300,
       width = 10,
       height = 6)

ggsave("04_out/Presentation_20260813/vis_2.png",
       vis_2,
       dpi = 300,
       width = 10,
       height = 6)

ggsave("04_out/Presentation_20260813/vis_3.png",
       vis_3,
       dpi = 300,
       width = 10,
       height = 6)

ggsave("04_out/Presentation_20260813/vis_4.png",
       vis_4,
       dpi = 300,
       width = 10,
       height = 6)

ggsave("04_out/Presentation_20260813/vis_5.png",
       vis_5,
       dpi = 300,
       width = 10,
       height = 6)

ggsave("04_out/Presentation_20260813/vis_6.png",
       vis_6,
       dpi = 300,
       width = 10,
       height = 6)

ggsave("04_out/Presentation_20260813/vis_7.png",
       vis_7,
       dpi = 300,
       width = 10,
       height = 6)

ggsave("04_out/Presentation_20260813/vis_8.png",
       vis_8,
       dpi = 300,
       width = 10,
       height = 6)

ggsave("04_out/Presentation_20260813/vis_9.png",
       vis_9,
       dpi = 300,
       width = 10,
       height = 5)
