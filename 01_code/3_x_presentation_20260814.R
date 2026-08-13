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
  group_by(Owner) %>%
  summarize %>%
  ungroup %>%
  makeValid(buffer = TRUE)

#   Notifications

dat_notifications = 
  "03_intermediate/dat_notifications_1_9.gdb" %>% 
  vect %>% 
  makeValid(buffer = TRUE) %>% 
  select(UID)

dat_notifications_relate = 
  dat_notifications %>% 
  relate(dat_owner, relation = "within", pairs = TRUE, na.rm = TRUE) %>% 
  as_tibble %>% 
  select(INDEX = id.x)

dat_notifications = 
  dat_notifications %>% 
  mutate(INDEX = row_number()) %>% 
  semi_join(dat_notifications_relate)

dat_notifications_valid = 
  dat_notifications %>% 
  is.valid %>% 
  as_tibble %>% 
  filter(value == TRUE) %>% 
  mutate(INDEX = row_number(), .keep = "none")

dat_notifications = 
  dat_notifications %>% 
  semi_join(dat_notifications_valid)

dat_notifications_lane = dat_notifications %>% crop(dat_counties)

#   NDVI

dat_ndvi = 
  "03_intermediate/dat_ndvi_mean.tif" %>% 
  rast %>% 
  crop(dat_counties, mask = TRUE)

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

# (1) Study Region

vis_1 = 
  ggplot() +
  geom_spatvector(data = dat_oregon, color = "#000000", fill = NA) +
  geom_spatvector(data = dat_bounds, color = "#000000", fill = "grey75")

# (2) Notifications

vis_2 = 
  vis_1 + 
  geom_spatvector(data = dat_notifications %>% centroids,
                  shape = 21,
                  color = "#000000",
                  fill = NA,
                  alpha = 0.25)

# (3) Lane County, Study Region

vis_3 = 
  vis_1 + 
  geom_spatvector(data = dat_counties, color = "#000000", fill = "grey50") +
  geom_spatvector(data = dat_notifications_lane %>% centroids,
                  shape = 21,
                  color = "#000000",
                  fill = NA,
                  alpha = 0.25)

# (4) Notifications, Lane County

vis_4 = 
  ggplot() +
  geom_spatvector(data = dat_counties, color = "#000000", fill = NA) +
  geom_spatvector(data = dat_notifications_lane,
                  color = NA,
                  fill = "darkgreen")
  
# (5) Land Cover Change

library(RColorBrewer)

pal_red = brewer.pal(9, "Reds")[8]
pal_orange = brewer.pal(9, "Oranges")[8]
pal_blue = brewer.pal(9, "Blues")[8]
pal_green = brewer.pal(9, "Greens")[8]
pal_purple = brewer.pal(9, "Purples")[8]

vis_5 = 
  ggplot() +
  geom_spatraster(data = dat_ndvi, maxcell = 2500000) +
  geom_spatvector(data = dat_counties, color = "#000000", fill = NA) +
  geom_spatvector(data = dat_notifications_lane,
                  color = "#000000",
                  fill = NA) + 
  scale_fill_gradient2(
    low = pal_orange, 
    mid = "white",
    high = pal_blue, 
    midpoint = 0,
    limits = c(-1, 1),
    na.value = NA
  ) +
  theme(legend.position = "none")
  
# (6) Ownership

vis_6 = 
  ggplot() +
  geom_spatvector(data = dat_counties, color = "#000000", fill = NA) +
  geom_spatvector(data = dat_owner %>% crop(dat_counties),
                  color = NA,
                  fill = pal_green) +
  geom_spatvector(data = dat_notifications_lane,
                  color = "#000000",
                  fill = NA)

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
  geom_spatvector(data = dat_notifications_lane,
                  color = "#000000",
                  fill = NA) + 
  scale_fill_gradient(
    low = "white", 
    high = pal_purple,
    na.value = NA
  ) +
  theme(legend.position = "none")

# (8) Fires

vis_8 =
  ggplot() + 
  geom_spatvector(data = dat_counties, color = "#000000", fill = NA) + 
  geom_spatvector(data = dat_mtbs %>% crop(dat_counties), color = NA, fill = pal_red) +
  geom_spatvector(data = dat_notifications_lane,
                  color = "#000000",
                  fill = NA)

# (9) Prices

dat_price_stumpage = "03_intermediate/data_stumpage.csv" %>% read_csv
dat_price_lumber = "03_intermediate/data_lumber.csv" %>% read_csv

vis_9 = 
  dat_price_stumpage %>% 
  left_join(dat_price_lumber) %>% 
  pivot_longer(-Year_Quarter) %>% 
  mutate(DouglasFir = ifelse(str_detect(name, "DouglasFir"), "Douglas fir", "Western hemlock")) %>% 
  filter(!str_detect(name, "Logs")) %>% 
  ggplot() +
  geom_line(aes(x = Year_Quarter,
                y = value,
                group = name,
                color = name)) +
  facet_wrap(~ DouglasFir) +
  theme(legend.position = "none")

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
