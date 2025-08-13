# Get packages.

library(tidyverse)
library(terra)
library(tidyterra)
library(ggpubr)

options(scipen = 999)

# Get data.

dat = "data/Boundaries_ODF/StateForestDistricts" %>% vect

# Visualize an initial idea of regions.

dat %>% 
  filter(DISTRICT %in% c("Astoria", "Tillamook", "Forest Grove", "North Cascade", "West Oregon", "Western Lane", "Coos Bay", "Grants Pass")) %>% 
  mutate(Region = 
           case_when(DISTRICT %in% c("Astoria", "Tillamook", "Forest Grove") ~ "AT/TL/FG (1)",
                     DISTRICT %in% c("North Cascade", "West Oregon", "Western Lane") ~ "NC/WO/WL (2, 3)",
                     DISTRICT %in% c("Coos Bay", "Grants Pass") ~ "Grants Pass (4)")) %>% 
  drop_na(DISTRICT) %>% 
  mutate(Region = Region %>% factor %>% fct_relevel("AT/TL/FG (1)", "NC/WO/WL (2, 3)", "Grants Pass (4)")) %>% 
  ggplot() +
  geom_spatvector(aes(fill = Region)) +
  geom_spatvector_text(aes(label = DISTRICT)) +
  theme(axis.title = element_blank())

ggsave("figures/vis_prices_1.png", dpi = 300)

# Visualize another idea (the right one?) for regions.

dat %>% 
  filter(DISTRICT %in% c("Astoria", "Tillamook", "Forest Grove", "North Cascade", "West Oregon", "Western Lane", "Coos Bay", "Grants Pass")) %>% 
  mutate(Region = 
           case_when(DISTRICT %in% c("Astoria", "Tillamook", "Forest Grove", "North Cascade", "West Oregon", "Western Lane") ~ "AT/TL/FG (1)",
                     DISTRICT %in% c("Coos Bay") ~ "NC/WO/WL (2, 3)",
                     DISTRICT %in% c("Grants Pass") ~ "Grants Pass (4)")) %>% 
  drop_na(DISTRICT) %>% 
  mutate(Region = Region %>% factor %>% fct_relevel("AT/TL/FG (1)", "NC/WO/WL (2, 3)", "Grants Pass (4)")) %>% 
  ggplot() +
  geom_spatvector(aes(fill = Region)) +
  geom_spatvector_text(aes(label = DISTRICT)) +
  theme(axis.title = element_blank())

ggsave("figures/vis_prices_2.png", dpi = 300)
