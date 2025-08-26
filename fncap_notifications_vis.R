# Visualize other data with notifications.

#  Remember that this is run on a subset for now. 11391/22800.

# Options

options(scipen = 999)

# Packages

library(tidyverse)
library(terra)
library(tidyterra)


# Data

dat = 
  "output/data_notifications_demo_20250824.gdb" %>% 
  vect %>% 
  mutate(MBFAcres = MBF / Acres)

# Univariate (Histograms)

# Year

vis_uni_year = 
  dat %>% 
  as_tibble %>% 
  group_by(Year) %>% 
  summarize(Count = n()) %>% 
  ungroup %>% 
  ggplot() +
  geom_col(aes(x = Year %>% factor,
               y = Count)) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = "Year") +
  theme_minimal()

# Month

vis_uni_month = 
  dat %>% 
  as_tibble %>% 
  group_by(Month) %>% 
  summarize(Count = n()) %>% 
  ungroup %>% 
  ggplot() +
  geom_col(aes(x = Month %>% factor,
               y = Count)) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = "Month") +
  theme_minimal()

# MBF

vis_uni_mbf = 
  dat %>% 
  as_tibble %>% 
  mutate(Bin = MBF %>% log %>% cut_interval(n = 10)) %>% 
  group_by(Bin) %>%
  summarize(Count = n()) %>% 
  ungroup %>% 
  ggplot() +
  geom_col(aes(x = Bin,
               y = Count)) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = "MBF (Log)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Acres

vis_uni_acres = 
  dat %>% 
  as_tibble %>% 
  mutate(Bin = Acres %>% log %>% cut_interval(n = 10)) %>% 
  group_by(Bin) %>%
  summarize(Count = n()) %>% 
  ungroup %>% 
  ggplot() +
  geom_col(aes(x = Bin,
               y = Count)) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = "Acres (Log)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# MBF / Acre

vis_uni_mbfacres = 
  dat %>% 
  as_tibble %>% 
  mutate(Bin = MBFAcres %>% log %>% cut_interval(n = 10)) %>% 
  group_by(Bin) %>%
  summarize(Count = n()) %>% 
  ungroup %>% 
  ggplot() +
  geom_col(aes(x = Bin,
               y = Count)) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = "MBF / Acre (Log)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Pyrome

vis_uni_pyrome = 
  dat %>% 
  as_tibble %>% 
  group_by(Pyrome) %>% 
  summarize(Count = n()) %>% 
  ungroup %>% 
  ggplot() +
  geom_col(aes(x = Pyrome,
               y = Count)) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = "Pyrome") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Fires (Difference: 0, 15, 30)

vis_uni_fire = 
  dat %>% 
  as_tibble %>% 
  select(Fire_0, Fire_15_Difference, Fire_30_Difference) %>% 
  pivot_longer(cols = everything(),
               values_to = "Fires",
               names_to = "Measure") %>% 
  group_by(Measure, Fires) %>% 
  summarize(Count = n()) %>% 
  ungroup %>% 
  ggplot() +
  geom_col(aes(x = Fires %>% factor,
               y = Count,
               fill = Measure),
           position = position_dodge2()) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = "Fires") +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.direction = "horizontal")

# Price

vis_uni_price = 
  dat %>% 
  as_tibble %>% 
  drop_na(Price) %>% 
  mutate(Bin = Price %>% log %>% cut_interval(n = 10)) %>% 
  group_by(Bin) %>%
  summarize(Count = n()) %>% 
  ungroup %>% 
  ggplot() +
  geom_col(aes(x = Bin,
               y = Count)) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = "Price") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Export

mapply(ggsave,
       filename = 
         paste0(
           "output/",
           c("vis_uni_year",
             "vis_uni_month",
             "vis_uni_pyrome",
             "vis_uni_mbf",
             "vis_uni_acres",
             "vis_uni_mbfacres",
             "vis_uni_fire",
             "vis_uni_price"),
           ".png"),
       plot = 
         list(vis_uni_year,
              vis_uni_month,
              vis_uni_pyrome,
              vis_uni_mbf,
              vis_uni_acres,
              vis_uni_mbfacres,
              vis_uni_fire,
              vis_uni_price),
       dpi = 300,
       width = 6,
       height = 4,
       bg = 'transparent') %>% 
  invisible

# Bivariate (Scatters, Boxplots)

# MBF / Year

vis_biv_mbf_year = 
  dat %>% 
  as_tibble %>% 
  ggplot() +
  geom_boxplot(aes(x = Year %>% factor,
                   y = MBF %>% log)) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = "Year",
       y = "MBF (Log)") +
  theme_minimal() 

# MBF / Month

vis_biv_mbf_month = 
  dat %>% 
  as_tibble %>% 
  ggplot() +
  geom_boxplot(aes(x = Month %>% factor,
                   y = MBF %>% log)) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = "Month",
       y = "MBF (Log)") +
  theme_minimal()

# MBF / Acres

vis_biv_mbf_acres = 
  dat %>% 
  as_tibble %>% 
  ggplot() +
  geom_point(aes(x = Acres %>% log,
                   y = MBF %>% log),
             alpha = 0.1) +
  labs(x = "Acres (Log)",
       y = "MBF (Log)") +
  theme_minimal() 

# MBF / Pyrome

vis_biv_mbf_pyrome = 
  dat %>% 
  as_tibble %>% 
  ggplot() +
  geom_boxplot(aes(x = Pyrome,
                   y = MBF %>% log)) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = "Pyrome",
       y = "MBF (Log)") +
  theme_minimal()

# MBF / Fires

vis_biv_mbf_fires = 
  dat %>% 
  as_tibble %>% 
  ggplot() +
  geom_boxplot(aes(x = Fire_15_Difference %>% factor,
                 y = MBF %>% log)) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = "Fires (15km, Difference)",
       y = "MBF (Log)") +
  theme_minimal() 

# MBF / Price

vis_biv_mbf_price = 
  dat %>% 
  as_tibble %>% 
  ggplot() +
  geom_point(aes(x = Price,
                 y = MBF %>% log),
             alpha = 0.1) +
  labs(x = "Price",
       y = "MBF (Log)") +
  theme_minimal() 

# Acres / Year

vis_biv_acres_year = 
  dat %>% 
  as_tibble %>% 
  ggplot() +
  geom_boxplot(aes(x = Year %>% factor,
                   y = Acres %>% log)) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = "Year",
       y = "Acres (Log)") +
  theme_minimal() 

# Acres / Month

vis_biv_acres_month = 
  dat %>% 
  as_tibble %>% 
  ggplot() +
  geom_boxplot(aes(x = Month %>% factor,
                   y = Acres %>% log)) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = "Month",
       y = "Acres (Log)") +
  theme_minimal() 

# Acres / Pyrome

vis_biv_acres_pyrome = 
  dat %>% 
  as_tibble %>% 
  ggplot() +
  geom_boxplot(aes(x = Pyrome,
                   y = Acres %>% log)) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = "Pyrome",
       y = "Acres (Log)") +
  theme_minimal()

# Acres / Fires

vis_biv_acres_fires = 
  dat %>% 
  as_tibble %>% 
  ggplot() +
  geom_boxplot(aes(x = Fire_15_Difference %>% factor,
                   y = Acres %>% log)) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = "Fires (15km, Difference)",
       y = "Acres (Log)") +
  theme_minimal() 

# Acres / Price

vis_biv_acres_price = 
  dat %>% 
  as_tibble %>% 
  ggplot() +
  geom_point(aes(x = Price,
                 y = Acres %>% log),
             alpha = 0.1) +
  labs(x = "Price",
       y = "Acres (Log)") +
  theme_minimal() 

# MBF / Acres / Year

vis_biv_mbfacres_year = 
  dat %>% 
  as_tibble %>% 
  ggplot() +
  geom_boxplot(aes(x = Year %>% factor,
                   y = MBFAcres %>% log)) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = "Year",
       y = "MBF / Acres (Log)") +
  theme_minimal() 

# MBF / Acres / Month

vis_biv_mbfacres_month = 
  dat %>% 
  as_tibble %>% 
  ggplot() +
  geom_boxplot(aes(x = Month %>% factor,
                   y = MBFAcres %>% log)) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = "Month",
       y = "MBF / Acres (Log)") +
  theme_minimal() 

# MBF / Acres / Acres

vis_biv_mbfacre_acres = 
  dat %>% 
  as_tibble %>% 
  ggplot() +
  geom_point(aes(x = Acres %>% log,
                 y = MBFAcres %>% log),
             alpha = 0.1) +
  labs(x = "Acres (Log)",
       y = "MBF / Acres (Log)") +
  theme_minimal() 

# MBF / Acres / Pyrome

vis_biv_mbfacre_pyrome = 
  dat %>% 
  as_tibble %>% 
  ggplot() +
  geom_boxplot(aes(x = Pyrome,
                   y = MBFAcres %>% log)) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = "Pyrome",
       y = "MBF / Acres (Log)") +
  theme_minimal()

# MBF / Acres / Fires

vis_biv_mbfacres_fires = 
  dat %>% 
  as_tibble %>% 
  ggplot() +
  geom_boxplot(aes(x = Fire_15_Difference %>% factor,
                   y = MBFAcres %>% log)) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = "Fires (15km, Difference)",
       y = "MBF / Acres (Log)") +
  theme_minimal() 

# MBF / Acres / Price

vis_biv_mbfacres_price = 
  dat %>% 
  as_tibble %>% 
  ggplot() +
  geom_point(aes(x = Price,
                 y = MBFAcres %>% log),
             alpha = 0.1) +
  labs(x = "Price",
       y = "MBF / Acres (Log)") +
  theme_minimal()

# Pyrome / Year

vis_biv_pyrome_year = 
  dat %>% 
  as_tibble %>% 
  select(Year, Pyrome) %>% 
  group_by(Year, Pyrome) %>% 
  summarize(Count = n()) %>% 
  ungroup %>% 
  ggplot() +
  geom_col(aes(x = Year %>% factor,
               y = Count,
               fill = Pyrome),
           position = position_dodge2()) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = "Year") +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.direction = "horizontal")

# Pyrome / Month

vis_biv_pyrome_month = 
  dat %>% 
  as_tibble %>% 
  select(Month, Pyrome) %>% 
  group_by(Month, Pyrome) %>% 
  summarize(Count = n()) %>% 
  ungroup %>% 
  ggplot() +
  geom_col(aes(x = Month %>% factor,
               y = Count,
               fill = Pyrome),
           position = position_dodge2()) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = "Month") +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.direction = "horizontal")

# Pyrome / Fires

vis_biv_pyrome_fires = 
  dat %>% 
  as_tibble %>% 
  select(Pyrome, Fire_15_Difference) %>% 
  ggplot() +
  geom_boxplot(aes(x = Pyrome %>% factor,
               y = Fire_15_Difference,
               color = Pyrome)) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = "Pyrome",
       y = "Fires (15km, Difference)") +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.direction = "horizontal")

# Export

mapply(ggsave,
       filename = 
         paste0(
           "output/",
           c("vis_biv_mbf_year",
             "vis_biv_mbf_month",
             "vis_biv_mbf_acres",
             "vis_biv_mbf_pyrome",
             "vis_biv_mbf_fires",
             "vis_biv_mbf_price",
             "vis_biv_acres_year",
             "vis_biv_acres_month",
             "vis_biv_acres_pyrome",
             "vis_biv_acres_fires",
             "vis_biv_acres_price",
             "vis_biv_mbfacres_year",
             "vis_biv_mbfacres_month",
             "vis_biv_mbfacre_acres",
             "vis_biv_mbfacre_pyrome",
             "vis_biv_mbfacres_fires",
             "vis_biv_mbfacres_price",
             "vis_biv_pyrome_year",
             "vis_biv_pyrome_month",
             "vis_biv_pyrome_fires"),
           ".png"),
       plot = 
         list(vis_biv_mbf_year,
              vis_biv_mbf_month,
              vis_biv_mbf_acres,
              vis_biv_mbf_pyrome,
              vis_biv_mbf_fires,
              vis_biv_mbf_price,
              vis_biv_acres_year,
              vis_biv_acres_month,
              vis_biv_acres_pyrome,
              vis_biv_acres_fires,
              vis_biv_acres_price,
              vis_biv_mbfacres_year,
              vis_biv_mbfacres_month,
              vis_biv_mbfacre_acres,
              vis_biv_mbfacre_pyrome,
              vis_biv_mbfacres_fires,
              vis_biv_mbfacres_price,
              vis_biv_pyrome_year,
              vis_biv_pyrome_month,
              vis_biv_pyrome_fires),
       dpi = 300,
       width = 6,
       height = 4,
       bg = 'transparent') %>% 
  invisible
