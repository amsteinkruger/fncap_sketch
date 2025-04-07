# Get packages.
# Get data.
# Visualize:
#  INVYR
#  MEASYEAR
#  STDAGE
#  FLDAGE
#  SITECLCD
#  DSTRBCD
#  DSTRBYR
#  TRTCD
#  TRTYR
#  VOLCFNET
#  VOLBFNET
#  DRYBIO
#  CARBON
# Export:
#  ""

# Get packages.

library(tidyverse)
library(ggpubr)

options(scipen = 999)

# Get data.

dat = "output/dat_or_intermediate.csv" %>% read_csv

# Visualize.

#  Data with some shenanigans.

#   Just growth.

vis_growth = 
  dat %>% 
  select(starts_with("VOLCFNET")) %>% 
  ggplot() +
  geom_point(aes(x = VOLCFNET_0, 
                 y = VOLCFNET_1 - VOLCFNET_0),
             alpha = 0.50) +
  scale_x_continuous(limits = c(0, 25000), expand = c(0, 0)) +
  scale_y_continuous(limits = c(-15000, 5000), expand = c(0, 0)) +
  theme_pubr()

#   Growth with DSTRBCD_0, omitting DSTRBCD_0 = 0 to reduce overplotting.  

vis_growth_disturbance_0 = 
  dat %>% 
  select(starts_with("VOLCFNET"), starts_with("DSTRBCD")) %>% 
  filter(DSTRBCD1_0 != 0) %>% 
  mutate(DSTRBCD1_0 = DSTRBCD1_0 %>% factor) %>% 
  ggplot() +
  geom_point(aes(x = VOLCFNET_0, 
                 y = VOLCFNET_1 - VOLCFNET_0,
                 color = DSTRBCD1_0)) +
  scale_x_continuous(limits = c(0, 25000), expand = c(0, 0)) +
  scale_y_continuous(limits = c(-15000, 5000), expand = c(0, 0)) +
  theme_pubr() +
  theme(legend.position = "right")

#   Growth with DSTRBCD1_1, omitting DSTRBCD1_1 = 0 to reduce overplotting.  

vis_growth_disturbance_1 = 
  dat %>% 
  select(starts_with("VOLCFNET"), starts_with("DSTRBCD")) %>% 
  filter(DSTRBCD1_1 != 0) %>% 
  mutate(DSTRBCD1_1 = DSTRBCD1_1 %>% factor) %>% 
  ggplot() +
  geom_point(aes(x = VOLCFNET_0, 
                 y = VOLCFNET_1 - VOLCFNET_0,
                 color = DSTRBCD1_1)) +
  scale_x_continuous(limits = c(0, 25000), expand = c(0, 0)) +
  scale_y_continuous(limits = c(-15000, 5000), expand = c(0, 0)) +
  theme_pubr() +
  theme(legend.position = "right")

#   Growth with TRTCD_0, omitting TRTCD_0 = 0 to reduce overplotting.  

vis_growth_treatment_0 = 
  dat %>% 
  select(starts_with("VOLCFNET"), starts_with("TRTCD")) %>% 
  filter(TRTCD1_0 != 0) %>% 
  mutate(TRTCD1_0 = TRTCD1_0 %>% factor) %>% 
  ggplot() +
  geom_point(aes(x = VOLCFNET_0, 
                 y = VOLCFNET_1 - VOLCFNET_0,
                 color = TRTCD1_0)) +
  scale_x_continuous(limits = c(0, 25000), expand = c(0, 0)) +
  scale_y_continuous(limits = c(-15000, 5000), expand = c(0, 0)) +
  theme_pubr() +
  theme(legend.position = "right")

#   Growth with TRTCD1_1, omitting TRTCD1_1 = 0 to reduce overplotting.  

vis_growth_treatment_1 = 
  dat %>% 
  select(starts_with("VOLCFNET"), starts_with("TRTCD")) %>% 
  filter(TRTCD1_1 != 0) %>% 
  mutate(TRTCD1_1 = TRTCD1_1 %>% factor) %>% 
  ggplot() +
  geom_point(aes(x = VOLCFNET_0, 
                 y = VOLCFNET_1 - VOLCFNET_0,
                 color = TRTCD1_1)) +
  scale_x_continuous(limits = c(0, 25000), expand = c(0, 0)) +
  scale_y_continuous(limits = c(-15000, 5000), expand = c(0, 0)) +
  theme_pubr() +
  theme(legend.position = "right")

#  Growth, highlighting observations with TRTCD1_1 == 10.

vis_growth_treatment_result = 
  dat %>% 
  select(starts_with("VOLCFNET"), starts_with("TRTCD")) %>% 
  mutate(TRTCD1_Result = ifelse(TRTCD1_1 == 10, "Removal", "No Removal")) %>% 
  ggplot() +
  geom_point(aes(x = VOLCFNET_0, 
                 y = VOLCFNET_1 - VOLCFNET_0,
                 color = TRTCD1_Result),
             alpha = 0.50) +
  scale_x_continuous(limits = c(0, 25000), expand = c(0, 0)) +
  scale_y_continuous(limits = c(-15000, 5000), expand = c(0, 0)) +
  theme_pubr() +
  theme(legend.position = "right")

#   Comparing changes in site class to the occurrence of any disturbance or treatment.

vis_class_change = 
  dat %>% 
  select(starts_with("SITECLCD"), 
         starts_with("DSTRBCD"), 
         starts_with("TRTCD")) %>% 
  mutate(`Class Change` = SITECLCD_1 - SITECLCD_0,
         `Class Change` = `Class Change` %>% factor,
         `Dist., Treat.` = (DSTRBCD1_0 + DSTRBCD1_1 + TRTCD1_0 + TRTCD1_1) / (DSTRBCD1_0 + DSTRBCD1_1 + TRTCD1_0 + TRTCD1_1),
         `Dist., Treat.` = `Dist., Treat.` %>% replace_na(0) %>% factor) %>% 
  select(`Class Change`, `Dist., Treat.`) %>% 
  group_by(`Class Change`, `Dist., Treat.`) %>% 
  summarize(Count = n()) %>% 
  ungroup %>% 
  ggplot() +
  geom_col(aes(x = `Class Change`,
               y = Count,
               fill = `Dist., Treat.`),
           position = position_dodge2()) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 200),
                     breaks = seq(0, 200, by = 50),
                     expand = c(0, 0)) +
  theme_pubr()

#  Data with no shenanigans.

#  INVYR

vis_invyr = 
  dat %>% 
  select(INVYR_0, INVYR_1) %>% 
  mutate(INVYR_D = INVYR_1 - INVYR_0) %>% 
  pivot_longer(everything()) %>% 
  group_by(name, value) %>% 
  summarize(count = n()) %>% 
  ungroup %>% 
  mutate(facet = ifelse(name == "INVYR_D", "Difference", "Inventory Year"),
         facet = facet %>% factor %>% fct_rev) %>% 
  ggplot() +
  geom_col(aes(x = value %>% factor,
               y = count,
               fill = name)) +
  scale_y_continuous(expand = c(0, 0)) +
  facet_wrap(~ facet,
             scales = "free") +
  theme_pubr() +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        axis.title.y = element_blank())

#  MEASYEAR

vis_measyear = 
  dat %>% 
  select(MEASYEAR_0, MEASYEAR_1) %>% 
  mutate(MEASYEAR_D = MEASYEAR_1 - MEASYEAR_0) %>% 
  pivot_longer(everything()) %>% 
  group_by(name, value) %>% 
  summarize(count = n()) %>% 
  ungroup %>% 
  mutate(facet = ifelse(name == "MEASYEAR_D", "Difference", "Measurement Year"),
         facet = facet %>% factor %>% fct_rev) %>% 
  ggplot() +
  geom_col(aes(x = value %>% factor,
               y = count,
               fill = name)) +
  scale_y_continuous(expand = c(0, 0)) +
  facet_wrap(~ facet,
             scales = "free") +
  theme_pubr() +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        axis.title.y = element_blank())

#  STDAGE

vis_stdage = 
  dat %>% 
  select(STDAGE_0, STDAGE_1) %>% 
  mutate(STDAGE_D = STDAGE_1 - STDAGE_0) %>% 
  pivot_longer(everything()) %>% 
  group_by(name, value) %>% 
  summarize(count = n()) %>% 
  ungroup %>% 
  mutate(facet = ifelse(name == "STDAGE_D", "Difference", ifelse(name == "STDAGE_0", "Stand Age (0)", "Stand Age (1)")),
         facet = facet %>% factor %>% fct_relevel("Stand Age (0)", "Stand Age (1)", "Difference")) %>% 
  ggplot() +
  geom_histogram(aes(x = value,
                     fill = name),
                 bins = 50) + 
  scale_y_continuous(expand = c(0, 0)) +
  facet_wrap(~ facet,
             scales = "free") +
  theme_pubr() +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        axis.title.y = element_blank())

#  FLDAGE

vis_fldage = 
  dat %>% 
  select(FLDAGE_0, FLDAGE_1) %>% 
  mutate(FLDAGE_D = FLDAGE_1 - FLDAGE_0) %>% 
  pivot_longer(everything()) %>% 
  group_by(name, value) %>% 
  summarize(count = n()) %>% 
  ungroup %>% 
  mutate(facet = ifelse(name == "FLDAGE_D", "Difference", ifelse(name == "FLDAGE_0", "Stand Age (Field Estimate) (0)", "Stand Age (Field Estimate) (1)")),
         facet = facet %>% factor %>% fct_relevel("Stand Age (Field Estimate) (0)", "Stand Age (Field Estimate) (1)", "Difference")) %>% 
  ggplot() +
  geom_histogram(aes(x = value,
                     fill = name),
                 bins = 50) + 
  scale_y_continuous(expand = c(0, 0)) +
  facet_wrap(~ facet,
             scales = "free") +
  theme_pubr() +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        axis.title.y = element_blank())

#  SITECLCD

vis_siteclcd = 
  dat %>% 
  select(SITECLCD_0, SITECLCD_1) %>% 
  mutate(SITECLCD_D = SITECLCD_1 - SITECLCD_0) %>% 
  pivot_longer(everything()) %>% 
  group_by(name, value) %>% 
  summarize(count = n()) %>% 
  ungroup %>% 
  mutate(facet = ifelse(name == "SITECLCD_D", "Difference", "Site Class"),
         facet = facet %>% factor %>% fct_rev) %>% 
  ggplot() +
  geom_col(aes(x = value %>% factor,
               y = count,
               fill = name),
           position = position_dodge2()) +
  scale_y_continuous(expand = c(0, 0)) +
  facet_wrap(~ facet,
             scales = "free") +
  theme_pubr() +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        axis.title.y = element_blank())

#  DSTRBCD

vis_dstrbcd1 = 
  dat %>% 
  select(DSTRBCD1_0, DSTRBCD1_1) %>% 
  mutate(DSTRBCD1_D = DSTRBCD1_1 - DSTRBCD1_0) %>% 
  pivot_longer(everything()) %>% 
  group_by(name, value) %>% 
  summarize(count = n()) %>% 
  ungroup %>% 
  mutate(facet = ifelse(name == "DSTRBCD1_D", "Difference", "Disturbance Code"),
         facet = facet %>% factor %>% fct_rev) %>% 
  ggplot() +
  geom_col(aes(x = value %>% factor,
               y = count,
               fill = name),
           position = position_dodge2()) +
  scale_y_continuous(expand = c(0, 0)) +
  facet_wrap(~ facet,
             scales = "free") +
  theme_pubr() +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        axis.title.y = element_blank())

#  DSTRBYR

vis_dstrbyr1 = 
  dat %>% 
  select(DSTRBYR1_0, DSTRBYR1_1) %>% 
  mutate(DSTRBYR1_D = DSTRBYR1_1 - DSTRBYR1_0) %>% 
  pivot_longer(everything()) %>% 
  group_by(name, value) %>% 
  summarize(count = n()) %>% 
  ungroup %>% 
  mutate(facet = ifelse(name == "DSTRBYR1_D", "Difference", "Disturbance Year"),
         facet = facet %>% factor %>% fct_rev) %>% 
  ggplot() +
  geom_col(aes(x = value %>% factor,
               y = count,
               fill = name),
           position = position_dodge2()) +
  scale_y_continuous(expand = c(0, 0)) +
  facet_wrap(~ facet,
             scales = "free") +
  theme_pubr() +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        axis.title.y = element_blank())

#  TRTCD

vis_trtcd1 = 
  dat %>% 
  select(TRTCD1_0, TRTCD1_1) %>% 
  mutate(TRTCD1_D = TRTCD1_1 - TRTCD1_0) %>% 
  pivot_longer(everything()) %>% 
  group_by(name, value) %>% 
  summarize(count = n()) %>% 
  ungroup %>% 
  mutate(facet = ifelse(name == "TRTCD1_D", "Difference", "Treatment Code"),
         facet = facet %>% factor %>% fct_rev) %>% 
  ggplot() +
  geom_col(aes(x = value %>% factor,
               y = count,
               fill = name),
           position = position_dodge2()) +
  scale_y_continuous(expand = c(0, 0)) +
  facet_wrap(~ facet,
             scales = "free") +
  theme_pubr() +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        axis.title.y = element_blank())

#  TRTYR

vis_trtyr1 = 
  dat %>% 
  select(TRTYR1_0, TRTYR1_1) %>% 
  mutate(TRTYR1_D = TRTYR1_1 - TRTYR1_0) %>% 
  pivot_longer(everything()) %>% 
  group_by(name, value) %>% 
  summarize(count = n()) %>% 
  ungroup %>% 
  mutate(facet = ifelse(name == "TRTYR1_D", "Difference", "Treatment Year"),
         facet = facet %>% factor %>% fct_rev) %>% 
  ggplot() +
  geom_col(aes(x = value %>% factor,
               y = count,
               fill = name),
           position = position_dodge2()) +
  scale_y_continuous(expand = c(0, 0)) +
  facet_wrap(~ facet,
             scales = "free") +
  theme_pubr() +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        axis.title.y = element_blank())

#  VOLCFNET

vis_volcfnet = 
  dat %>% 
  select(VOLCFNET_0, VOLCFNET_1) %>% 
  mutate(VOLCFNET_D = VOLCFNET_1 - VOLCFNET_0) %>% 
  pivot_longer(everything()) %>% 
  mutate(facet = ifelse(name == "VOLCFNET_D", "Difference", ifelse(name == "VOLCFNET_0", "Net Ft^3 Stem Volume (0)", "Net Ft^3 Stem Volume (1)")),
         facet = facet %>% factor %>% fct_relevel("Net Ft^3 Stem Volume (0)", "Net Ft^3 Stem Volume (1)", "Difference")) %>% 
  ggplot() +
  geom_histogram(aes(x = value,
                     fill = name),
                 bins = 50) +
  scale_y_continuous(expand = c(0, 0)) +
  facet_wrap(~ facet,
             scales = "free") +
  theme_pubr() +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        axis.title.y = element_blank())

#  VOLBFNET

vis_volbfnet = 
  dat %>% 
  select(VOLBFNET_0, VOLBFNET_1) %>% 
  mutate(VOLBFNET_D = VOLBFNET_1 - VOLBFNET_0) %>% 
  pivot_longer(everything()) %>% 
  mutate(facet = ifelse(name == "VOLBFNET_D", "Difference", ifelse(name == "VOLBFNET_0", "Gross Sawlog Volume (0)", "Gross Sawlog Volume (1)")),
         facet = facet %>% factor %>% fct_relevel("Gross Sawlog Volume (0)", "Gross Sawlog Volume (1)", "Difference")) %>% 
  ggplot() +
  geom_histogram(aes(x = value,
                     fill = name),
                 bins = 50) +
  scale_y_continuous(expand = c(0, 0)) +
  facet_wrap(~ facet,
             scales = "free") +
  theme_pubr() +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        axis.title.y = element_blank())

#  DRYBIO

vis_drybio = 
  dat %>% 
  select(DRYBIO_0, DRYBIO_1) %>% 
  mutate(DRYBIO_D = DRYBIO_1 - DRYBIO_0) %>% 
  pivot_longer(everything()) %>% 
  mutate(facet = ifelse(name == "DRYBIO_D", "Difference", ifelse(name == "DRYBIO_0", "Total Dry Biomass (0)", "Total Dry Biomass (1)")),
         facet = facet %>% factor %>% fct_relevel("Total Dry Biomass (0)", "Total Dry Biomass (1)")) %>% 
  ggplot() +
  geom_histogram(aes(x = value,
                     fill = name),
                 bins = 50) +
  scale_y_continuous(expand = c(0, 0)) +
  facet_wrap(~ facet,
             scales = "free") +
  theme_pubr() +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        axis.title.y = element_blank())

#  CARBON

vis_carbon = 
  dat %>% 
  select(CARBON_0, CARBON_1) %>% 
  mutate(CARBON_D = CARBON_1 - CARBON_0) %>% 
  pivot_longer(everything()) %>% 
  mutate(facet = ifelse(name == "CARBON_D", "Difference", ifelse(name == "CARBON_0", "Total Carbon (0)", "Total Carbon (1)")),
         facet = facet %>% factor %>% fct_relevel("Total Carbon (0)", "Total Carbon (1)")) %>% 
  ggplot() +
  geom_histogram(aes(x = value,
                     fill = name),
                 bins = 50) +
  scale_y_continuous(expand = c(0, 0)) +
  facet_wrap(~ facet,
             scales = "free") +
  theme_pubr() +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        axis.title.y = element_blank())

# Export.

#  Growth

ggsave("figures/vis_growth.png",
       vis_growth,
       dpi = 300,
       width = 5,
       height = 4)

#  Growth, Disturbance (0)

ggsave("figures/vis_growth_disturbance_0.png",
       vis_growth_disturbance_0,
       dpi = 300,
       width = 6,
       height = 4)

#  Growth, Disturbance (1)

ggsave("figures/vis_growth_disturbance_1.png",
       vis_growth_disturbance_1,
       dpi = 300,
       width = 6,
       height = 4)

#  Growth, Treatment (0)

ggsave("figures/vis_growth_treatment_0.png",
       vis_growth_treatment_0,
       dpi = 300,
       width = 6,
       height = 4)

#  Growth, Treatment (1)

ggsave("figures/vis_growth_treatment_1.png",
       vis_growth_treatment_1,
       dpi = 300,
       width = 6,
       height = 4)

#  Growth, highlighting observations with TRTCD1_1 == 10.

ggsave("figures/vis_growth_treatment_result.png",
       vis_growth_treatment_result,
       dpi = 300,
       width = 6,
       height = 4.25)

# Changes in site class with treatment and disturbance.

ggsave("figures/vis_class_change.png",
       vis_class_change,
       dpi = 300,
       width = 5,
       height = 5)

#  INVYR
#  MEASYEAR
#  STDAGE
#  FLDAGE
#  SITECLCD
#  DSTRBCD
#  DSTRBYR
#  TRTCD
#  TRTYR
#  VOLCFNET
#  VOLBFNET
#  DRYBIO
#  CARBON

ggsave("figures/vis_invyr.png",
       vis_invyr,
       dpi = 300,
       width = 6.5,
       height = 3)

ggsave("figures/vis_measyear.png",
       vis_measyear,
       dpi = 300,
       width = 6.5,
       height = 3)

ggsave("figures/vis_stdage.png",
       vis_stdage,
       dpi = 300,
       width = 6.5,
       height = 3)

ggsave("figures/vis_fldage.png",
       vis_fldage,
       dpi = 300,
       width = 6.5,
       height = 3)

ggsave("figures/vis_siteclcd.png",
       vis_siteclcd,
       dpi = 300,
       width = 6.5,
       height = 3)

ggsave("figures/vis_dstrbcd1.png",
       vis_dstrbcd1,
       dpi = 300,
       width = 6.5,
       height = 3)

ggsave("figures/vis_dstrbyr1.png",
       vis_dstrbyr1,
       dpi = 300,
       width = 6.5,
       height = 3)

ggsave("figures/vis_trtcd1.png",
       vis_trtcd1,
       dpi = 300,
       width = 6.5,
       height = 3)

ggsave("figures/vis_trtyr1.png",
       vis_trtyr1,
       dpi = 300,
       width = 6.5,
       height = 3)

ggsave("figures/vis_volcfnet.png",
       vis_volcfnet,
       dpi = 300,
       width = 6.5,
       height = 3)

ggsave("figures/vis_volbfnet.png",
       vis_volbfnet,
       dpi = 300,
       width = 6.5,
       height = 3)

ggsave("figures/vis_drybio.png",
       vis_drybio,
       dpi = 300,
       width = 6.5,
       height = 3)

ggsave("figures/vis_carbon.png",
       vis_carbon,
       dpi = 300,
       width = 6.5,
       height = 3)
