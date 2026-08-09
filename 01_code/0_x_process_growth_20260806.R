# Wrangle FIA and estimate growth models.

# Grab disturbance and treatment variables, then reduce meaningfully. 

#  Data

#   Bounds

dat_bounds = "03_intermediate/dat_bounds.gdb" %>% vect

#   Pyromes

dat_pyrome = 
  "02_data/1_2_2_USFS_Pyromes/Data/Pyromes_CONUS_20200206.shp" %>% 
  vect %>% 
  rename(WHICH = NAME) %>% # Band-Aid for a reserved attribute name.
  filter(WHICH %in% c("Marine Northwest Coast Forest", "Klamath Mountains", "Middle Cascades")) %>% 
  select(Pyrome = WHICH) %>% 
  project("EPSG:2992") %>% 
  crop(dat_bounds)

#  ODF Private Forest Districts

dat_districts = 
  "02_data/1_6_7_ODF_Districts/District_Boundaries.geojson" %>%
  vect %>%
  select(District = pf_dist) %>%
  project("EPSG:2992") %>%
  makeValid(buffer = TRUE) %>%
  crop(dat_bounds)

#  Counties

dat_counties = 
  "02_data/1_6_6_TIGER/TIGER.gdb" %>% 
  vect(layer = "County") %>% 
  select(County = NAMELSAD) %>% 
  project("EPSG:2992") %>%
  crop(dat_bounds)

#   Plots

dat_plot = 
  "02_data/1_5_1_FIA/OR_PLOT.csv" %>%
  read_csv %>% 
  select(
    INVYR,
    MEASYEAR,
    PLT_CN = CN,
    STATECD,
    COUNTYCD,
    LAT,
    LON
  )

#   Conditions
  
dat_condition =
  "02_data/1_5_1_FIA/OR_COND.csv" %>%
  read_csv %>%
  select(
    INVYR,
    PLT_CN,
    CON_CN = CN,
    CONDID,
    STDAGE,
    SITECLCD,
    FORTYPCD,
    OWNGRPCD
  )

#   Trees

dat_tree =
  "02_data/1_5_1_FIA/OR_TREE.csv" %>%
  read_csv %>% 
  select(
    INVYR,
    PLT_CN,
    CONDID,
    TRE_CN = CN,
    TPA_UNADJ,
    VOLBFNET,
    SPCD)

#   Tree Growth Estimation

dat_growth = 
  "02_data/1_5_1_FIA/OR_TREE_GRM_ESTN.csv" %>% 
  read_csv %>% 
  filter(COMPONENT == "SURVIVOR") %>% 
  filter(ESTIMATE == "VOLBFNET") %>% 
  filter(ANN_NET_GROWTH > 0) %>% 
  mutate(EST_BEGIN_ACRE = EST_BEGIN * TPAGROW_UNADJ,
         EST_END_ACRE = EST_END * TPAGROW_UNADJ,
         ANN_NET_GROWTH_ACRE = ANN_NET_GROWTH * TPAGROW_UNADJ) %>% 
  select(INVYR, PLT_CN, TRE_CN, EST_BEGIN_ACRE, EST_END_ACRE, ANN_NET_GROWTH_ACRE)

#   Wrangle

#    Pick age range.

vec_stdage = 1:75

dat_use = 
  # Handle tree data.
  dat_tree %>% 
  filter(!is.na(VOLBFNET)) %>%
  filter(SPCD %in% c(202, 263)) %>%
  mutate(VOLBFNET_ACRE = VOLBFNET * TPA_UNADJ) %>% 
  # Handle growth estimation data.
  left_join(dat_growth) %>% 
  distinct %>% # Why do observations duplicate on join? 
  # Reduce to conditions. 
  group_by(
    INVYR,
    PLT_CN,
    CONDID,
    SPCD
  ) %>% 
  summarize(
    VOLBFNET_ACRE = sum(VOLBFNET_ACRE, na.rm = TRUE),
    EST_BEGIN_ACRE = sum(EST_BEGIN_ACRE, na.rm = TRUE),
    EST_END_ACRE = sum(EST_END_ACRE, na.rm = TRUE),
    ANN_NET_GROWTH_ACRE = sum(ANN_NET_GROWTH_ACRE, na.rm = TRUE)
    ) %>% 
  ungroup %>% 
  mutate(across(ends_with("ACRE"), ~ ifelse(.x == 0, NA, .x)),
         across(ends_with("ACRE"), ~ .x / 1000)) %>% # BF to MBF. 
  # Handle condition data.
  left_join(dat_condition) %>% 
  filter(FORTYPCD == 201) %>% 
  filter(OWNGRPCD == 40) %>% 
  filter(STDAGE %!in% c(NA, 0, 998, 999)) %>% 
  # Handle plot data.
  left_join(dat_plot) %>% 
  # Explicate spatial data and reduce to region of interest.
  vect(
    geom = c("LON", "LAT"),
    crs = "EPSG:4326"
    ) %>% 
  project("EPSG:2992") %>% 
  crop(dat_bounds) %>% 
  # Match to pyromes, districts, and counties.
  intersect(dat_pyrome) %>% 
  intersect(dat_districts) %>% 
  intersect(dat_counties) %>% 
  # Back to implicit spatial data. 
  as_tibble %>% 
  # Cut western hemlock for now. 
  filter(SPCD == 202) %>%
  # Cut stands older than 75 years for now.
  filter(STDAGE %in% 1:75) %>%
  # Handle outliers.
  filter(ntile(VOLBFNET_ACRE, 100) %in% 2:99) %>% 
  filter(ntile(VOLBFNET_ACRE / STDAGE, 100) %in% 2:99) %>% 
  filter(ntile(EST_BEGIN_ACRE, 100) %in% 2:99 | is.na(EST_BEGIN_ACRE)) # %>% 
  # filter(ntile(ANN_NET_GROWTH_ACRE, 100) %in% 2:99 | is.na(ANN_NET_GROWTH_ACRE)) %>% 
  # filter(ntile(ANN_NET_GROWTH_ACRE / VOLBFNET_ACRE, 100) %in% 2:99 | is.na(ANN_NET_GROWTH_ACRE))

# 2262 yield observations, 588 growth observations for Douglas fir without age restriction.
# 2192 yield observations, 573 growth observations for Douglas fir with STDAGE < 100. 
# 1555 yield observations, 430 growth observations for Douglas fir with STDAGE < 100 and FORTYPCD == 201. 
# 1472 yield observations with STDAGE <= 75, FORTYPCD == 201. 

# 47 of 1472 observations with MBF > 47. 

# Estimation

dat_estimates = 
  dat_use %>% 
  mutate(Aggregate = "All") %>% 
  pivot_longer(
    c(Aggregate, Pyrome, District, County),
    names_to = "Definition",
    values_to = "Region") %>% 
  group_by(Definition, Region) %>% 
  nest %>% 
  ungroup %>% 
  arrange(Definition, Region) %>% 
  # Estimate parameters. 
  mutate(
    Estimate_OLS = 
      data %>% 
      map(
        ~ lm(
          VOLBFNET_ACRE ~ 0 + STDAGE, 
          data = .x
        )
      ),
    Estimate_VB =
      data %>%
      map(
        ~ tryCatch(
          {
            nls(
              VOLBFNET_ACRE ~ a * (1 - exp(- b * STDAGE)) ^ 3,
              data = .,
              start = list(a = 150, b = 0.01)
            )
          },
          error = function(message){NA}
        )
      )
  ) %>% 
  # Get parameters. 
  mutate(
    n = 
      data %>% 
      map(nrow),
    OLS_b = 
      Estimate_OLS %>% 
      map(~ .x %>% coef %>% magrittr::extract(1)),
    VB_a = 
      Estimate_VB %>% 
      map(
        ~ ifelse(
          !is.logical(.x),
          .x %>% coef %>% magrittr::extract("a"),
          NA
        )
      ),
    VB_b = 
      Estimate_VB %>% 
      map(
        ~ ifelse(
          !is.logical(.x),
          .x %>% coef %>% magrittr::extract("b"),
          NA
        )
      )
    ) %>% 
  unnest(cols = c(n, OLS_b, VB_a, VB_b)) %>% 
  arrange(Definition, desc(n)) %>% 
  # Add predictions.
  mutate(
    Prediction_OLS = 
      OLS_b %>% 
      map(~ .x * vec_stdage),
    Prediction_VB = 
      map2(
        VB_a,
        VB_b,
        ~ .x * (1 - exp(- .y * vec_stdage)) ^ 3
      )
  )

# Tables

tab_pyrome = 
  dat_estimates %>% 
  filter(Definition %in% c("Aggregate", "Pyrome")) %>% 
  select(Pyrome = Region, n, OLS_b, VB_a, VB_b) %>% 
  mutate(across(c(OLS_b, VB_a, VB_b), ~ round(.x, 3))) %T>% 
  write_csv("04_out/Presentation_20260809/tab_pyrome.csv")

tab_district = 
  dat_estimates %>% 
  filter(Definition %in% c("Aggregate", "District")) %>% 
  select(District = Region, n, OLS_b, VB_a, VB_b) %>% 
  mutate(across(c(OLS_b, VB_a, VB_b), ~ round(.x, 3))) %T>% 
  write_csv("04_out/Presentation_20260809/tab_district.csv")

tab_county = 
  dat_estimates %>% 
  filter(Definition %in% c("Aggregate", "County")) %>% 
  select(County = Region, n, OLS_b, VB_a, VB_b) %>% 
  mutate(across(c(OLS_b, VB_a, VB_b), ~ round(.x, 3))) %T>% 
  write_csv("04_out/Presentation_20260809/tab_county.csv")

# Plots

#  Pyromes

#   All growth estimates in one plot

vis_pyrome_all = 
  dat_estimates %>% 
  filter(Definition %in% c("Aggregate", "Pyrome")) %>% 
  select(Region, Prediction_VB) %>% 
  unnest(Prediction_VB) %>% 
  mutate(Age = rep(vec_stdage, length(unique(Region)))) %>% # Band-Aid
  pivot_longer(Prediction_VB) %>% 
  mutate(Region_Which = ifelse(Region == "All", "All", "Other")) %>% 
  ggplot() +
  geom_line(aes(x = Age,
                y = value,
                group = Region,
                # color = Region,
                linewidth = Region_Which,
                linetype = Region_Which)) +
  labs(x = "Stand Age",
       y = "MBF") +
  scale_x_continuous(limits = c(0, 75),
                     breaks = c(0, 25, 50, 75),
                     expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 30),
                     breaks = c(0, 10, 20, 30),
                     expand = c(0, 0)) +
  scale_linewidth_manual(values = c(1.75, 1.25)) +
  scale_linetype_manual(values = c("dashed", "solid")) +
  theme_pubr() +
  theme(legend.position = "none")

ggsave("04_out/Presentation_20260809/vis_pyrome_all.png",
       vis_pyrome_all,
       dpi = 300,
       width = 8,
       height = 6)

#   Each growth estimate in a separate plot with points for observations

vis_pyrome_each =
  dat_estimates %>% 
  filter(Definition %in% c("Aggregate", "Pyrome")) %>% 
  select(Region, data, Prediction_OLS, Prediction_VB) %>% 
  ggplot() +
  geom_point(data = 
               . %>% 
               unnest(data) %>% 
               select(Region, Age = STDAGE, MBF = VOLBFNET_ACRE),
             aes(x = Age,
                 y = MBF),
             shape = 21,
             fill = NA,
             alpha = 0.33) +
  geom_line(data = 
              . %>% 
              unnest(Prediction_OLS) %>% 
              mutate(Age = rep(vec_stdage, length(unique(Region)))) %>% # Band-Aid
              select(Region, Age, MBF = Prediction_OLS),
            aes(x = Age,
                y = MBF),
            linewidth = 1.25,
            color = "red3",
            alpha = 0.50) +
  geom_line(data = 
              . %>% 
              unnest(Prediction_VB) %>% 
              mutate(Age = rep(vec_stdage, length(unique(Region)))) %>% # Band-Aid
              select(Region, Age, MBF = Prediction_VB),
            aes(x = Age,
                y = MBF),
            linewidth = 1.25,
            color = "red",
            alpha = 0.75) +
  facet_wrap(~ Region) +
  scale_x_continuous(limits = c(0, 75),
                     breaks = c(0, 25, 50, 75)) +
  scale_y_continuous(limits = c(0, 45),
                     breaks = c(0, 15, 30, 45)) +
  theme_pubr()

ggsave("04_out/Presentation_20260809/vis_pyrome_each.png",
       vis_pyrome_each,
       dpi = 300,
       width = 8,
       height = 6)

#   n, a, b for each geography

vis_pyrome_map_n = 
  dat_estimates %>% 
  filter(Definition %in% "Pyrome") %>% 
  select(Pyrome = Region, n) %>% 
  left_join(dat_pyrome, .) %>% 
  ggplot() + 
  geom_spatvector(aes(fill = n), color = NA) +
  scale_fill_distiller(palette = "Greens", direction = 1) +
  theme_void() +
  theme(legend.position = "bottom",
        legend.direction = "horizontal")

vis_pyrome_map_a = 
  dat_estimates %>% 
  filter(Definition %in% "Pyrome") %>% 
  select(Pyrome = Region, VB_a) %>% 
  left_join(dat_pyrome, .) %>% 
  ggplot() + 
  geom_spatvector(aes(fill = VB_a), color = NA) +
  scale_fill_distiller(palette = "Oranges", direction = 1) +
  theme_void() +
  theme(legend.position = "bottom",
        legend.direction = "horizontal")

vis_pyrome_map_b = 
  dat_estimates %>% 
  filter(Definition %in% "Pyrome") %>% 
  select(Pyrome = Region, VB_b) %>% 
  left_join(dat_pyrome, .) %>% 
  ggplot() + 
  geom_spatvector(aes(fill = VB_b), color = NA) +
  scale_fill_distiller(palette = "Purples", direction = 1) +
  theme_void() +
  theme(legend.position = "bottom",
        legend.direction = "horizontal")

vis_pyrome_map = vis_pyrome_map_n + vis_pyrome_map_a + vis_pyrome_map_b

ggsave("04_out/Presentation_20260809/vis_pyrome_map.png",
       vis_pyrome_map,
       dpi = 300,
       width = 8,
       height = 6)

#  Districts

#   All growth estimates in one plot

vis_district_all = 
  dat_estimates %>% 
  filter(Definition %in% c("Aggregate", "District")) %>% 
  select(Region, Prediction_VB) %>% 
  unnest(Prediction_VB) %>% 
  mutate(Age = rep(vec_stdage, length(unique(Region)))) %>% # Band-Aid
  pivot_longer(Prediction_VB) %>% 
  mutate(Region_Which = ifelse(Region == "All", "All", "Other")) %>% 
  ggplot() +
  geom_line(aes(x = Age,
                y = value,
                group = Region,
                # color = Region,
                linewidth = Region_Which,
                linetype = Region_Which)) +
  labs(x = "Stand Age",
       y = "MBF") +
  scale_x_continuous(limits = c(0, 75),
                     breaks = c(0, 25, 50, 75),
                     expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 30),
                     breaks = c(0, 10, 20, 30),
                     expand = c(0, 0)) +
  scale_linewidth_manual(values = c(1.75, 1.25)) +
  scale_linetype_manual(values = c("dashed", "solid")) +
  theme_pubr() +
  theme(legend.position = "none")

ggsave("04_out/Presentation_20260809/vis_district_all.png",
       vis_district_all,
       dpi = 300,
       width = 8,
       height = 6)

#   Each growth estimate in a separate plot with points for observations

vis_district_each =
  dat_estimates %>% 
  filter(Definition %in% c("Aggregate", "District")) %>% 
  select(Region, data, Prediction_OLS, Prediction_VB) %>% 
  ggplot() +
  geom_point(data = 
               . %>% 
               unnest(data) %>% 
               select(Region, Age = STDAGE, MBF = VOLBFNET_ACRE),
             aes(x = Age,
                 y = MBF),
             shape = 21,
             fill = NA,
             alpha = 0.33) +
  geom_line(data = 
              . %>% 
              unnest(Prediction_OLS) %>% 
              mutate(Age = rep(vec_stdage, length(unique(Region)))) %>% # Band-Aid
              select(Region, Age, MBF = Prediction_OLS),
            aes(x = Age,
                y = MBF),
            linewidth = 1.25,
            color = "red3",
            alpha = 0.50) +
  geom_line(data = 
              . %>% 
              unnest(Prediction_VB) %>% 
              mutate(Age = rep(vec_stdage, length(unique(Region)))) %>% # Band-Aid
              select(Region, Age, MBF = Prediction_VB),
            aes(x = Age,
                y = MBF),
            linewidth = 1.25,
            color = "red",
            alpha = 0.75) +
  facet_wrap(~ Region) +
  scale_x_continuous(limits = c(0, 75),
                     breaks = c(0, 25, 50, 75)) +
  scale_y_continuous(limits = c(0, 45),
                     breaks = c(0, 15, 30, 45)) +
  theme_pubr()

ggsave("04_out/Presentation_20260809/vis_district_each.png",
       vis_district_each,
       dpi = 300,
       width = 8,
       height = 6)

#   n, a, b for each geography

vis_district_map_n = 
  dat_estimates %>% 
  filter(Definition %in% "District") %>% 
  select(District = Region, n) %>% 
  left_join(dat_districts, .) %>% 
  ggplot() + 
  geom_spatvector(aes(fill = n), color = NA) +
  scale_fill_distiller(palette = "Greens", direction = 1) +
  theme_void() +
  theme(legend.position = "bottom",
        legend.direction = "horizontal")

vis_district_map_a = 
  dat_estimates %>% 
  filter(Definition %in% "District") %>% 
  select(District = Region, VB_a) %>% 
  left_join(dat_districts, .) %>% 
  ggplot() + 
  geom_spatvector(aes(fill = VB_a), color = NA) +
  scale_fill_distiller(palette = "Oranges", direction = 1) +
  theme_void() +
  theme(legend.position = "bottom",
        legend.direction = "horizontal")

vis_district_map_b = 
  dat_estimates %>% 
  filter(Definition %in% "District") %>% 
  select(District = Region, VB_b) %>% 
  left_join(dat_districts, .) %>% 
  ggplot() + 
  geom_spatvector(aes(fill = VB_b), color = NA) +
  scale_fill_distiller(palette = "Purples", direction = 1) +
  theme_void() +
  theme(legend.position = "bottom",
        legend.direction = "horizontal")

vis_district_map = vis_district_map_n + vis_district_map_a + vis_district_map_b

ggsave("04_out/Presentation_20260809/vis_district_map.png",
       vis_district_map,
       dpi = 300,
       width = 8,
       height = 6)

#  Counties

#   All growth estimates in one plot

vis_county_all = 
  dat_estimates %>% 
  filter(Definition %in% c("Aggregate", "County")) %>% 
  select(Region, Prediction_VB) %>% 
  unnest(Prediction_VB) %>% 
  mutate(Age = rep(vec_stdage, length(unique(Region)))) %>% # Band-Aid
  pivot_longer(Prediction_VB) %>% 
  mutate(Region_Which = ifelse(Region == "All", "All", "Other")) %>% 
  ggplot() +
  geom_line(aes(x = Age,
                y = value,
                group = Region,
                # color = Region,
                linewidth = Region_Which,
                linetype = Region_Which)) +
  labs(x = "Stand Age",
       y = "MBF") +
  scale_x_continuous(limits = c(0, 75),
                     breaks = c(0, 25, 50, 75),
                     expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 30),
                     breaks = c(0, 10, 20, 30),
                     expand = c(0, 0)) +
  scale_linewidth_manual(values = c(1.75, 1.25)) +
  scale_linetype_manual(values = c("dashed", "solid")) +
  theme_pubr() +
  theme(legend.position = "none")

ggsave("04_out/Presentation_20260809/vis_county_all.png",
       vis_county_all,
       dpi = 300,
       width = 8,
       height = 6)

#   Each growth estimate in a separate plot with points for observations

vis_county_each =
  dat_estimates %>% 
  filter(Definition %in% c("Aggregate", "County")) %>% 
  select(Region, data, Prediction_OLS, Prediction_VB) %>% 
  ggplot() +
  geom_point(data = 
               . %>% 
               unnest(data) %>% 
               select(Region, Age = STDAGE, MBF = VOLBFNET_ACRE),
             aes(x = Age,
                 y = MBF),
             shape = 21,
             fill = NA,
             alpha = 0.33) +
  geom_line(data = 
              . %>% 
              unnest(Prediction_OLS) %>% 
              mutate(Age = rep(vec_stdage, length(unique(Region)))) %>% # Band-Aid
              select(Region, Age, MBF = Prediction_OLS),
            aes(x = Age,
                y = MBF),
            linewidth = 1.25,
            color = "red3",
            alpha = 0.50) +
  geom_line(data = 
              . %>% 
              unnest(Prediction_VB) %>% 
              mutate(Age = rep(vec_stdage, length(unique(Region)))) %>% # Band-Aid
              select(Region, Age, MBF = Prediction_VB),
            aes(x = Age,
                y = MBF),
            linewidth = 1.25,
            color = "red",
            alpha = 0.75) +
  facet_wrap(~ Region) +
  scale_x_continuous(limits = c(0, 75),
                     breaks = c(0, 25, 50, 75)) +
  scale_y_continuous(limits = c(0, 45),
                     breaks = c(0, 15, 30, 45)) +
  theme_pubr()

ggsave("04_out/Presentation_20260809/vis_county_each.png",
       vis_county_each,
       dpi = 300,
       width = 8,
       height = 6)

#   n, a, b for each geography

vis_county_map_n = 
  dat_estimates %>% 
  filter(Definition %in% "County") %>% 
  select(County = Region, n) %>% 
  left_join(dat_counties, .) %>% 
  ggplot() + 
  geom_spatvector(aes(fill = n), color = NA) +
  scale_fill_distiller(palette = "Greens", direction = 1) +
  theme_void() +
  theme(legend.position = "bottom",
        legend.direction = "horizontal")

vis_county_map_a = 
  dat_estimates %>% 
  filter(Definition %in% "County") %>% 
  select(County = Region, VB_a) %>% 
  left_join(dat_counties, .) %>% 
  ggplot() + 
  geom_spatvector(aes(fill = VB_a), color = NA) +
  scale_fill_distiller(palette = "Oranges", direction = 1) +
  theme_void() +
  theme(legend.position = "bottom",
        legend.direction = "horizontal")

vis_county_map_b = 
  dat_estimates %>% 
  filter(Definition %in% "County") %>% 
  select(County = Region, VB_b) %>% 
  left_join(dat_counties, .) %>% 
  ggplot() + 
  geom_spatvector(aes(fill = VB_b), color = NA) +
  scale_fill_distiller(palette = "Purples", direction = 1) +
  theme_void() +
  theme(legend.position = "bottom",
        legend.direction = "horizontal")

vis_county_map = vis_county_map_n + vis_county_map_a + vis_county_map_b

ggsave("04_out/Presentation_20260809/vis_county_map.png",
       vis_county_map,
       dpi = 300,
       width = 8,
       height = 6)
