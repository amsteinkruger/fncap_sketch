# This won't run without results of 05_join_annual in the global environment.
# Eventually, the useful parts of this script should be run in 01 to get the right subset running in 05-06.

# Get equalities of notifications.

dat_notifications_equal_relational = 
  dat_notifications %>% 
  # slice_sample(n = 1000) %>% 
  relate(., ., relation = "equals") %>% 
  as_tibble %>% 
  rownames_to_column("from") %>% 
  pivot_longer(cols = -from,
               names_to = "to",
               values_to = "equal") %>% 
  mutate(to = to %>% str_sub(2, -1),
         across(c(to, from), ~ as.integer(.x))) %>% 
  filter(from != to) %>% # Drop autocomparisons.
  filter(from < to) %>% # Drop duplicate comparisons.
  filter(equal == TRUE) %>% # Drop uninteresting comparisons.
  left_join(dat_notifications_less_2 %>% as_tibble, by = c("from" = "UID")) %>% 
  rename(Year_Start_From = Year_Start) %>% 
  left_join(dat_notifications_less_2 %>% as_tibble, by = c("to" = "UID")) %>% 
  rename(Year_Start_To = Year_Start) %>% 
  mutate(Year_Difference = abs(Year_Start_To - Year_Start_From))

# Check the upper and lower bounds on equalities of notifications.

val_notifications_equal_count_lower = dat_notifications_equal_relational %>% nrow

val_notifications_equal_count_upper = 
  dat_notifications_equal_relational %>% 
  select(from, to) %>% 
  pivot_longer(everything()) %>% 
  pull(value) %>% 
  unique %>% 
  length

# Plot year intervals of equal notifications.

vis_notifications_equal = 
  dat_notifications_equal_relational %>% 
  group_by(Year_Difference) %>% 
  summarize(Count = n()) %>% 
  ungroup %>% 
  ggplot() +
  geom_col(aes(x = Year_Difference,
               y = Count)) +
  scale_x_continuous(breaks = 0:7) +
  labs(x = "Interval in Years",
       title = "Counts of equal notifications by interval") +
  theme_minimal()

# Get intersections of notifications.

dat_notifications_intersect_relational = 
  dat_notifications %>% 
  # slice_sample(n = 1000) %>% 
  relate(., ., relation = "intersects") %>% 
  as_tibble %>% 
  rownames_to_column("from") %>% 
  pivot_longer(cols = -from,
               names_to = "to",
               values_to = "intersects") %>% 
  mutate(to = to %>% str_sub(2, -1),
         across(c(to, from), ~ as.integer(.x))) %>% 
  filter(from != to) %>% # Drop autocomparisons.
  filter(from < to) %>% # Drop duplicate comparisons.
  filter(intersects == TRUE) %>% # Drop uninteresting comparisons.
  mutate(area_intersect = 
           map2(from, 
                to, 
                ~ expanse(intersect(dat_notifications_less_1[.x], 
                                    dat_notifications_less_1[.y]), 
                          unit = "ha"))) %>% 
  unnest(area_intersect) %>% 
  mutate(Acres_Intersect = area_intersect * 2.47) %>% # Hectares to acres.
  left_join(dat_notifications %>% 
              as_tibble %>% 
              select(UID, Year_Start, Acres), 
            by = c("from" = "UID")) %>% 
  rename(Acres_From = Acres,
         Year_From = Year_Start) %>% 
  left_join(dat_notifications %>% 
              as_tibble %>% 
              select(UID, Year_Start, Acres), 
            by = c("to" = "UID")) %>% 
  rename(Acres_To = Acres,
         Year_To = Year_Start) %>% 
  mutate(Acres_From_Proportion = Acres_Intersect / Acres_From,
         Acres_To_Proportion = Acres_Intersect / Acres_To,
         Acres_Proportion_Less = min(Acres_From_Proportion, Acres_To_Proportion),
         Acres_Proportion_More = max(Acres_From_Proportion, Acres_To_Proportion),
         Tercile = ntile(Acres_Proportion_Less, 3),
         Year_Difference = abs(Year_From - Year_To))

dat_notifications_intersect_relational_99 = 
  dat_notifications_intersect_relational %>% 
  filter(Acres_From_Proportion > 0.99 & Acres_To_Proportion > 0.99)

dat_notifications_intersect_relational_01 = 
  dat_notifications_intersect_relational %>% 
  filter(Acres_From_Proportion < 0.01 & Acres_To_Proportion < 0.01)

dat_notifications_intersect_relational_other = 
  dat_notifications_intersect_relational %>% 
  anti_join(dat_notifications_intersect_relational_99, by = c("from", "to")) %>% 
  anti_join(dat_notifications_intersect_relational_01, by = c("from", "to"))

dat_notifications_intersect_relational_other_01 = 
  dat_notifications_intersect_relational_other %>% 
  select(from, to, Acres_From_Proportion, Acres_To_Proportion) %>% 
  pivot_longer(cols = c(from, to)) %>% 
  mutate(proportion = ifelse(name == "from", Acres_From_Proportion, Acres_To_Proportion)) %>% 
  select(value, proportion) %>% 
  group_by(value) %>% 
  summarize(proportion_max = max(proportion),
            proportion_sum = sum(proportion)) %>% 
  ungroup %>% 
  filter(proportion_max < 0.01 & proportion_sum < 0.01)

dat_notifications_intersect_drop = 
  dat_notifications_intersect_relational_other %>% 
  select(from, to) %>% 
  pivot_longer(everything()) %>% 
  distinct(value) %>% 
  anti_join(dat_notifications_intersect_relational_other_01)

# Counts in each group

val_intersect_all = 
  dat_notifications_intersect_relational %>% 
  select(from, to) %>% 
  pivot_longer(everything()) %>% 
  distinct(value) %>% 
  nrow

val_intersect_99 = 
  dat_notifications_intersect_relational_99 %>% 
  select(from, to) %>% 
  pivot_longer(everything()) %>% 
  distinct(value) %>% 
  nrow

val_intersect_01 = 
  dat_notifications_intersect_relational_01 %>% 
  select(from, to) %>% 
  pivot_longer(everything()) %>% 
  distinct(value) %>% 
  nrow

val_intersect_other = 
  dat_notifications_intersect_relational_other %>% 
  select(from, to) %>% 
  pivot_longer(everything()) %>% 
  distinct(value) %>% 
  nrow

val_intersect_other_01 = 
  dat_notifications_intersect_relational_other_01 %>% 
  nrow

val_intersect_drop = 
  dat_notifications_intersect_drop %>% 
  nrow

# Histogram of intersection areas

vis_intersect_all = 
  dat_notifications_intersect_relational %>% 
  select(Acres_From_Proportion, Acres_To_Proportion) %>% 
  pivot_longer(everything()) %>% 
  # pull(value) %>% 
  ggplot() +
  geom_histogram(aes(x = value),
                 bins = 100) + 
  labs(x = "Proportional Overlap",
       y = "Count") +
  theme_minimal()

ggsave("output/vis_intersect_all.png",
       vis_intersect_all,
       dpi = 300,
       width = 6.5)

# Histogram of intervals of years in "99" group

vis_intersect_99 = 
  dat_notifications_intersect_relational_99 %>% 
  group_by(Year_Difference) %>% 
  summarize(Count = n()) %>% 
  ungroup %>% 
  ggplot() +
  geom_col(aes(x = Year_Difference %>% factor,
               y = Count)) +
  labs(x = "Intervals of Years between Notifications",
       y = "Count") +
  theme_minimal()

ggsave("output/vis_intersect_99.png",
       vis_intersect_99,
       dpi = 300,
       width = 6.5)

# Histogram of intervals of years in "01" group

vis_intersect_01 = 
  dat_notifications_intersect_relational_01 %>% 
  group_by(Year_Difference) %>% 
  summarize(Count = n()) %>% 
  ungroup %>% 
  ggplot() +
  geom_col(aes(x = Year_Difference %>% factor,
               y = Count)) +
  labs(x = "Intervals of Years between Notifications",
       y = "Count") +
  theme_minimal()

ggsave("output/vis_intersect_01.png",
       vis_intersect_01,
       dpi = 300,
       width = 6.5)

# Histogram of intervals of years in "Other" group

vis_intersect_other = 
  dat_notifications_intersect_relational_other %>% 
  group_by(Year_Difference) %>% 
  summarize(Count = n()) %>% 
  ungroup %>% 
  ggplot() +
  geom_col(aes(x = Year_Difference %>% factor,
               y = Count)) +
  labs(x = "Intervals of Years between Notifications",
       y = "Count") +
  theme_minimal()

ggsave("output/vis_intersect_other.png",
       vis_intersect_other,
       dpi = 300,
       width = 6.5)

# (2026/02/03) Maps from random starting nodes with single-degree networks. There must be jargon for that.

# Function to get nested plots. Note global call to notifications.

fun_intersect_plot = 
  function(id, dat){
    
    dat %>% 
      semi_join(dat_notifications, ., by = c("UID" = "value")) %>% 
      left_join(dat, by = c("UID" = "value")) %>% 
      ggplot() + 
      geom_spatvector(aes(fill = UID %>% factor,
                          linetype = name %>% factor), 
                      alpha = 0.50) + 
      geom_spatvector_text(aes(label = Year_Start %>% factor)) +
      labs(linetype = "Type",
           fill = "ID") +
      theme_void()
    
    ggsave(paste0("output/intersects/intersects_", id, ".png"),
           dpi = 300,
           width = 4,
           height = 4)
      
  }

dat_notifications_intersect_sample = 
  dat_notifications_intersect_relational %>% 
  select(from, to) %>% 
  pivot_longer(cols = c(from, to)) %>% 
  select(UID = value) %>% 
  slice_sample(n = 1000) %>% 
  nest(data = UID, .by = UID) %>% 
  
  # This next bit is complicated by having dropped either the top or bottom triangle of the initial intersection matrix.
  
  mutate(data_from = 
           data %>% 
           map(~ left_join(.x, dat_notifications_intersect_relational %>% select(from, to), by = c("UID" = "from"))) %>% 
           map(~ pivot_longer(.x, cols = c(UID, to))), 
         data_to = 
           data %>% 
           map(~ left_join(.x, dat_notifications_intersect_relational %>% select(from, to), by = c("UID" = "to"))) %>% 
           map(~ pivot_longer(.x, cols = c(UID, from))),
         data_both = 
           map2(data_from,
                data_to,
                ~ bind_rows(.x, .y)) %>% 
           map(distinct) %>% 
           map(drop_na) %>% 
           map(~ mutate(.x, name = ifelse(name == "UID", "Keystone", "Other"))),
         out = 
           map2(UID, data_both, ~ fun_intersect_plot(id = .x, dat = .y)))
