# Handle spatial and temporal intersections in FERNS. 

#  Clear the environment.

rm(list = ls())

# Start timing. 

time_start = Sys.time()

# Notifications

dat_notifications = 
  "03_intermediate/dat_notifications_1_2.gdb" %>% 
  vect %>% 
  cbind(., expanse(., unit = "ha") * 2.47105381) %>% 
  rename(Acres = y)

# Intersections

#  Count intersections.

dat_notifications_intersect = 
  dat_notifications %>% 
  relate(., ., relation = "intersects")

dat_join_intersect_binary = 
  dat_notifications_intersect %>% 
  rowSums() %>% 
  `-` (1) %>% 
  tibble(UID = seq(1, length(.)), Intersects = .)

#  Compute proportional areas of intersections.

dat_join_intersect_proportional = 
  dat_notifications_intersect %>% 
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
              select(UID, Year_Start, Year_End, Acres), 
            by = c("from" = "UID")) %>% 
  rename(Acres_From = Acres,
         Year_From = Year_Start) %>% 
  left_join(dat_notifications %>% 
              as_tibble %>% 
              select(UID, Year_Start, Year_End, Acres), 
            by = c("to" = "UID")) %>% 
  rename(Acres_To = Acres,
         Year_To = Year_Start) %>% 
  mutate(Acres_From_Proportion = Acres_Intersect / Acres_From,
         Acres_To_Proportion = Acres_Intersect / Acres_To) %>% 
  select(from, to, Acres_From_Proportion, Acres_To_Proportion) %>% 
  pivot_longer(c(from, to)) %>% 
  mutate(Proportion = ifelse(name == "from", Acres_From_Proportion, Acres_To_Proportion)) %>% 
  select(UID = value, Intersect = Proportion) %>% 
  group_by(UID) %>% 
  summarize(Intersect_Maximum = Intersect %>% max) %>% 
  left_join(dat_notifications_less_1 %>% as_tibble, .) %>% 
  mutate(Intersect_Maximum = Intersect_Maximum %>% replace_na(0))
