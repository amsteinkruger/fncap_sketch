# Handle spatial and temporal intersections in FERNS. 

# take intersection reassignment approach before proceeding

#  Clear the environment.

rm(list = ls())

# Start timing. 

time_start = Sys.time()

# Notifications

dat_notifications = 
  "03_intermediate/dat_notifications_1_3.gdb" %>% 
  vect %>% 
  filter(ActivityType == "Clearcut/Overstory Removal") %>% # Implicit in 1_3. 
  filter(ActivityUnit == "MBF") %>%
  filter(str_sub(OperationName, 1, 10) != "do not use") %>% 
  cbind(., expanse(., unit = "ha") * 2.47105381) %>% 
  rename(Acres = y) %>% 
  # slice_sample(n = 10000) %>% 
  mutate(UID = row_number()) # Joins in this script require a sequential ID. 

dat_notifications_less = 
  dat_notifications %>% 
  select(UID)

# Intersections

#  Count intersections.

dat_notifications_intersect = 
  dat_notifications_less %>% 
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
                ~ expanse(intersect(dat_notifications_less[.x], 
                                    dat_notifications_less[.y]), 
                          unit = "ha"))) %>% 
  unnest(area_intersect) %>% 
  mutate(Acres_Intersect = area_intersect * 2.47) %>% # Hectares to acres.
  left_join(dat_notifications %>% 
              as_tibble %>% 
              select(UID, Acres), 
            by = c("from" = "UID")) %>% 
  rename(Acres_From = Acres) %>% 
  left_join(dat_notifications %>% 
              as_tibble %>% 
              select(UID, Acres), 
            by = c("to" = "UID")) %>% 
  rename(Acres_To = Acres) %>% 
  mutate(Acres_From_Proportion = Acres_Intersect / Acres_From,
         Acres_To_Proportion = Acres_Intersect / Acres_To) %>% 
  select(from, to, Acres_From_Proportion, Acres_To_Proportion) %>% 
  pivot_longer(c(from, to)) %>% 
  mutate(Proportion = ifelse(name == "from", Acres_From_Proportion, Acres_To_Proportion)) %>% 
  select(UID = value, Intersect = Proportion) %>% 
  group_by(UID) %>% 
  summarize(Intersect_Maximum = Intersect %>% max) %>% 
  left_join(dat_notifications_less %>% as_tibble, .) %>% 
  mutate(Intersect_Maximum = Intersect_Maximum %>% replace_na(0))

#  Join and export. 

dat_notifications = 
  dat_notifications %>% 
  left_join(dat_join_intersect_proportional) %>% 
  filter(Intersect_Maximum < 0.01) %T>% 
  # Export with spatial data. 
  writeVector("03_intermediate/dat_notifications_1_4.gdb") %>% 
  # Export without spatial data. 
  as_tibble %T>% 
  write_csv("03_intermediate/dat_notifications_1_4.csv")

#  Stop timing. 

time_end = Sys.time()

time_end - time_start
