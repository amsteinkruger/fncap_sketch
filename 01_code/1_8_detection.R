# Detect clearcuts (and, later, thinning and salvage).  

#  Clear the environment.

rm(list = ls())

#  Start timing. 

time_start = Sys.time()

#  Notifications

dat_notifications = 
  "03_intermediate/dat_notifications_1_6.gdb" %>% 
  vect %>% 
  makeValid(buffer = TRUE)

dat_notifications_less = 
  dat_notifications %>% 
  select(UID)

dat_notifications_variant = 
  "03_intermediate/dat_notifications_1_7.csv" %>% 
  read_csv

dat_notifications_more = 
  dat_notifications %>% 
  full_join(dat_notifications_variant)

# NDVI

dat_ndvi = "03_intermediate/dat_ndvi.tif" %>% rast

dat_notifications_ndvi = 
  dat_notifications_less %>% 
  terra::extract(dat_ndvi, 
                 ., 
                 fun = mean,
                 na.rm = TRUE) %T>% 
  write_csv("03_intermediate/data_notifications_ndvi.csv")

# NDVI to interannual change by quarter.  

dat_notifications_ndvi_join = 
  dat_notifications_ndvi %>% 
  bind_cols(dat_notifications_less %>% as_tibble, .) %>% 
  select(-ID) %>% 
  pivot_longer(cols = starts_with("NDVI"),
               names_prefix = "NDVI_",
               names_to = "Year_Quarter",
               values_to = "NDVI") %>% 
  arrange(UID, Year_Quarter) %>% 
  group_by(UID) %>% 
  mutate(across(NDVI, setNames(lapply(1:4, \(k) ~ lag(.x, k)), paste0("Lag_", 1:4)))) %>% 
  ungroup %>% 
  drop_na(starts_with("NDVI")) %>% 
  mutate(NDVI_Change = NDVI - NDVI_Lag_4) %>% 
  select(UID, Year_Quarter, NDVI_Change)

# Naive modeling approach starts here. 

dat_notifications_out = 
  dat_notifications_variant %>% 
  mutate(Year_Quarter = YearQuarter %>% str_remove("Q")) %>% 
  left_join(dat_notifications_ndvi_join) %>% 
  select(UID, Year_Quarter, NDVI_Change) %>% 
  group_by(UID) %>% 
  mutate(Completion = (NDVI_Change == min(NDVI_Change, na.rm = TRUE)) & NDVI_Change < 0) %>% 
  ungroup %>% 
  filter(Completion) %>% 
  select(-Completion) %>% 
  rename(QuarterCompletion = Year_Quarter) %>% 
  inner_join(dat_notifications, .) %>% 
  left_join(dat_notifications_variant %>%
              mutate(Year_Quarter = YearQuarter %>% str_remove("Q"))) %>%
  select(-YearQuarter) %>%
  relocate(QuarterCompletion, .after = DateCompletion) %T>%
  # Export with spatial data.
  writeVector("03_intermediate/dat_notifications_1_8.gdb") %>%
  # Export without spatial data.
  as_tibble %T>%
  write_csv("03_intermediate/dat_notifications_1_8.csv")

# dat_notifications_out = 
#   dat_notifications %>% 
#   semi_join(dat_notifications_predicted_out) %>% 
#   left_join(dat_notifications_predicted_out) %>% 
#   left_join(dat_notifications_variant %>% 
#               mutate(Year_Quarter = YearQuarter %>% str_remove("Q"))) %>% 
#   select(-YearQuarter) %>% 
#   rename(QuarterCompletion = Year_Quarter) %>% 
#   relocate(QuarterCompletion, .after = DateCompletion) %T>% 
#   # Export with spatial data. 
#   writeVector("03_intermediate/dat_notifications_1_8.gdb") %>% 
#   # Export without spatial data. 
#   as_tibble %T>% 
#   write_csv("03_intermediate/dat_notifications_1_8.csv")

# Less naive modeling approach follows for reference.

# NDVI to long notifications. 

# dat_notifications_labeled = 
#   dat_notifications_variant %>% 
#   select(UID, Year_Quarter = YearQuarter) %>% 
#   mutate(Year_Quarter = Year_Quarter %>% str_remove("Q")) %>% 
#   left_join(dat_notifications %>% 
#               as_tibble %>% 
#               select(UID, Completion, DateCompletion)) %>% 
#   mutate(Year = DateCompletion %>% year,
#          Month = DateCompletion %>% month,
#          Quarter = Month %>% multiply_by(1 / 3) %>% ceiling,
#          Year_Quarter_Completion = ifelse(!is.na(Quarter), paste0(Year, "_", Quarter), NA)) %>% 
#   select(UID, Year_Quarter, Year_Quarter_Completion, Completion) %>% 
#   filter(Completion %in% c("Completed", "Did Not Operate")) %>% 
#   mutate(Completion_Binary = ifelse(Year_Quarter == Year_Quarter_Completion, 1, 0)) %>% 
#   # Account for completion dates outside of the start-end range. Assume these are noise. 
#   group_by(UID) %>% 
#   filter(!(Completion == "Completed" & max(Completion_Binary) == 0)) %>% 
#   ungroup %>% 
#   left_join(dat_notifications_ndvi_join) %>% 
#   mutate(NDVI_Change = NDVI_Change %>% round(3))

#  This brings us to a stupid problem:
#  ODF's "completion" field is as unreliable as the federal alternative.
#  So, what we actually have is just *whether* a notification was realized.
#  One solution would be to use pre-notification mean NDVI changes for comparison.
#  For now, I'll (1) model completion and then (2) pick the earliest quarter with completion. 
#   wait actually use a cumulative minimum to get at the same thing

# dat_notifications_labeled_wrong = 
#   dat_notifications_labeled %>% 
#   arrange(UID, Year_Quarter) %>% 
#   group_by(UID, Completion) %>% 
#   summarize(NDVI_Change_Least = min(NDVI_Change)) %>% 
#   ungroup %>% 
#   mutate(Completion_Binary_Wrong = ifelse(Completion == "Completed", 1, 0))
# 
# mod_wrong = 
#   dat_notifications_labeled_wrong %>% 
#   glm(Completion_Binary_Wrong ~ NDVI_Change_Least, 
#         data = .,
#         family = binomial(link = "probit"))
# 
# par_wrong_0 = mod_wrong$coefficients[1]
# par_wrong_1 = mod_wrong$coefficients[2]

# Model to all notifications; remember that this is the wrong workflow. 

# dat_notifications_predicted = 
#   dat_notifications_ndvi_join %>% 
#   semi_join(dat_notifications_variant %>% mutate(Year_Quarter = YearQuarter %>% str_remove("Q"))) %>% 
#   semi_join(dat_notifications %>% 
#               as_tibble %>% 
#               select(UID, DateStart, DateEnd) %>% 
#               filter(DateStart %>% year > 2014 & DateEnd %>% year < 2025)) %>% 
#   mutate(Completion_Predicted = predict(mod_wrong, select(., NDVI_Change_Least = NDVI_Change), type = "response"),
#          Completion_Predicted_Binary = ifelse(Completion_Predicted > 0.90, 1, 0)) %>% 
#   group_by(UID) %>% 
#   mutate(Completion_Predicted_Binary_Cumulative = Completion_Predicted_Binary %>% cummax) %>% 
#   ungroup
# 
# dat_notifications_predicted_out = 
#   dat_notifications_predicted %>% 
#   group_by(UID) %>% 
#   mutate(Completion_Keep = cumsum(Completion_Predicted_Binary_Cumulative)) %>% 
#   ungroup %>% 
#   filter(Completion_Keep == 1) %>% 
#   select(UID, Year_Quarter)

#  Export

# dat_notifications_out = 
#   dat_notifications %>% 
#   semi_join(dat_notifications_predicted_out) %>% 
#   left_join(dat_notifications_predicted_out) %>% 
#   left_join(dat_notifications_variant %>% 
#               mutate(Year_Quarter = YearQuarter %>% str_remove("Q"))) %>% 
#   select(-YearQuarter) %>% 
#   rename(QuarterCompletion = Year_Quarter) %>% 
#   relocate(QuarterCompletion, .after = DateCompletion) %T>% 
#   # Export with spatial data. 
#   writeVector("03_intermediate/dat_notifications_1_8.gdb") %>% 
#   # Export without spatial data. 
#   as_tibble %T>% 
#   write_csv("03_intermediate/dat_notifications_1_8.csv")

#  Stop timing. 

time_end = Sys.time()

time_end - time_start
