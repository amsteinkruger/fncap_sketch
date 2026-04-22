# Handle land ownership. 

#  Steps:
#   Review company names with context; consolidate or discard.
#   For individuals who put their own names in the company field, discard.
#   For uninterpretable strings, discard and add a value to the "Flag" field. 

#  For next time: 
#   Eliminate double spaces, all punctuation, and turn & into "and."
#   Pencil in "Private" for "LLC," "Inc," "Company," . . ..
#   Pencil in "Trust" for "Trust."
#   Note "Family Limited Partnerships."

#  Clear the environment.

rm(list = ls())

#  Start timing. 

time_start = Sys.time()

#  Export notifications for review. 

#   Use a subset for the problem at hand. 

dat_owners_out =
  "03_intermediate/dat_notifications_1_2.csv" %>%
  read_csv %>%
  filter(ActivityType == "Clearcut/Overstory Removal") %>% 
  select(Landowner_Company) %>%
  distinct %>%
  arrange(Landowner_Company) %T>%
  write_xlsx("03_intermediate/dat_owners_out.xlsx")

#  Import reviewed notifications.

#   This only keeps landowners that are (1) reviewed and (2) companies. 

dat_owners_in = 
  "03_intermediate/dat_owners_in.xlsx" %>% 
  read_xlsx %>% 
  drop_na(Landowner_Company_Reviewed) %>% 
  filter(Landowner_Private == 1) %>%
  select(1:2)

dat_notifications = 
  "03_intermediate/dat_notifications_1_2.gdb" %>% 
  vect %>% 
  filter(ActivityType == "Clearcut/Overstory Removal") %>% 
  semi_join(dat_owners_in) %>%
  left_join(dat_owners_in) %T>% 
  # Export with spatial data. 
  writeVector("03_intermediate/dat_notifications_1_3.gdb") %>% 
  # Export without spatial data. 
  as_tibble %T>% 
  write_csv("03_intermediate/dat_notifications_1_3.csv")

#  Stop timing. 

time_end = Sys.time()

time_end - time_start
