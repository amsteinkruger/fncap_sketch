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

#  Eventually:
#   Reconcile notifications with records of land ownership. 

#  20260907 scheme:
#   ideally, handle change detection before anything else; then use reduced notifications from 1_1 on
#   but in any case, nest both ownership and notifications by year-quarter
#   then for each year-quarter, add spatial data (parcels) to ownership, then extract owners to notifications
#    note that the extraction can either be a centroid NN for convenience or a polygon-polygons match
#    but no idea what to do many-many polygon-polygons (that isn't equivalent in principle to centroid NN)
#   then use some combination of notifcations and land use codes to pick timberland
#   then pull out owner strings, apply reasonable string clean-up and hand review to check (1) notification-Cotality disagreements and (2) types
#   consider speeding that up with automated binning into public/private with public lands data

#  wouldn't it be better to intersect all notifications with the subset of data-rich parcels, then map to the ownership panel?
#  skips nesting for a single spatial join

#  Clear the environment.

rm(list = ls())

#  Start timing. 

time_start = Sys.time()

#  Export notifications for review. 

#   Use a subset for the problem at hand. 

vec_activities = c("Clearcut/Overstory Removal", "Commercial Thinning/Selective Cutting", "Salvage")

dat_owners_out =
  "03_intermediate/dat_notifications_1_2.csv" %>%
  read_csv %>%
  filter(ActivityType %in% vec_activities) %>% 
  select(Landowner_Company) %>%
  distinct %>%
  arrange(Landowner_Company) %T>%
  write_xlsx("03_intermediate/dat_owners_out.xlsx")

#  Import reviewed notifications.

#   This only keeps landowners that are (1) reviewed and (2) companies. 

dat_owners_in = 
  "03_intermediate/dat_owners_in.xlsx" %>% 
  read_xlsx %>%
  filter(Landowner_Private == 1) %>%
  drop_na(Landowner_Company_Reviewed) %>%
  select(1:2)

dat_notifications = 
  "03_intermediate/dat_notifications_1_2.gdb" %>% 
  vect %>% 
  filter(ActivityType %in% vec_activities) %>% 
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
