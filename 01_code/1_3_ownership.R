# Handle land ownership. 

#  Clear the environment.

rm(list = ls())

#  Start timing. 

time_start = Sys.time()

#  Export notifications for review. 

dat_owners_out = 
  "03_intermediate/dat_notifications_1_2.csv" %>% 
  read_csv %>% 
  select(starts_with("Landowner"),
         starts_with("Timberowner")) %>% 
  distinct %>% 
  arrange(Landowner_Company,
          Timberowner_Company) %T>% 
  write_xlsx("03_intermediate/dat_owners_out.xlsx")

# 
# dat_join_owner = 
#   "output/landowners.xlsx" %>% 
#   read_xlsx %>% 
#   mutate(LandOwnerCompany = ifelse(is.na(LandOwnerCompany), "", LandOwnerCompany),
#          LandOwnerCompany = ifelse(LandOwnerCompany == "NA", "", LandOwnerCompany)) %>% 
#   left_join(dat_notifications %>% 
#               select(UID, LandOwner, LandOwnerCompany) %>% 
#               as_tibble %>% 
#               mutate(LandOwnerCompany = ifelse(is.na(LandOwnerCompany), "", LandOwnerCompany),
#                      LandOwnerCompany = ifelse(LandOwnerCompany == "NA", "", LandOwnerCompany)), 
#             .) %>% 
#   select(UID, Company = Company_2)
# 
# dat_notifications = dat_notifications %>% select(-LandOwnerCompany)

dat_notifications = 
  # read in notifications with spatial data
  # join on owner information
  # Export with spatial data. 
  writeVector("03_intermediate/dat_notifications_1_3.gdb") %>% 
  # Export without spatial data. 
  as_tibble %T>% 
  write_csv("03_intermediate/dat_notifications_1_3.csv")

#  Stop timing. 

time_end = Sys.time()

time_end - time_start