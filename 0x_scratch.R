# Check whether owner strings match.

#  Note that this does not fit into the current progression of scripts due to 01-05 shenanigans.
#  Note also that this does not restrict the data to western OR (which should be fine).

dat_notifications_strings = 
  read_excel("data/Flat_Notifications.xlsx") %>% 
  distinct %>% 
  filter(`Activity Type` == "Clearcut/Overstory Removal") %>% 
  filter(`Activity Units` == "MBF") %>% 
  filter(LandOwnerType == "Partnership/Corporate Forestland Ownership") %>% 
  select(ID = NoapIdentifier,
         starts_with("LO"),
         LandOwnerType,
         LandOwnerSize) 
  
dat_notifications_strings_all = 
  dat_notifications_strings %>% 
  group_by(LOFirstName,
           LOMiddleName,
           LOLastName,
           `LO Company Name`,
           LOCity,
           LOState,
           LandOwnerType,
           LandOwnerSize) %>% 
  summarize(count = n()) %>% 
  ungroup %>% 
  arrange(desc(count))

dat_notifications_strings_all$count %>% hist

dat_notifications_strings_company = 
  dat_notifications_strings %>% 
  filter(`LO Company Name` != "NULL") %>% # Lose ~4,000/22,000.
  mutate(name = 
           `LO Company Name` %>% 
           str_replace_all("[[:punct:]]", "") %>% 
           str_replace_all(" ", "") %>% 
           str_to_lower) %>% 
  group_by(name) %>% 
  summarize(count = n()) %>% 
  ungroup %>% 
  arrange(desc(count))

dat_notifications_strings_company$count %>% hist
dat_notifications_strings_company$count %>% log %>%  hist
