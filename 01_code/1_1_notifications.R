# Process FERNS. 

#  Problems: 
#   QA/QC on Activity_Completion_Status
#   Substitute for landowner type from original flat data

#  Clear the environment.

rm(list = ls())

# Start timing. 

time_start = Sys.time()

#  Get spatial data (polygons, excluding lines and points).  

# dat_spat_1 = "02_data/1_1_1_ODF_FERNS/Spatial/Polygons_20250820.gdb" %>% vect
dat_spat_2 = "02_data/1_1_1_ODF_FERNS/Spatial/Polygons_20260311.gdb" %>% vect

#   Note that the earlier file is a subset of the later file. 

dat_spat = 
  # This is where code to reconcile the two datasets would go. 
  dat_spat_2 %>% 
  mutate(LandOwner = LandOwners %>% str_trim,
         UID_Spat = row_number()) %>% 
  select(NOAPID = NoapIdenti,
         UnitID,
         UnitName,
         OperationName = OperationN, 
         ActivityType = ActType,
         DateSubmit = SubmitDate,
         DateStart = StartDate,
         DateEnd = EndDate,
         UID_Spat)

#  Get flat data.

# dat_flat_1 = "02_data/1_1_1_ODF_FERNS/Flat/Notifications_20250820.xlsx" %>% read_excel
# dat_flat_2 = "02_data/1_1_1_ODF_FERNS/Flat/Notifications_20251124.csv" %>% read_csv
# dat_flat_3 = "02_data/1_1_1_ODF_FERNS/Flat/Notifications_20260311.csv" %>% read_csv
dat_flat_4 = "02_data/1_1_1_ODF_FERNS/Flat/Notifications_20260401.csv" %>% read_csv

#   Functions to handle date strings. 

fun_zero = function(string){
  
  ifelse(string %>% nchar < 2, 
         paste0(0, string),
         string)
  
}

fun_date = function(date){
  
  ifelse(is.na(date), 
         NA,
         paste(str_split_i(date, "/", 3) %>% fun_zero,
               str_split_i(date, "/", 1) %>% fun_zero, 
               str_split_i(date, "/", 2) %>% fun_zero,
               sep = "-")) %>% 
    as.POSIXct(tz = "UTC")
  
}

dat_flat = 
  # This is where code to reconcile datasets would go. 
  dat_flat_4 %>% 
  mutate(across(ends_with("Date"), ~ fun_date(.x))) %>% 
  select(NOAPID = Noap_Identifier,
         UnitName = Unit_Name,
         OperationName = Operation_Name,
         DateSubmit = Submit_Date,
         DateStart = Activity_Start_Date,
         DateEnd = Activity_End_Date,
         DateCompletion = Activity_Completion_Date,
         ActivityStatus = Activity_Completion_Status, 
         ActivityType = Activity_Name,
         ActivityUnit = Quantity_Units,
         ActivityQuantity = Quantity, 
         Notifier = Notifier_Name,
         Notifier_Company = Notifier_Company_Name,
         Landowner = Landowner_Name,
         Landowner_Company = Landowner_Company_Name,
         Timberowner = Timber_Owner_Name,
         Timberowner_Company = Timber_Owner_Company_Name,
         Operator = Operator_Name,
         Operator_Company = Operator_Company_Name) %>% 
  distinct %>% 
  mutate(UID_Flat = row_number())

#   Note that 
#    (1) files obtained through FERNS share records for (e.g.) 2015 
#    (2) but have fewer records across years than the file obtained by hand. 

#  Join and export.

dat_join = 
  dat_spat %>% 
  inner_join(dat_flat,
             by = 
               join_by(NOAPID, 
                       UnitName, 
                       OperationName, 
                       ActivityType, 
                       DateSubmit, 
                       DateStart, 
                       DateEnd)) %>% 
  makeValid %>% # If placed before inner_join, makeValid breaks on oddball rows. 
  group_by(across(-UID_Spat)) %>% 
  tidyterra::summarize(UID_Spat = min(UID_Spat)) %>% # check whether aggregated areas are correct
  ungroup %>% 
  # Organize columns.
  select(NOAPID, 
         OperationName, 
         UnitName, 
         UnitID,
         ActivityType,
         ActivityStatus, 
         ActivityQuantity,
         ActivityUnit,
         DateSubmit,
         DateStart,
         DateEnd,
         DateCompletion,
         starts_with("Notifier"),
         starts_with("Operator"),
         starts_with("Landowner"),
         starts_with("Timberowner")) %>% 
  arrange(DateSubmit, NOAPID, ActivityType, OperationName, UnitName) %>% 
  mutate(UID = row_number()) %>% 
  relocate(UID) %T>% 
  # Export with spatial data. 
  writeVector("03_intermediate/dat_notifications_1_1.gdb") %>% 
  # Export without spatial data. 
  as_tibble %T>% 
  write_csv("03_intermediate/dat_notifications_1_1.csv")

# Stop timing. 

time_end = Sys.time()

time_end - time_start
