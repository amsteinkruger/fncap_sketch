# Process notification data.

#  Problems: 
#   1-N, N-1 matches between spatial and flat data
#   Understanding data burns and data in/exclusion in different FERNS versions
#   Interpreting Unit, Operation, . . .
#   QA/QC on Activity_Completion_Status
#   Substitute for landowner type from original flat data

#   To sort out N-1, aggregate after matching? 
#   And leave 1-N as real -- multiple activities, etc per spatial unit

#   Presently losing 20% of clearcut observations to non-/multi-matches. 

#  Get spatial data (polygons, excluding lines and points).  

dat_spat_1 = "02_data/ODF_FERNS/Spatial/Polygons_20250820.gdb" %>% vect
dat_spat_2 = "02_data/ODF_FERNS/Spatial/Polygons_20260311.gdb" %>% vect

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

dat_flat_1 = "02_data/ODF_FERNS/Flat/Notifications_20250820.xlsx" %>% read_excel
dat_flat_2 = "02_data/ODF_FERNS/Flat/Notifications_20251124.csv" %>% read_csv
dat_flat_3 = "02_data/ODF_FERNS/Flat/Notifications_20260311.csv" %>% read_csv
dat_flat_4 = "02_data/ODF_FERNS/Flat/Notifications_20260401.csv" %>% read_csv

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
  mutate(across(ends_with("Date"), ~ fun_date(.x)),
         UID_Flat = row_number()) %>% 
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
         Operator_Company = Operator_Company_Name,
         UID_Flat)

#   Note that 
#    (1) files obtained through FERNS share records for (e.g.) 2015 
#    (2) but have fewer records across years than the file obtained by hand. 

#  Join and export.

dat_join = 
  left_join(dat_spat,
            dat_flat,
            by = 
              join_by(NOAPID, 
                      UnitName, 
                      OperationName, 
                      ActivityType, 
                      DateSubmit, 
                      DateStart, 
                      DateEnd)) %>% 
  # Drop non-matches and multi-matches. 
  filter(!is.na(UID_Flat)) %>% 
  group_by(UID_Spat) %>% 
  filter(n() == 1) %>% 
  group_by(UID_Flat) %>% 
  filter(n() == 1) %>% 
  ungroup %>% 
  # Pick and reorder columns.
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
  arrange(DateSubmit, NOAPID, OperationName, UnitName, ActivityType) %T>% 
  # Export with spatial data. 
  writeVector("03_intermediate/dat_notifications.gdb") %>% 
  # Export without spatial data. 
  as_tibble %T>% 
  write_csv("03_intermediate/dat_notifications.csv")
