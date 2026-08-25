# Wrangle prices. 

#  Stumpage via FastMarkets

dat_price_stumpage = 
  "02_data/1_7_3_FastMarkets/data_stumpage.csv" %>% 
  read_csv %>% 
  rename(Price_Stumpage_DouglasFir = 2,
         Price_Stumpage_WesternHemlock = 3) %>% 
  mutate(Year_Quarter = 
           paste0(str_sub(Quarter, 1, 4), 
                  "_", 
                  str_sub(Quarter, -2, -1))) %>% 
  select(Year_Quarter, starts_with("Price_Stumpage_")) %>% 
  filter(Year_Quarter > "2004_Q4" & Year_Quarter < "2025_Q1") %T>% 
  write_csv("03_intermediate/data_stumpage.csv")

dat_price_lumber =
  "02_data/1_7_3_FastMarkets/data_lumber.csv" %>%
  read_csv %>%
  rename(Logs_DouglasFir_Sawmill_2_Columbia = 2,
         Logs_DouglasFir_Sawmill_3_Columbia = 3,
         Logs_DouglasFir_Sawmill_4_Columbia = 4,
         Logs_DouglasFir_Sawmill_2_Southern = 5,
         Logs_DouglasFir_Sawmill_3_Southern = 6,
         Logs_DouglasFir_Sawmill_4_Southern = 7,
         Logs_DouglasFir_Pulp_Southern = 8,
         Lumber_HemFir_Kiln_2x6_20 = 9,
         Lumber_HemFir_Kiln_2x8_20 = 10,
         Lumber_HemFir_Kiln_2x10_20 = 11,
         Lumber_HemFir_Kiln_2x12_20 = 12,
         Lumber_HemFir_Kiln_2x6_RL = 13,
         Lumber_HemFir_Kiln_2x8_RL = 14,
         Lumber_HemFir_Kiln_2x10_RL = 15,
         Lumber_HemFir_Kiln_2x12_RL = 16,
         Lumber_DouglasFir_Kiln_2x6_RL = 17,
         Lumber_DouglasFir_Kiln_2x8_RL = 18,
         Lumber_DouglasFir_Kiln_2x10_RL = 19,
         Lumber_DouglasFir_Kiln_2x12_RL = 20,
         Lumber_DouglasFir_Green_2x6_20 = 21,
         Lumber_DouglasFir_Green_2x8_20 = 22,
         Lumber_DouglasFir_Green_2x10_20 = 23,
         Lumber_DouglasFir_Green_2x12_20 = 24,
         Lumber_DouglasFir_Green_2x6_RL = 25,
         Lumber_DouglasFir_Green_2x8_RL = 26,
         Lumber_DouglasFir_Green_2x10_RL = 27,
         Lumber_DouglasFir_Green_2x12_RL = 28,
         Composite_Framing = 29,
         Composite_Dimension = 30,
         Composite_Stud = 31,
         Composite_DimensionLowGrade = 32,
         Composite_Board = 33,
         Composite_GreenDouglasFir = 34) %>% 
  mutate(Year = Date %>% year,
         Month = Date %>% month,
         Quarter = Month %>% multiply_by(1 / 3) %>% ceiling,
         Year_Quarter = paste0(Year, "_Q", Quarter)) %>% 
  select(-Year, -Month, -Quarter) %>% 
  filter(Year_Quarter > "2004_Q4" & Year_Quarter < "2025_Q1") %>% 
  group_by(Year_Quarter) %>% 
  summarize(across(everything(), ~ mean(.x, na.rm = TRUE))) %>% 
  # ungroup %>% 
  group_by(Year_Quarter) %>% 
  mutate(Price_Logs_DouglasFir_Sawmill = 
           mean(
             c(Logs_DouglasFir_Sawmill_2_Columbia,
               Logs_DouglasFir_Sawmill_3_Columbia, 
               Logs_DouglasFir_Sawmill_4_Columbia,
               Logs_DouglasFir_Sawmill_2_Southern,
               Logs_DouglasFir_Sawmill_3_Southern, 
               Logs_DouglasFir_Sawmill_4_Southern),
             na.rm = TRUE
           ),
         Price_Logs_DouglasFir_Pulp = Logs_DouglasFir_Pulp_Southern,
         Price_Lumber_HemFir_Kiln_20 = 
           mean(
             c(
               Lumber_HemFir_Kiln_2x6_20,
               Lumber_HemFir_Kiln_2x8_20,
               Lumber_HemFir_Kiln_2x10_20,
               Lumber_HemFir_Kiln_2x12_20
             ),
             na.rm = TRUE
           ),
         Price_Lumber_HemFir_Kiln_RL = 
           mean(
             c(
               Lumber_HemFir_Kiln_2x6_RL,
               Lumber_HemFir_Kiln_2x8_RL,
               Lumber_HemFir_Kiln_2x10_RL,
               Lumber_HemFir_Kiln_2x12_RL
             ),
             na.rm = TRUE
           ),
         Price_Lumber_DouglasFir_Kiln_RL = 
           mean(
             c(
               Lumber_DouglasFir_Kiln_2x6_RL,
               Lumber_DouglasFir_Kiln_2x8_RL,
               Lumber_DouglasFir_Kiln_2x10_RL,
               Lumber_DouglasFir_Kiln_2x12_RL
             ),
             na.rm = TRUE
           ),
         Price_Lumber_DouglasFir_Green_20 = 
           mean(
             c(
               Lumber_DouglasFir_Green_2x6_20,
               Lumber_DouglasFir_Green_2x8_20,
               Lumber_DouglasFir_Green_2x10_20,
               Lumber_DouglasFir_Green_2x12_20
             ),
             na.rm = TRUE
           ),
         Price_Lumber_DouglasFir_Green_RL = 
           mean(
             c(
               Lumber_DouglasFir_Green_2x6_RL,
               Lumber_DouglasFir_Green_2x8_RL,
               Lumber_DouglasFir_Green_2x10_RL,
               Lumber_DouglasFir_Green_2x12_RL
             ),
             na.rm = TRUE
           ),
         Price_Composite = 
           mean(
             c(
               Composite_Framing,
               Composite_Dimension,
               Composite_Stud,
               Composite_DimensionLowGrade,
               Composite_Board
             )
           ),
         Price_Composite_DouglasFir_Green = Composite_GreenDouglasFir
  ) %>% 
  ungroup %>% 
  select(Year_Quarter, starts_with("Price")) %T>% 
  write_csv("03_intermediate/data_lumber.csv")
