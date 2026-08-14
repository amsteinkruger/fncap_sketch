# Wrangle prices. 

#  Stumpage via FastMarkets

dat_price_stumpage = 
  "02_data/1_7_3_FastMarkets/data_stumpage.csv" %>% 
  read_csv %>% 
  rename(Stumpage_DouglasFir = 2,
         Stumpage_WesternHemlock = 3) %>% 
  mutate(Year_Quarter = 
           paste0(str_sub(Quarter, 1, 4), 
                  "_", 
                  str_sub(Quarter, -2, -1))) %>% 
  select(Year_Quarter, starts_with("Stumpage_")) %>% 
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

# Reference Plot

vis_price_test = 
  dat_price_stumpage %>% 
  left_join(dat_price_lumber) %>% 
  pivot_longer(-Year_Quarter) %>% 
  mutate(DouglasFir = ifelse(str_detect(name, "DouglasFir"), "Douglas Fir", "Western hemlock")) %>% 
  ggplot() +
  geom_line(aes(x = Year_Quarter,
                y = value,
                group = name,
                color = name)) +
  facet_wrap(~ DouglasFir)

# vis_price_test = 
#   dat_price_test %>% 
#   filter(Commodity %in% c("Logs", "Lumber/Sawn Timber")) %>% 
#   ggplot() + 
#   geom_vline(xintercept = "2008",
#              color = "red",
#              linetype = "dashed") +
#   geom_vline(xintercept = "2020", 
#              color = "red",
#              linetype = "dashed") +
#   geom_boxplot(aes(x = Year %>% factor,
#                    y = Price %>% log,
#                    color = Commodity),
#                alpha = 0.75) +
#   scale_x_discrete(breaks = c("2000", "2010", "2020")) +
#   scale_color_manual(values = c("gray40", "gray20")) +
#   labs(x = "Year",
#        y = "Price (Nominal) (Log.)") +
#   facet_wrap(~ Commodity) +
#   theme_minimal() +
#   theme(legend.position = "none")

# ggsave("04_out/vis_price_20260401.png",
#        vis_price_test,
#        dpi = 300,
#        width = 6,
#        height = 4)
