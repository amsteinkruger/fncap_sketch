# Estimate simple linear models of MBF/Acre on a kitchen sink of variables.

dat = 
  "03_intermediate/dat_notifications_1_9.csv" %>% 
  read_csv %>% 
  # Reduce to clearcuts.
  filter(str_sub(Activity, 1, 5) == "Clear") %>% 
  # Get a species-agnostic yield variable.
  mutate(MBF_Acre_Both = MBF_Acre_2_DouglasFir + MBF_Acre_2_WesternHemlock) %>% 
  # Get landowner percentiles by total production of both species.
  group_by(Landowner) %>% 
  mutate(MBF_Both_Total = sum(MBF_2_DouglasFir) + sum(MBF_2_WesternHemlock)) %>% 
  ungroup %>% 
  mutate(Landowner_MBF_Percentile = ntile(MBF_Both_Total, 100)) %>% 
  # Get factors for some categorical variables.
  mutate(Activity = Activity %>% factor,
         Pyrome = Pyrome %>% factor,
         County = County %>% factor,
         District = District %>% factor)

#   Get acres and yield of (observed) standing timber. This is poorly implemented.

dat_standing = 
  "03_intermediate/dat_notifications_1_9.csv" %>% 
  read_csv %>% 
  group_by(Landowner, QuarterCompletion) %>% 
  summarize(MBF_Standing = sum(MBF_2_DouglasFir) + sum(MBF_2_WesternHemlock),
            Acres_Standing = Acres_1 %>% sum) %>% 
  ungroup %>% 
  arrange(Landowner, desc(QuarterCompletion)) %>% 
  group_by(Landowner) %>% 
  mutate(MBF_Standing_Forward = cumsum(MBF_Standing) - MBF_Standing,
         Acres_Standing_Forward = cumsum(Acres_Standing) - Acres_Standing) %>% 
  ungroup %>% 
  select(Landowner, 
         QuarterCompletion, 
         MBF_Standing_Forward, 
         Acres_Standing_Forward)

fun_standing = 
  function(lastquarter, thisquarter){
    
    ifelse((is.na(lastquarter) | (lastquarter == 0)) & is.na(thisquarter), 
           0, 
           ifelse(!is.na(lastquarter) & is.na(thisquarter), 
                  lastquarter,
                  thisquarter))
    
  }

dat_standing_explicit =
  dat_standing %>%
  select(Landowner, QuarterCompletion) %>%
  complete(Landowner, QuarterCompletion) %>%
  left_join(dat_standing) %>%
  arrange(Landowner, desc(QuarterCompletion)) %>%
  group_by(Landowner) %>%
  mutate(MBF_Standing_Forward = accumulate(MBF_Standing_Forward, ~ fun_standing(.x, .y)) %>% replace_na(0),
         Acres_Standing_Forward = accumulate(Acres_Standing_Forward, ~ fun_standing(.x, .y)) %>% replace_na(0)) %>% 
  ungroup %>% 
  select(Landowner, QuarterCompletion, MBF_Standing_Forward, Acres_Standing_Forward) %>% 
  rename(Landowner_MBF_Standing = MBF_Standing_Forward,
         Landowner_Acres_Standing = Acres_Standing_Forward)

# Add explicit standing timber to other data.

dat_use = dat %>% left_join(dat_standing_explicit)

# Add means (1-year, 5-year, 10-year).
#  There are a couple smarter ways to do this:
#   Go long
#   Go wide with a custom function

dat_use_really =
  dat_use %>% 
  # Price_Stumpage_DouglasFir
  mutate(
    Price_Stumpage_DouglasFir_Mean_1Y = 
      mean(
        c(Price_Stumpage_DouglasFir_Lag_1,
          Price_Stumpage_DouglasFir_Lag_2,
          Price_Stumpage_DouglasFir_Lag_3,
          Price_Stumpage_DouglasFir_Lag_4),
        na.rm = TRUE),
    Price_Stumpage_DouglasFir_Mean_5Y = 
      rowMeans(
        pick(
          starts_with("Price_Stumpage_DouglasFir_Lag_") & ends_with(as.character(1:20))),
        na.rm = TRUE
      ),
    Price_Stumpage_DouglasFir_Mean_10Y = 
      rowMeans(
        pick(
          starts_with("Price_Stumpage_DouglasFir_Lag_") & ends_with(as.character(1:40))),
        na.rm = TRUE
      )
  ) %>% 
  # Price_Stumpage_WesternHemlock
  mutate(
    Price_Stumpage_WesternHemlock_Mean_1Y = 
      mean(
        c(Price_Stumpage_WesternHemlock_Lag_1,
          Price_Stumpage_WesternHemlock_Lag_2,
          Price_Stumpage_WesternHemlock_Lag_3,
          Price_Stumpage_WesternHemlock_Lag_4),
        na.rm = TRUE),
    Price_Stumpage_WesternHemlock_Mean_5Y = 
      rowMeans(
        pick(
          starts_with("Price_Stumpage_WesternHemlock_Lag_") & ends_with(as.character(1:20))),
        na.rm = TRUE
      ),
    Price_Stumpage_WesternHemlock_Mean_10Y = 
      rowMeans(
        pick(
          starts_with("Price_Stumpage_WesternHemlock_Lag_") & ends_with(as.character(1:40))),
        na.rm = TRUE
      )
  ) %>% 
  # Price_Logs_DouglasFir_Sawmill
  mutate(
    Price_Logs_DouglasFir_Sawmill_Mean_1Y = 
      mean(
        c(Price_Logs_DouglasFir_Sawmill_Lag_1,
          Price_Logs_DouglasFir_Sawmill_Lag_2,
          Price_Logs_DouglasFir_Sawmill_Lag_3,
          Price_Logs_DouglasFir_Sawmill_Lag_4),
        na.rm = TRUE),
    Price_Logs_DouglasFir_Sawmill_Mean_5Y = 
      rowMeans(
        pick(
          starts_with("Price_Logs_DouglasFir_Sawmill_Lag_") & ends_with(as.character(1:20))),
        na.rm = TRUE
      ),
    Price_Logs_DouglasFir_Sawmill_Mean_10Y = 
      rowMeans(
        pick(
          starts_with("Price_Logs_DouglasFir_Sawmill_Lag_") & ends_with(as.character(1:40))),
        na.rm = TRUE
      )
  ) %>% 
  # Price_Logs_DouglasFir_Pulp
  mutate(
    Price_Logs_DouglasFir_Pulp_Mean_1Y = 
      mean(
        c(Price_Logs_DouglasFir_Pulp_Lag_1,
          Price_Logs_DouglasFir_Pulp_Lag_2,
          Price_Logs_DouglasFir_Pulp_Lag_3,
          Price_Logs_DouglasFir_Pulp_Lag_4),
        na.rm = TRUE),
    Price_Logs_DouglasFir_Pulp_Mean_5Y = 
      rowMeans(
        pick(
          starts_with("Price_Logs_DouglasFir_Pulp_Lag_") & ends_with(as.character(1:20))),
        na.rm = TRUE
      ),
    Price_Logs_DouglasFir_Pulp_Mean_10Y = 
      rowMeans(
        pick(
          starts_with("Price_Logs_DouglasFir_Pulp_Lag_") & ends_with(as.character(1:40))),
        na.rm = TRUE
      )
  ) %>% 
  # Price_Lumber_HemFir_Kiln_20
  mutate(
    Price_Lumber_HemFir_Kiln_20_Mean_1Y = 
      mean(
        c(Price_Lumber_HemFir_Kiln_20_Lag_1,
          Price_Lumber_HemFir_Kiln_20_Lag_2,
          Price_Lumber_HemFir_Kiln_20_Lag_3,
          Price_Lumber_HemFir_Kiln_20_Lag_4),
        na.rm = TRUE),
    Price_Lumber_HemFir_Kiln_20_Mean_5Y = 
      rowMeans(
        pick(
          starts_with("Price_Lumber_HemFir_Kiln_20_Lag_") & ends_with(as.character(1:20))),
        na.rm = TRUE
      ),
    Price_Lumber_HemFir_Kiln_20_Mean_10Y = 
      rowMeans(
        pick(
          starts_with("Price_Lumber_HemFir_Kiln_20_Lag_") & ends_with(as.character(1:40))),
        na.rm = TRUE
      )
  ) %>% 
  # Price_Lumber_HemFir_Kiln_RL
  mutate(
    Price_Lumber_HemFir_Kiln_RL_Mean_1Y = 
      mean(
        c(Price_Lumber_HemFir_Kiln_RL_Lag_1,
          Price_Lumber_HemFir_Kiln_RL_Lag_2,
          Price_Lumber_HemFir_Kiln_RL_Lag_3,
          Price_Lumber_HemFir_Kiln_RL_Lag_4),
        na.rm = TRUE),
    Price_Lumber_HemFir_Kiln_RL_Mean_5Y = 
      rowMeans(
        pick(
          starts_with("Price_Lumber_HemFir_Kiln_RL_Lag_") & ends_with(as.character(1:20))),
        na.rm = TRUE
      ),
    Price_Lumber_HemFir_Kiln_RL_Mean_10Y = 
      rowMeans(
        pick(
          starts_with("Price_Lumber_HemFir_Kiln_RL_Lag_") & ends_with(as.character(1:40))),
        na.rm = TRUE
      )
  ) %>% 
  # Price_Lumber_DouglasFir_Green_20
  mutate(
    Price_Lumber_DouglasFir_Green_20_Mean_1Y = 
      mean(
        c(Price_Lumber_DouglasFir_Green_20_Lag_1,
          Price_Lumber_DouglasFir_Green_20_Lag_2,
          Price_Lumber_DouglasFir_Green_20_Lag_3,
          Price_Lumber_DouglasFir_Green_20_Lag_4),
        na.rm = TRUE),
    Price_Lumber_DouglasFir_Green_20_Mean_5Y = 
      rowMeans(
        pick(
          starts_with("Price_Lumber_DouglasFir_Green_20_Lag_") & ends_with(as.character(1:20))),
        na.rm = TRUE
      ),
    Price_Lumber_DouglasFir_Green_20_Mean_10Y = 
      rowMeans(
        pick(
          starts_with("Price_Lumber_DouglasFir_Green_20_Lag_") & ends_with(as.character(1:40))),
        na.rm = TRUE
      )
  ) %>% 
  # Price_Lumber_DouglasFir_Green_RL
  mutate(
    Price_Lumber_DouglasFir_Green_RL_Mean_1Y = 
      mean(
        c(Price_Lumber_DouglasFir_Green_RL_Lag_1,
          Price_Lumber_DouglasFir_Green_RL_Lag_2,
          Price_Lumber_DouglasFir_Green_RL_Lag_3,
          Price_Lumber_DouglasFir_Green_RL_Lag_4),
        na.rm = TRUE),
    Price_Lumber_DouglasFir_Green_RL_Mean_5Y = 
      rowMeans(
        pick(
          starts_with("Price_Lumber_DouglasFir_Green_RL_Lag_") & ends_with(as.character(1:20))),
        na.rm = TRUE
      ),
    Price_Lumber_DouglasFir_Green_RL_Mean_10Y = 
      rowMeans(
        pick(
          starts_with("Price_Lumber_DouglasFir_Green_RL_Lag_") & ends_with(as.character(1:40))),
        na.rm = TRUE
      )
  ) %>% 
  # Price_Composite
  mutate(
    Price_Composite_Mean_1Y = 
      mean(
        c(Price_Composite_Lag_1,
          Price_Composite_Lag_2,
          Price_Composite_Lag_3,
          Price_Composite_Lag_4),
        na.rm = TRUE),
    Price_Composite_Mean_5Y = 
      rowMeans(
        pick(
          starts_with("Price_Composite_Lag_") & ends_with(as.character(1:20))),
        na.rm = TRUE
      ),
    Price_Composite_Mean_10Y = 
      rowMeans(
        pick(
          starts_with("Price_Composite_Lag_") & ends_with(as.character(1:40))),
        na.rm = TRUE
      )
  ) %>% 
  # Price_Composite_DouglasFir_Green
  mutate(
    Price_Composite_DouglasFir_Green_Mean_1Y = 
      mean(
        c(Price_Composite_DouglasFir_Green_Lag_1,
          Price_Composite_DouglasFir_Green_Lag_2,
          Price_Composite_DouglasFir_Green_Lag_3,
          Price_Composite_DouglasFir_Green_Lag_4),
        na.rm = TRUE),
    Price_Composite_DouglasFir_Green_Mean_5Y = 
      rowMeans(
        pick(
          starts_with("Price_Composite_DouglasFir_Green_Lag_") & ends_with(as.character(1:20))),
        na.rm = TRUE
      ),
    Price_Composite_DouglasFir_Green_Mean_10Y = 
      rowMeans(
        pick(
          starts_with("Price_Composite_DouglasFir_Green_Lag_") & ends_with(as.character(1:40))),
        na.rm = TRUE
      )
  ) %>% 
  # Rate
  mutate(
    Rate_Mean_1Y = 
      mean(
        c(Rate_Lag_1,
          Rate_Lag_2,
          Rate_Lag_3,
          Rate_Lag_4),
        na.rm = TRUE),
    Rate_Mean_5Y = 
      rowMeans(
        pick(
          starts_with("Rate_Lag_") & ends_with(as.character(1:20))),
        na.rm = TRUE
      ),
    Rate_Mean_10Y = 
      rowMeans(
        pick(
          starts_with("Rate_Lag_") & ends_with(as.character(1:40))),
        na.rm = TRUE
      )
  ) %>% 
  # VPD
  mutate(
    VPD_Mean_1Y = 
      mean(
        c(VPD_Lag_1,
          VPD_Lag_2,
          VPD_Lag_3,
          VPD_Lag_4),
        na.rm = TRUE),
    VPD_Mean_5Y = 
      rowMeans(
        pick(
          starts_with("VPD_Lag_") & ends_with(as.character(1:20))),
        na.rm = TRUE
      ),
    VPD_Mean_10Y = 
      rowMeans(
        pick(
          starts_with("VPD_Lag_") & ends_with(as.character(1:40))),
        na.rm = TRUE
      )
  ) %>% 
  # PPT
  mutate(
    PPT_Mean_1Y = 
      mean(
        c(PPT_Lag_1,
          PPT_Lag_2,
          PPT_Lag_3,
          PPT_Lag_4),
        na.rm = TRUE),
    PPT_Mean_5Y = 
      rowMeans(
        pick(
          starts_with("PPT_Lag_") & ends_with(as.character(1:20))),
        na.rm = TRUE
      ),
    PPT_Mean_10Y = 
      rowMeans(
        pick(
          starts_with("PPT_Lag_") & ends_with(as.character(1:40))),
        na.rm = TRUE
      )
  ) %>% 
  # TMean
  mutate(
    TMean_Mean_1Y = 
      mean(
        c(TMean_Lag_1,
          TMean_Lag_2,
          TMean_Lag_3,
          TMean_Lag_4),
        na.rm = TRUE),
    TMean_Mean_5Y = 
      rowMeans(
        pick(
          starts_with("TMean_Lag_") & ends_with(as.character(1:20))),
        na.rm = TRUE
      ),
    TMean_Mean_10Y = 
      rowMeans(
        pick(
          starts_with("TMean_Lag_") & ends_with(as.character(1:40))),
        na.rm = TRUE
      )
  ) %>% 
  # TMax
  mutate(
    TMax_Mean_1Y = 
      mean(
        c(TMax_Lag_1,
          TMax_Lag_2,
          TMax_Lag_3,
          TMax_Lag_4),
        na.rm = TRUE),
    TMax_Mean_5Y = 
      rowMeans(
        pick(
          starts_with("TMax_Lag_") & ends_with(as.character(1:20))),
        na.rm = TRUE
      ),
    TMax_Mean_10Y = 
      rowMeans(
        pick(
          starts_with("TMax_Lag_") & ends_with(as.character(1:40))),
        na.rm = TRUE
      )
  ) %>% 
  # CWD
  mutate(
    CWD_Mean_1Y = 
      mean(
        c(CWD_Lag_1,
          CWD_Lag_2,
          CWD_Lag_3,
          CWD_Lag_4),
        na.rm = TRUE),
    CWD_Mean_5Y = 
      rowMeans(
        pick(
          starts_with("CWD_Lag_") & ends_with(as.character(1:20))),
        na.rm = TRUE
      ),
    CWD_Mean_10Y = 
      rowMeans(
        pick(
          starts_with("CWD_Lag_") & ends_with(as.character(1:40))),
        na.rm = TRUE
      )
  )


  # Price_Logs_DouglasFir_Sawmill_Lag_1
  # Price_Logs_DouglasFir_Pulp_Lag_1
  # Price_Lumber_HemFir_Kiln_20_Lag_1
  # Price_Lumber_HemFir_Kiln_RL_Lag_1
  # Price_Lumber_DouglasFir_Green_20_Lag_1
  # Price_Lumber_DouglasFir_Green_RL_Lag_1
  # Price_Composite_Lag_1
  # Price_Composite_DouglasFir_Green_Lag_1
  # Rate_Lag_1
  # VPD_Lag_1
  # PPT_Lag_1
  # TMean_Lag_1
  # TMax_Lag_1
  # CWD_Lag_1
  
# Drop extraneous lags. 

# keep if it ends with _Lag_# or drop if it otherwise includes _Lag_
# love regex

# Estimate.

mod_1 = 
  dat_use_really %>% 
  feols(MBF_Acre_Both ~ 
          # Landowner-varying
          Landowner_MBF_Percentile +
          Landowner_MBF_Standing +
          # Space-varying
          SiteClassMode +
          Elevation +
          Slope + 
          Distance_Road +
          Distance_Mill +
          Distance_Place +
          ProportionDouglasFirTree +
          Acres_Riparian_Proportion +
          # Time-varying
          #  1-Quarter Lag
          # Price_Stumpage_DouglasFir_Lag_1 +
          # Price_Stumpage_WesternHemlock_Lag_1 +
          # Price_Logs_DouglasFir_Sawmill_Lag_1 +
          # Price_Logs_DouglasFir_Pulp_Lag_1 +
          # Price_Lumber_HemFir_Kiln_20_Lag_1 +
          # Price_Lumber_HemFir_Kiln_RL_Lag_1 +
          # Price_Lumber_DouglasFir_Green_20_Lag_1 +
          # Price_Lumber_DouglasFir_Green_RL_Lag_1 +
          # Price_Composite_Lag_1 +
          # Price_Composite_DouglasFir_Green_Lag_1 +
          # Rate_Lag_1 +
          #  1-Year Mean
          # Price_Stumpage_DouglasFir_Mean_1Y +
          # Price_Stumpage_WesternHemlock_Mean_1Y +
          # Price_Logs_DouglasFir_Sawmill_Mean_1Y +
          # Price_Logs_DouglasFir_Pulp_Mean_1Y +
          # Price_Lumber_HemFir_Kiln_20_Mean_1Y +
          # Price_Lumber_HemFir_Kiln_RL_Mean_1Y +
          # Price_Lumber_DouglasFir_Green_20_Mean_1Y +
          # Price_Lumber_DouglasFir_Green_RL_Mean_1Y +
          # Price_Composite_Mean_1Y +
          # Price_Composite_DouglasFir_Green_Mean_1Y +
          # Rate_Mean_1Y +
          #  5-Year Mean
          # Price_Stumpage_DouglasFir_Mean_5Y +
          # Price_Stumpage_WesternHemlock_Mean_5Y +
          # Price_Logs_DouglasFir_Sawmill_Mean_5Y +
          # Price_Logs_DouglasFir_Pulp_Mean_5Y +
          # Price_Lumber_HemFir_Kiln_20_Mean_5Y +
          # Price_Lumber_HemFir_Kiln_RL_Mean_5Y +
          # Price_Lumber_DouglasFir_Green_20_Mean_5Y +
          # Price_Lumber_DouglasFir_Green_RL_Mean_5Y +
          # Price_Composite_Mean_5Y +
          # Price_Composite_DouglasFir_Green_Mean_5Y +
          # Rate_Mean_5Y +
          #  10-Year Mean
          # Price_Stumpage_DouglasFir_Mean_10Y +
          # Price_Stumpage_WesternHemlock_Mean_10Y +
          # Price_Logs_DouglasFir_Sawmill_Mean_10Y +
          # Price_Logs_DouglasFir_Pulp_Mean_10Y +
          # Price_Lumber_HemFir_Kiln_20_Mean_10Y +
          # Price_Lumber_HemFir_Kiln_RL_Mean_10Y +
          # Price_Lumber_DouglasFir_Green_20_Mean_10Y +
          # Price_Lumber_DouglasFir_Green_RL_Mean_10Y +
          Price_Composite_Mean_10Y +
          # Price_Composite_DouglasFir_Green_Mean_10Y +
          Rate_Mean_10Y +
          # Time- and space-varying
          #  MTBS
          Fire_0_Lag_1 +
          Fire_15_Doughnut_Lag_1 +
          Fire_30_Doughnut_Lag_1 +
          Fire_Proportion_Lag_1 +
          #  PRISM
          #  1-Quarter Lag
          # VPD_Lag_1 +
          # PPT_Lag_1 +
          # TMean_Lag_1 +
          # TMax_Lag_1 +
          #  1-Year Mean
          # VPD_Mean_1Y +
          # PPT_Mean_1Y +
          # TMean_Mean_1Y +
          # TMax_Mean_1Y +
          #  5-Year Mean
          # VPD_Mean_5Y +
          # PPT_Mean_5Y +
          # TMean_Mean_5Y +
          # TMax_Mean_5Y +
          #  10-Year Mean
          VPD_Mean_10Y +
          PPT_Mean_10Y +
          TMean_Mean_10Y +
          TMax_Mean_10Y +
          # CWD
          #  1-Quarter Lag
          # CWD_Lag_1 +
          #  1-Year Mean
          # CWD_Mean_1Y +
          #  5-Year Mean
          # CWD_Mean_5Y +
          #  10-Year Mean
          CWD_Mean_10Y
          )

mod_2 = 
  dat_use_really %>% 
  feols(MBF_Acre_Both ~ 
          # Landowner-varying
          Landowner_MBF_Percentile +
          Landowner_MBF_Standing +
          # Space-varying
          SiteClassMode +
          Elevation +
          Slope + 
          Distance_Road +
          Distance_Mill +
          Distance_Place +
          ProportionDouglasFirTree +
          Acres_Riparian_Proportion +
          # Time-varying
          Price_Composite_Mean_10Y +
          Rate_Mean_10Y +
          # Time- and space-varying
          #  MTBS
          Fire_0_Lag_1 +
          Fire_15_Doughnut_Lag_1 +
          Fire_30_Doughnut_Lag_1 +
          Fire_Proportion_Lag_1 +
          #  PRISM
          VPD_Mean_10Y +
          PPT_Mean_10Y +
          TMean_Mean_10Y +
          TMax_Mean_10Y +
          # CWD
          CWD_Mean_10Y |
          County)

mod_3 = 
  dat_use_really %>% 
  feols(MBF_Acre_Both ~ 
          # Landowner-varying
          # Landowner_MBF_Percentile +
          # Landowner_MBF_Standing +
          # Space-varying
          SiteClassMode +
          Elevation +
          Slope + 
          Distance_Road +
          Distance_Mill +
          Distance_Place +
          ProportionDouglasFirTree +
          Acres_Riparian_Proportion +
          # Time-varying
          Price_Composite_Mean_10Y +
          Rate_Mean_10Y +
          # Time- and space-varying
          #  MTBS
          Fire_0_Lag_1 +
          Fire_15_Doughnut_Lag_1 +
          Fire_30_Doughnut_Lag_1 +
          Fire_Proportion_Lag_1 +
          #  PRISM
          VPD_Mean_10Y +
          PPT_Mean_10Y +
          TMean_Mean_10Y +
          TMax_Mean_10Y +
          # CWD
          CWD_Mean_10Y |
          County + Landowner)

modelsummary(
  list("No FE" = mod_1,
       "County FE" = mod_2,
       "County + Owner FE" = mod_3),
  stars = TRUE, 
  output = "flextable") |> 
  autofit() |> 
  save_as_docx(path = "04_out/Smorgasbord/tab_general.docx")

# Adding runs by county.

fun_model = 
  function(dat){
    
    model = 
      dat %>% 
      feols(MBF_Acre_Both ~ 
              # Landowner-varying
              Landowner_MBF_Percentile +
              Landowner_MBF_Standing +
              # Space-varying
              SiteClassMode +
              Elevation +
              Slope + 
              Distance_Road +
              Distance_Mill +
              Distance_Place +
              ProportionDouglasFirTree +
              Acres_Riparian_Proportion +
              # Time-varying
              Price_Composite_Mean_10Y +
              Rate_Mean_10Y +
              # Time- and space-varying
              #  MTBS
              Fire_0_Lag_1 +
              Fire_15_Doughnut_Lag_1 +
              Fire_30_Doughnut_Lag_1 +
              Fire_Proportion_Lag_1 +
              #  PRISM
              VPD_Mean_10Y +
              PPT_Mean_10Y +
              TMean_Mean_10Y +
              TMax_Mean_10Y +
              # CWD
              CWD_Mean_10Y)
    
    return(model)

  }

dat_county = 
  dat_use_really %>% 
  group_by(County) %>% 
  mutate(MBF_County = MBF_Both_Total %>% sum) %>% 
  group_by(County, MBF_County) %>% 
  nest %>% 
  ungroup %>% 
  arrange(desc(MBF_County)) %>% 
  slice_head(n = 3) %>% 
  mutate(path = 
           paste0(
             "04_out/Smorgasbord/Table_", 
             str_replace_all(County, " ", "_"),
             ".docx"),
         model = 
           data %>% 
           map(fun_model)) %>% 
  # This is where a group_by on activity or region definition would go.
  summarize(counties = list(County),
            models = list(model)) %>% 
  unnest(c(counties, models)) %>% 
  mutate(counties = counties %>% as.character)

mod_county =
  setNames(dat_county$models, dat_county$counties) %>% 
  modelsummary(
    stars = TRUE,
    output = "flextable"
  ) %>% 
  autofit %T>% 
  save_as_docx(path = "04_out/Smorgasbord/tab_counties.docx")

# model-by-model export         
         
         # export = 
         #   model %>% 
         #   map(modelsummary,
         #        stars = TRUE,
         #        output = "flextable") %>% 
         #   map(autofit) %>% 
         #   map2(.x = .,
         #        .y = path,
         #        .f = ~ save_as_docx(.x, path = .y)))

# Adding runs by pyrome.

dat_pyrome = 
  dat_use_really %>% 
  group_by(Pyrome) %>% 
  mutate(MBF_Pyrome = MBF_Both_Total %>% sum) %>% 
  group_by(Pyrome, MBF_Pyrome) %>% 
  nest %>% 
  ungroup %>% 
  arrange(desc(MBF_Pyrome)) %>% 
  slice_head(n = 3) %>% 
  mutate(path = 
           paste0(
             "04_out/Smorgasbord/Table_", 
             str_replace_all(Pyrome, " ", "_"),
             ".docx"),
         model = 
           data %>% 
           map(fun_model)) %>% 
  # This is where a group_by on activity or region definition would go.
  summarize(pyromes = list(Pyrome),
            models = list(model)) %>% 
  unnest(c(pyromes, models)) %>% 
  mutate(pyromes = pyromes %>% as.character)

mod_pyrome =
  setNames(dat_pyrome$models, dat_pyrome$pyromes) %>% 
  modelsummary(
    stars = TRUE,
    output = "flextable"
  ) %>% 
  autofit %T>% 
  save_as_docx(path = "04_out/Smorgasbord/tab_pyromes.docx")
