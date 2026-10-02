# Set up notifications and land portfolios for estimation.

#  Clear the environment.

rm(list = ls())

#  Get portfolios. 

dat_portfolios = 
  "03_intermediate/dat_portfolios.gdb" %>% 
  vect %>% 
  as_tibble %>% 
  distinct(Owner_Cotality, Year_Quarter) %>% 
  # Note quick, incorrect fix for extra rows. 
  left_join(
    "03_intermediate/dat_portfolios_covariates_invariant.csv" %>% 
      read_csv %>% 
      group_by(Owner_Cotality, Year_Quarter) %>% 
      filter(row_number() == 1) %>% 
      ungroup) %>% 
  left_join("03_intermediate/dat_portfolios_covariates_treemap.csv" %>% read_csv) %>% 
  left_join("03_intermediate/dat_portfolios_covariates_variant.csv" %>% read_csv) %>% 
  # Clean out less useful district and county covariates.
  select(-starts_with(c("District", "County"))) %>% 
  select(-starts_with(c("Pyrome_Marine", "Pyrome_NA"))) %>% 
  select(-ends_with("_Area")) %>% 
  # Reduce lagged covariates to useful means.
  # Price_Stumpage_DouglasFir
  mutate(
    Price_Stumpage_DouglasFir_Mean = 
      rowMeans(
        pick(
          starts_with("Price_Stumpage_DouglasFir_Lag_") & ends_with(as.character(21:32))),
        na.rm = TRUE
      )
  ) %>% 
  # Price_Stumpage_WesternHemlock
  mutate(
    Price_Stumpage_WesternHemlock_Mean = 
      rowMeans(
        pick(
          starts_with("Price_Stumpage_WesternHemlock_Lag_") & ends_with(as.character(21:32))),
        na.rm = TRUE
      )
  ) %>% 
  # Rate
  mutate(
    Rate_Mean = 
      rowMeans(
        pick(
          starts_with("Rate_Lag_") & ends_with(as.character(1:8))),
        na.rm = TRUE
      )
  ) %>% 
  # MTBS
  mutate(
    Fire_0_Mean_4 = 
      rowMeans(
        pick(
          starts_with("Fire_0_Lag") & ends_with(as.character(1:4))),
        na.rm = TRUE
      ),
    Fire_0_Mean_8 = 
      rowMeans(
        pick(
          starts_with("Fire_0_Lag") & ends_with(as.character(5:8))),
        na.rm = TRUE
      ),
    Fire_0_Mean_12 = 
      rowMeans(
        pick(
          starts_with("Fire_0_Lag") & ends_with(as.character(9:12))),
        na.rm = TRUE
      ),
    Fire_0_Mean_16 = 
      rowMeans(
        pick(
          starts_with("Fire_0_Lag") & ends_with(as.character(13:16))),
        na.rm = TRUE
      ),
    Fire_0_Mean_20 = 
      rowMeans(
        pick(
          starts_with("Fire_0_Lag") & ends_with(as.character(17:20))),
        na.rm = TRUE
      ),
    Fire_15_Doughnut_Mean_4 = 
      rowMeans(
        pick(
          starts_with("Fire_15_Doughnut_Lag") & ends_with(as.character(1:4))),
        na.rm = TRUE
      ),
    Fire_15_Doughnut_Mean_8 = 
      rowMeans(
        pick(
          starts_with("Fire_15_Doughnut_Lag") & ends_with(as.character(5:8))),
        na.rm = TRUE
      ),
    Fire_15_Doughnut_Mean_12 = 
      rowMeans(
        pick(
          starts_with("Fire_15_Doughnut_Lag") & ends_with(as.character(9:12))),
        na.rm = TRUE
      ),
    Fire_15_Doughnut_Mean_16 = 
      rowMeans(
        pick(
          starts_with("Fire_15_Doughnut_Lag") & ends_with(as.character(13:16))),
        na.rm = TRUE
      ),
    Fire_15_Doughnut_Mean_20 = 
      rowMeans(
        pick(
          starts_with("Fire_15_Doughnut_Lag") & ends_with(as.character(17:20))),
        na.rm = TRUE
      ),
    Fire_30_Doughnut_Mean_4 = 
      rowMeans(
        pick(
          starts_with("Fire_30_Doughnut_Lag") & ends_with(as.character(1:4))),
        na.rm = TRUE
      ),
    Fire_30_Doughnut_Mean_8 = 
      rowMeans(
        pick(
          starts_with("Fire_30_Doughnut_Lag") & ends_with(as.character(5:8))),
        na.rm = TRUE
      ),
    Fire_30_Doughnut_Mean_12 = 
      rowMeans(
        pick(
          starts_with("Fire_30_Doughnut_Lag") & ends_with(as.character(9:12))),
        na.rm = TRUE
      ),
    Fire_30_Doughnut_Mean_16 = 
      rowMeans(
        pick(
          starts_with("Fire_30_Doughnut_Lag") & ends_with(as.character(13:16))),
        na.rm = TRUE
      ),
    Fire_30_Doughnut_Mean_20 = 
      rowMeans(
        pick(
          starts_with("Fire_30_Doughnut_Lag") & ends_with(as.character(17:20))),
        na.rm = TRUE
      )
  ) %>% 
  # VPD
  mutate(
    VPD_Mean = 
      rowMeans(
        pick(
          starts_with("VPD_Lag_") & ends_with(as.character(1:24))),
        na.rm = TRUE
      )
  ) %>% 
  # CWD
  mutate(
    CWD_Mean = 
      rowMeans(
        pick(
          starts_with("CWD_Lag_") & ends_with(as.character(1:24))),
        na.rm = TRUE
      )
  ) %>% 
  select(-ends_with(paste0("Lag_", 0:120)))

#  Get notifications. 

dat_implicit = 
  "03_intermediate/dat_notifications_1_9.csv" %>% 
  read_csv %>% 
  mutate(Landowner = Owner_Cotality_Frequent) %>% # Patch for modifications to land ownership workflow. 
  group_by(Landowner) %>% # Reuse this on the full panel.
  mutate(Landowner_ID = cur_group_id()) %>%
  ungroup %>%
  relocate(Landowner_ID, .after = "UID") %>%
  # Handle continuous variables. Assign production-weighted means by firm. 
  rename(MBF_DouglasFir = MBF_2_DouglasFir,
         MBF_WesternHemlock = MBF_2_WesternHemlock,
         Acres = Acres_1) %>% 
  mutate(MBF_Both = MBF_DouglasFir + MBF_WesternHemlock) %>% 
  group_by(Landowner, Landowner_ID, QuarterCompletion) %>% 
  # Note that sums follow means to avoid quiet failure on weighting by a sum. 
  summarize(Count = n(),
            across(
              c(MBF_DouglasFir,
                MBF_WesternHemlock,
                MBF_Both,
                Acres),
              ~ sum(.x, na.rm = TRUE))) %>%
  ungroup %>% 
  # Sneak in quarter and year variables.
  # mutate(Year = QuarterCompletion %>% str_split_i("_", 1) %>% as.numeric,
  #        Quarter = QuarterCompletion %>% str_split_i("_", 2) %>% as.numeric) %>% 
  # relocate(c(Year, Quarter), .after = "QuarterCompletion") %>% 
  rename(Year_Quarter = QuarterCompletion)

#  Join notifications to portfolios.

dat_panel = 
  dat_portfolios %>% 
  rename(Landowner = Owner_Cotality) %>% 
  left_join(dat_implicit) %>% 
  group_by(Landowner) %>% 
  mutate(Landowner_ID = mean(Landowner_ID, na.rm = TRUE)) %>% 
  ungroup %>% 
  drop_na(Landowner_ID) %>% 
  mutate(across(c(starts_with("MBF"), Acres, Count), ~ replace_na(.x, 0))) %>% 
  mutate(Acres_Owned = Area * 0.00024711) %>% 
  group_by(Landowner) %>% 
  mutate(Landowner_SFO = (max(Acres_Owned) < 5000)) %>% 
  ungroup %>% 
  select(
    Landowner_ID,
    Landowner,
    Landowner_SFO,
    Year_Quarter,
    starts_with("MBF"),
    Acres_Owned, 
    Acres_Harvested = Acres,
    Count,
    starts_with("Price"),
    Rate_Mean,
    Elevation,
    Slope,
    Site_Class = SiteClassMode,
    Proportion_DouglasFir = ProportionDouglasFirTree,
    starts_with("Distance"),
    starts_with("Pyrome"),
    VPD_Mean,
    CWD_Mean,
    starts_with("Fire")
  )

# Export.

dat_panel %>% filter(MBF_Both > 0) %T>% write_csv("03_intermediate/dat_panel_portfolios_implicit.csv")
dat_panel %>% mutate(MBF_Bin = (MBF_Both > 0)) %>% relocate(MBF_Bin, .before = MBF_Both) %T>% write_csv("03_intermediate/dat_panel_portfolios_explicit.csv")
