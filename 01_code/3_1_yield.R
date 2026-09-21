# Estimate simple linear models of MBF/Acre on a kitchen sink of variables.

dat = 
  "03_intermediate/dat_notifications_1_9.csv" %>% 
  read_csv %>% 
  # Reduce to clearcuts.
  filter(str_sub(Activity, 1, 5) == "Clear") %>% 
  # Get a species-agnostic yield variable.
  mutate(MBF_Acre_Both = MBF_Acre_2_DouglasFir + MBF_Acre_2_WesternHemlock) %>% 
  # Get landowner percentiles by total production of both species.
  group_by(Owner_Cotality_Frequent) %>% 
  mutate(MBF_Both_Total = sum(MBF_2_DouglasFir) + sum(MBF_2_WesternHemlock)) %>% 
  ungroup %>% 
  mutate(Landowner_MBF_Percentile = ntile(MBF_Both_Total, 100)) %>% 
  # Get factors for some categorical variables.
  mutate(Activity = Activity %>% factor,
         Pyrome = Pyrome %>% factor,
         County = County %>% factor,
         District = District %>% factor) %>% 
  # Alias the preferred landowner column for convenient references.
  mutate(Landowner = Owner_Cotality_Frequent)

#   Get acres and yield of (observed) standing timber. This is poorly implemented.

dat_standing = 
  "03_intermediate/dat_notifications_1_9.csv" %>% 
  read_csv %>% 
  mutate(Landowner = Owner_Cotality_Frequent) %>% 
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
    Price_Stumpage_DouglasFir_Mean = 
      rowMeans(
        pick(
          starts_with("Price_Stumpage_DouglasFir_Lag_") & ends_with(as.character(21:32))),
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
  # CWD
  mutate(
    CWD_Mean = 
      rowMeans(
        pick(
          starts_with("CWD_Lag_") & ends_with(as.character(1:24))),
        na.rm = TRUE
      )
  )

# Estimate.

mod_1 = 
  dat_use_really %>% 
  feols(
    MBF_Acre_Both ~ 
      # Landowner-varying
      Owner_Acres + 
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
      Price_Stumpage_DouglasFir_Mean +
      Rate_Mean +
      # Time- and space-varying
      #  MTBS
      # Fire_0_Lag_1 +
      # Fire_15_Doughnut_Lag_1 +
      Fire_30_Doughnut_Lag_1 +
      # Fire_Proportion_Lag_1 +
      #  PRISM
      # CWD
      CWD_Mean +
      # Pyrome
      Pyrome
  )

mod_2 = 
  dat_use_really %>% 
  feols(
    MBF_Acre_Both ~ 
      # Landowner-varying
      Owner_Acres + 
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
      Price_Stumpage_DouglasFir_Mean +
      Rate_Mean +
      # Time- and space-varying
      #  MTBS
      # Fire_0_Lag_1 +
      # Fire_15_Doughnut_Lag_1 +
      Fire_30_Doughnut_Lag_1 +
      # Fire_Proportion_Lag_1 +
      #  PRISM
      # CWD
      CWD_Mean +
      # Pyrome
      Pyrome |
      County
    )

mod_3 = 
  dat_use_really %>% 
  feols(
    MBF_Acre_Both ~ 
      # Landowner-varying
      Owner_Acres + 
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
      Price_Stumpage_DouglasFir_Mean +
      Rate_Mean +
      # Time- and space-varying
      #  MTBS
      # Fire_0_Lag_1 +
      # Fire_15_Doughnut_Lag_1 +
      Fire_30_Doughnut_Lag_1 +
      # Fire_Proportion_Lag_1 +
      #  PRISM
      # CWD
      CWD_Mean +
      # Pyrome
      Pyrome |
      County + Landowner
  )

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
      feols(
        MBF_Acre_Both ~ 
          # Landowner-varying
          Owner_Acres + 
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
          Price_Stumpage_DouglasFir_Mean +
          Rate_Mean +
          # Time- and space-varying
          #  MTBS
          # Fire_0_Lag_1 +
          # Fire_15_Doughnut_Lag_1 +
          Fire_30_Doughnut_Lag_1 +
          # Fire_Proportion_Lag_1 +
          #  PRISM
          # CWD
          CWD_Mean
      )
    
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
