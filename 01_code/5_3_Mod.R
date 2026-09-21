# Estimate something for 2YP.

#  Clear the environment.

rm(list = ls())

#  Start timing. 

time_start = Sys.time()

#  Get data. 

dat_implicit = 
  "03_intermediate/dat_firms_implicit_3_1.csv" %>% 
  read_csv %>% 
  drop_na(Landowner) %>% 
  mutate(MBF_Both = MBF_DouglasFir + MBF_WesternHemlock)

dat_explicit = 
  "03_intermediate/dat_firms_explicit_3_1.csv" %>% 
  read_csv %>% 
  drop_na(Landowner) %>% 
  mutate(MBF_Both = MBF_DouglasFir + MBF_WesternHemlock) %>% 
  mutate(MBF_Bin = ifelse(MBF_Both > 0, 1, 0)) %>% 
  relocate(MBF_Bin, MBF_Both, .after = "Count")

# Split data by ODF's Small Forestland Owner (SFO) definition.
#  This should be in 0_7 or 1_3. 

dat_explicit_small = 
  dat_explicit %>% 
  group_by(Landowner) %>% 
  # mutate(MBF_All = sum(MBF_Both)) %>% 
  mutate(Owner_Acres_Min = min(Owner_Acres, na.rm = TRUE)) %>% 
  ungroup %>% 
  # filter(MBF_All <= quantile(MBF_All, 0.50))
  filter(Owner_Acres_Min <= 5000)

dat_explicit_large = 
  dat_explicit %>% 
  group_by(Landowner) %>% 
  # mutate(MBF_All = sum(MBF_Both)) %>% 
  mutate(Owner_Acres_Min = min(Owner_Acres, na.rm = TRUE)) %>% 
  ungroup %>% 
  # filter(MBF_All > quantile(MBF_All, 0.50))
  filter(Owner_Acres_Min > 5000)

vec_small = dat_explicit_small$Landowner %>% unique
vec_large = dat_explicit_large$Landowner %>% unique

dat_implicit_small = dat_implicit %>% filter(Landowner %in% vec_small)
dat_implicit_large = dat_implicit %>% filter(Landowner %in% vec_large)
  
# Do:

# (1) Hurdle, No Inventory
# (2) Hurdle, Inventory
# (3) Hurdle, Inventory, Small Firms Only
# (4) Hurdle, Inventory, Large Firms Only (Preferred)

# AME, SE via Delta

# (1) Linear
# (2) Tobit
# (3) Heckit
# (4) Craggit

# AME, SE via Delta

# More specifications with more/fewer covariates?

# Demo

mod_0_implicit = 
  feols(
    MBF_Both ~ 
      # owner variables
      Owner_Acres + # this does vary over time, just not much
      SiteClassMode +
      Elevation +
      Slope +
      Distance_Place +
      # time-varying
      Price_Stumpage_DouglasFir_Mean +
      Rate_Mean +
      # time- and owner-varying
      Fire_30 + 
      CWD_Mean,
    vcov = "hetero",
    data = dat_implicit)

mod_0_explicit = 
  feols(
    MBF_Both ~ 
      Owner_Acres + 
      SiteClassMode +
      Elevation +
      Slope +
      Distance_Place +
      Price_Stumpage_DouglasFir_Mean +
      Rate_Mean +
      Fire_30 + 
      CWD_Mean,
    vcov = "hetero",
    data = dat_explicit)

etable(mod_0_implicit, mod_0_explicit)

# Hurdle Models?

#  First Stage

mod_hurdle_first_all = 
  feglm(
    MBF_Bin ~
      Owner_Acres + 
      SiteClassMode +
      # Elevation +
      # Slope +
      # Distance_Place +
      Price_Stumpage_DouglasFir_Mean +
      Rate_Mean +
      Fire_30 + 
      CWD_Mean,
    vcov = "hetero",
    family = binomial(link = "probit"),
    glm.iter = 50,
    glm.tol = 1e-8,
    data = dat_explicit
  )

mod_hurdle_first_small = 
  feglm(
    MBF_Bin ~
      Owner_Acres +
      SiteClassMode +
      # Elevation +
      # Slope +
      # Distance_Place +
      Price_Stumpage_DouglasFir_Mean +
      Rate_Mean +
      Fire_30 +
      CWD_Mean,
    vcov = "hetero",
    family = binomial(link = "probit"),
    data = dat_explicit_small,
    glm.iter = 50,
    glm.tol = 1e-8
  )

mod_hurdle_first_large = 
  feglm(
    MBF_Bin ~
      Owner_Acres + 
      SiteClassMode +
      # Elevation +
      # Slope +
      # Distance_Place +
      Price_Stumpage_DouglasFir_Mean +
      Rate_Mean +
      Fire_30 + 
      CWD_Mean,
    vcov = "hetero",
    family = binomial(link = "probit"),
    data = dat_explicit_large,
    glm.iter = 50,
    glm.tol = 1e-8
  )

etable(mod_hurdle_first_all, mod_hurdle_first_small, mod_hurdle_first_large)

#  Second Stage

mod_hurdle_second_all = 
  feols(
    MBF_Both ~
      Owner_Acres + 
      SiteClassMode +
      # Elevation +
      # Slope +
      # Distance_Place +
      Price_Stumpage_DouglasFir_Mean +
      Rate_Mean +
      Fire_30 + 
      CWD_Mean,
    vcov = "hetero",
    data = dat_implicit
  )

mod_hurdle_second_small = 
  feols(
    MBF_Both ~
      Owner_Acres + 
      SiteClassMode +
      # Elevation +
      # Slope +
      # Distance_Place +
      Price_Stumpage_DouglasFir_Mean +
      Rate_Mean +
      Fire_30 + 
      CWD_Mean,
    vcov = "hetero",
    data = dat_implicit_small
  )

mod_hurdle_second_large = 
  feols(
    MBF_Both ~
      Owner_Acres + 
      SiteClassMode +
      # Elevation +
      # Slope +
      # Distance_Place +
      Price_Stumpage_DouglasFir_Mean +
      Rate_Mean +
      Fire_30 + 
      CWD_Mean,
    vcov = "hetero",
    data = dat_implicit_large
  )

etable(mod_hurdle_second_all, mod_hurdle_second_small, mod_hurdle_second_large)

#  AME

fun_ame_inner <-
  function(mod_first, mod_second, vec_p, vec_mu, vec_var) { # vec_var not a vec as a argument
    
    mean(vec_p * (1 - vec_p) * coef(mod_first)[vec_var] * vec_mu + vec_p * coef(mod_second[vec_var]))
    
  }

fun_ame_outer <-
  function(formula_first, formula_second, data_first, data_second) {
    
    mod_first <- feglm(formula_first, data = data_first, family = binomial("logit"), glm.iter = 50, vcov = "hetero")
    mod_second <- feols(formula_second, data = data_second, vcov = "hetero")
    
    vec_p <- predict(mod_first, newdata = data_first, type = "response")
    vec_mu <- predict(mod_second, newdata = data_second)
    
    vec_var <- mod_first %>% coef %>% names
    
    vec_ame = map(vec_var, ~ fun_ame_inner(mod_first, mod_second, vec_p, vec_mu, .x))
    
    return(vec_ame)
    
  }

fun_marginal <- 
  function(var, first, second, data){
    
    # Estimate probabilities of production for each observation. (p)
    
    vec_predict_first <- predict(first, newdata = data, type = "response") 
    
    # Estimate conditional production for each observation. (mu)
    
    vec_predict_second <- predict(second, newdata = data, type = "response")

    # Assign coefficient estimates.
    val_gamma <- coef(first)[var]
    val_beta  <- coef(second)[var]
    
    # Calculate AMEs.
    val_ame <- vec_predict_first * (1 - vec_predict_first) * val_gamma * vec_predict_second + vec_predict_first * val_beta
    
    # Return.
    tibble(Variable = var, AME = mean(val_ame, na.rm = TRUE))
    
  }

mod_marginal = 
  tibble(
    Specification = c("All", "Small", "Large"),
    Model_First = 
      list(
        mod_hurdle_first_all,
        mod_hurdle_first_small,
        mod_hurdle_first_large
      ),
    Model_Second = 
      list(
        mod_hurdle_second_all,
        mod_hurdle_second_small,
        mod_hurdle_second_large
      ),
    Covariates = 
      mod_hurdle_first_all$coefficients %>% 
      names %>% 
      list
  ) %>% 
  unnest(Covariates) %>% 
  mutate(AME = 
           pmap(
             .l = 
               list(
                 first = Model_First, 
                 second = Model_Second, 
                 var = Covariates
               ),
             .f = fun_marginal,
             data = dat_explicit
           )
  ) %>% 
  select(Specification, AME) %>% 
  unnest(AME) %>% 
  pivot_wider(
    values_from = AME, 
    names_from = Specification,
    names_prefix = "Model_"
  )

#  SE



# Exports

# library(modelsummary)
# library(flextable)
# 
# modelsummary(
#   list("A" = mod_hurdle_first_1,
#        "B" = mod_hurdle_first_2,
#        "C" = mod_hurdle_first_3,
#        "D" = mod_hurdle_first_4),
#        stars = TRUE, 
#        output = "flextable") |> 
#   autofit() |> 
#   save_as_docx(path = "04_out/tab_first.docx")
# 
# modelsummary(
#   list("A" = mod_hurdle_second_1,
#        "B" = mod_hurdle_second_2,
#        "C" = mod_hurdle_second_3,
#        "D" = mod_hurdle_second_4),
#   stars = TRUE, 
#   output = "flextable") |> 
#   autofit() |> 
#   save_as_docx(path = "04_out/tab_second.docx")
# 
# mod_marginal %>% 
#   mutate(across(starts_with("Model"), ~ round(.x, 5))) %>% 
#   flextable %>% 
#   autofit %>% 
#   save_as_docx(path = "04_out/tab_third.docx")

# Checking out mhurdle: appears to throw computational errors pretty often.

# library(mhurdle)
# 
# mod_hurdle_first_1_mhurdle = 
#   mhurdle(
#     MBF_Both ~ 
#       Stumpage_Mean_20 +
#       Rate_Mean_20 +
#       VPD_Mean_20 + 
#       Fire_30_Doughnut_Lag_1 | 
#       SiteClassMode +
#       Elevation +
#       Distance_Mill +
#       Stumpage_Mean_20 +
#       Rate_Mean_20 +
#       VPD_Mean_20 + 
#       Fire_30_Doughnut_Lag_1,
#     data = dat_explicit)

# Figure out tex installation on this machine. 

# Exports?

# etable(mod_hurdle_first_1, 
#        mod_hurdle_second_1,
#        tex = TRUE,
#        style.tex = style.tex("aer"),
#        file = "04_out/tab_test.tex")

# write_csv(mod_hurdle_first_1 %>% tidy, "04_out/tab_mod_first.csv")
# write_csv(mod_hurdle_second_1 %>% tidy, "04_out/tab_mod_second.csv")

# write_csv(dat_ame, "04_out/tab_ame.csv")
