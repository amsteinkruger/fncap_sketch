# Estimate something for 2YP.

#  Clear the environment.

rm(list = ls())

#  Start timing. 

time_start = Sys.time()

#  Get data. 

dat_implicit = 
  "03_intermediate/dat_firms_implicit_3_1.csv" %>% 
  read_csv %>% 
  drop_na(Landowner, Owner_Acres) %>% 
  mutate(MBF_Both = MBF_DouglasFir + MBF_WesternHemlock) %>% 
  relocate(MBF_Both, .after = "Landowner_ID") %>% 
  select(Landowner_ID, QuarterCompletion, MBF_Both, Owner_Acres, SiteClassMode, Price_Stumpage_DouglasFir_Mean, Rate_Mean, Fire_30, CWD_Mean)

dat_explicit = 
  "03_intermediate/dat_firms_explicit_3_1.csv" %>% 
  read_csv %>% 
  drop_na(Landowner, Owner_Acres) %>% 
  mutate(MBF_Both = MBF_DouglasFir + MBF_WesternHemlock) %>% 
  mutate(MBF_Bin = ifelse(MBF_Both > 0, 1, 0)) %>% 
  relocate(MBF_Both, MBF_Both, .after = "Landowner_ID") %>% 
  select(Landowner_ID, QuarterCompletion, MBF_Bin, MBF_Both, Owner_Acres, SiteClassMode, Price_Stumpage_DouglasFir_Mean, Rate_Mean, Fire_30, CWD_Mean)

# Split data by ODF's Small Forestland Owner (SFO) definition.
#  This should be in 0_7 or 1_3. 

dat_explicit_small = 
  dat_explicit %>% 
  group_by(Landowner_ID) %>% 
  mutate(Owner_Acres_Max = max(Owner_Acres, na.rm = TRUE)) %>% 
  ungroup %>% 
  filter(Owner_Acres_Max < 5000)

dat_explicit_large = 
  dat_explicit %>% 
  group_by(Landowner_ID) %>% 
  # mutate(MBF_All = sum(MBF_Both)) %>% 
  mutate(Owner_Acres_Max = max(Owner_Acres, na.rm = TRUE)) %>% 
  ungroup %>% 
  # filter(MBF_All > quantile(MBF_All, 0.50))
  filter(Owner_Acres_Max >= 5000)

vec_small = dat_explicit_small$Landowner_ID %>% unique
vec_large = dat_explicit_large$Landowner_ID %>% unique

dat_implicit_small = dat_implicit %>% filter(Landowner_ID %in% vec_small)
dat_implicit_large = dat_implicit %>% filter(Landowner_ID %in% vec_large)
  
# Do:

# (1-3) Craggit First-Stage, All/Small/Large
# (1-3) Craggit Second-Stage, All/Small/Large
# (1-3) Craggit AME, All/Small/Large

# (1-3) Craggit First-/Second-/AME, All
# (1-3) Craggit First-/Second-/AME, Small
# (1-3) Craggit First-/Second-/AME, Large

# (1) Linear
# (2) Tobit
# (3) Heckit
# (4) Craggit

# Dataframes out for ggplot and latex

# Formulae

formula_first = 
  MBF_Bin ~ 
  Owner_Acres + 
  SiteClassMode + 
  Price_Stumpage_DouglasFir_Mean + 
  Rate_Mean +
  Fire_30 + 
  CWD_Mean

formula_second = 
  MBF_Both ~ 
  Owner_Acres + 
  SiteClassMode + 
  Price_Stumpage_DouglasFir_Mean + 
  Rate_Mean +
  Fire_30 + 
  CWD_Mean

# Hurdle Models

#  First Stage

mod_hurdle_first_all = 
  feglm(
    formula_first,
    vcov = "hetero",
    family = binomial(link = "probit"),
    glm.iter = 50,
    glm.tol = 1e-8,
    data = dat_explicit
  )

mod_hurdle_first_small = 
  feglm(
    formula_first,
    vcov = "hetero",
    family = binomial(link = "probit"),
    data = dat_explicit_small,
    glm.iter = 50,
    glm.tol = 1e-8
  )

mod_hurdle_first_large = 
  feglm(
    formula_first,
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
    formula_second,
    vcov = "hetero",
    data = dat_implicit
  )

mod_hurdle_second_small = 
  feols(
    formula_second,
    vcov = "hetero",
    data = dat_implicit_small
  )

mod_hurdle_second_large = 
  feols(
    formula_second,
    vcov = "hetero",
    data = dat_implicit_large
  )

etable(mod_hurdle_second_all, mod_hurdle_second_small, mod_hurdle_second_large)

#  AME Functions

fun_ame_inner <-
  function(mod_first, mod_second, vec_eta, vec_p, vec_mu, var) {
    
    mean(dnorm(vec_eta) * coef(mod_first)[[var]] * vec_mu + vec_p * coef(mod_second)[[var]], na.rm = TRUE)
    
  }

fun_ame_outer <-
  function(formula_first, formula_second, data_first, data_second) {
    
    mod_first <- feglm(formula_first, data = data_first, family = binomial("probit"), glm.iter = 25, glm.tol = 1e-08) # Defaults for glm.*.
    mod_second <- feols(formula_second, data = data_second)
    
    vec_eta <- predict(mod_first, newdata = data_first, type = "link")
    vec_p   <- predict(mod_first, newdata = data_first, type = "response")
    vec_mu  <- predict(mod_second, newdata = data_first, type = "response")
    
    vec_var <- mod_first %>% coef %>% names %>% setdiff("(Intercept)")
    
    vec_ame = 
      map_dbl(
        vec_var, 
        ~ fun_ame_inner(mod_first, mod_second, vec_eta, vec_p, vec_mu, .x)
      )
    
    return(vec_ame)
    
  }

#   Test

fun_ame_outer(
  formula_first,
  formula_second,
  dat_explicit_small,
  dat_implicit_small
)

#  Computation

#   AME

dat_ame = 
  tibble(
    Subset = c("All", "Small", "Large"),
    Data_Explicit = list(dat_explicit, dat_explicit_small, dat_explicit_large),
    Data_Implicit = list(dat_implicit, dat_implicit_small, dat_implicit_large)
  ) %>% 
  mutate(
    AME = 
      map2(
        Data_Explicit,
        Data_Implicit,
        ~ fun_ame_outer(
          formula_first,
          formula_second,
          .x,
          .y
        )
      ) %>% 
      map(
        ~ tibble(
          Var = 
            c(
              "Owner_Acres", 
              "SiteClassMode",
              "Price_Stumpage_DouglasFir_Mean",
              "Rate_Mean",
              "Fire_30",
              "CWD_Mean"
            ),
          AME = .x
        )
      )
  ) %>% 
  select(Subset, AME) %>% 
  unnest(AME)

#   SE

# It seems like there's some nonlinear increase in runtime with draws related to throwing datasets around.
# No idea where to start fixing that.
# Could also be that the problem is in the many, many string comparisons in each filter().
# ~1h for 1000 draws with three subsets and 32 cores running. 

library(furrr)

plan(multisession, workers = 4)

set.seed(0112358) 

dat_se = 
  tibble(Draw = 1:12) %>% 
  mutate(
    Landowners_All = map(Draw, ~ dat_implicit$Landowner_ID %>% unique %>% sample(replace = TRUE)),
    Landowners_Small = map(Draw, ~ dat_implicit_small$Landowner_ID %>% unique %>% sample(replace = TRUE)),
    Landowners_Large = map(Draw, ~ dat_implicit_large$Landowner_ID %>% unique %>% sample(replace = TRUE))
  ) %>% 
  pivot_longer(
    starts_with("Landowners"), 
    names_prefix = "Landowners_", 
    names_to = "Subset", 
    values_to = "Landowners"
  ) %>% 
  mutate(
    data_first = Landowners %>% map(~ filter(dat_explicit, Landowner_ID %in% .x)),
    data_second = Landowners %>% map(~ filter(dat_implicit, Landowner_ID %in% .x)), 
    AME = 
      future_map2( # Note futures. 
        data_first,
        data_second,
        ~ fun_ame_outer(
          formula_first,
          formula_second,
          .x,
          .y
        ),
        .options = furrr_options(seed = TRUE),
        .progress = TRUE
      ) %>% 
      map(
        ~ tibble(
          Var = 
            c(
              "Owner_Acres", 
              "SiteClassMode",
              "Price_Stumpage_DouglasFir_Mean",
              "Rate_Mean",
              "Fire_30",
              "CWD_Mean"
            ),
          AME = .x
        )
      )
  ) %>% 
  select(-Landowners) %>% 
  unnest(AME) %>% 
  group_by(Var, Subset) %>% 
  summarize(
    AME_Bootstrap = mean(AME),
    SE = sd(AME),
    CI_01 = quantile(AME, 0.01),
    CI_05 = quantile(AME, 0.05),
    CI_10 = quantile(AME, 0.10),
    CI_90 = quantile(AME, 0.90),
    CI_95 = quantile(AME, 0.95),
    CI_99 = quantile(AME, 0.99)) %>% 
  ungroup

#   p

# join and compute

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
