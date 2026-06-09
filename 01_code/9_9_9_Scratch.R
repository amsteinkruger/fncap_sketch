# Sketching a hurdle model with a robot's input.

# Remember to worry about FE and SE clustering.

library(tidyverse)
library(fixest)
library(boot)

# outcome
# y = amount produced

# covariates
# x1 x2 x3

df <- mydata

# participation indicator
df <- df %>%
  mutate(any_prod = if_else(y > 0, 1, 0))

# first hurdle: logit
m1 <- feglm(
  any_prod ~ x1 + x2 + x3,
  data = df,
  family = "binomial"
)

# second hurdle: OLS on positive observations
m2 <- feols(
  y ~ x1 + x2 + x3,
  data = df %>% filter(y > 0)
)

# AME function 

ame_two_hurdle <- function(var, logit_model, ols_model, data){
  
  # predicted probability
  p <- predict(logit_model, type = "response")
  
  # predicted conditional mean
  mu <- predict(ols_model, 
                newdata = data,
                type = "response")
  
  # coefficients
  gamma_k <- coef(logit_model)[var]
  beta_k  <- coef(ols_model)[var]
  
  # individual marginal effects
  me_i <- p * (1 - p) * gamma_k * mu +
    p * beta_k
  
  tibble(
    variable = var,
    AME = mean(me_i, na.rm = TRUE)
  )
}

# AME calculation

vars <- c("x1", "x2", "x3")

ame_results <- map_dfr(
  vars,
  ame_two_hurdle,
  logit_model = m1,
  ols_model = m2,
  data = df
)

ame_results

# SE bootstrap function

boot_ame <- function(data, indices, var){
  
  d <- data[indices, ]
  
  d <- d %>%
    mutate(any_prod = if_else(y > 0, 1, 0))
  
  # re-estimate models
  m1 <- feglm(
    any_prod ~ x1 + x2 + x3,
    data = d,
    family = "binomial"
  )
  
  m2 <- feols(
    y ~ x1 + x2 + x3,
    data = d %>% filter(y > 0)
  )
  
  # predictions
  p <- predict(m1, type = "response")
  
  mu <- predict(m2,
                newdata = d,
                type = "response")
  
  gamma_k <- coef(m1)[var]
  beta_k  <- coef(m2)[var]
  
  me_i <- p * (1 - p) * gamma_k * mu +
    p * beta_k
  
  mean(me_i, na.rm = TRUE)
}

# SE bootstrap calculation

boot_results <- map(
  vars,
  ~ boot(
    data = df,
    statistic = function(data, indices){
      boot_ame(data, indices, .x)
    },
    R = 999
  )
)

# output

final_results <- map2_dfr(
  vars,
  boot_results,
  function(v, b){
    
    ame_hat <- mean(b$t)
    
    se_hat <- sd(b$t)
    
    z_val <- ame_hat / se_hat
    
    p_val <- 2 * (1 - pnorm(abs(z_val)))
    
    tibble(
      variable = v,
      AME = ame_hat,
      SE = se_hat,
      z = z_val,
      p_value = p_val
    )
  }
)

final_results

# ---
# multispecification alternative
# ---

specs <- list(
  spec1 = c("x1", "x2"),
  spec2 = c("x1", "x2", "x3"),
  spec3 = c("x1", "x3", "x4")
)

run_two_hurdle <- function(vars, data, R = 499){
  
  rhs <- paste(vars, collapse = " + ")
  
  logit_formula <- as.formula(
    paste("any_prod ~", rhs)
  )
  
  ols_formula <- as.formula(
    paste("y ~", rhs)
  )
  
  data <- data %>%
    mutate(any_prod = if_else(y > 0, 1, 0))
  
  # estimate models
  m1 <- feglm(
    logit_formula,
    data = data,
    family = "binomial"
  )
  
  m2 <- feols(
    ols_formula,
    data = data %>% filter(y > 0)
  )
  
  # bootstrap each variable
  results <- map_dfr(vars, function(v){
    
    b <- boot(
      data = data,
      statistic = function(data, indices){
        
        d <- data[indices, ]
        
        mm1 <- feglm(
          logit_formula,
          data = d,
          family = "binomial"
        )
        
        mm2 <- feols(
          ols_formula,
          data = d %>% filter(y > 0)
        )
        
        p <- predict(mm1, type = "response")
        
        mu <- predict(mm2,
                      newdata = d,
                      type = "response")
        
        gamma_k <- coef(mm1)[v]
        beta_k  <- coef(mm2)[v]
        
        me_i <- p * (1 - p) * gamma_k * mu +
          p * beta_k
        
        mean(me_i, na.rm = TRUE)
      },
      R = R
    )
    
    ame_hat <- mean(b$t)
    se_hat  <- sd(b$t)
    
    z_val <- ame_hat / se_hat
    
    p_val <- 2 * (1 - pnorm(abs(z_val)))
    
    tibble(
      variable = v,
      AME = ame_hat,
      SE = se_hat,
      z = z_val,
      p_value = p_val
    )
  })
  
  results
}

all_results <- imap_dfr(
  specs,
  ~ run_two_hurdle(.x, df, R = 499) %>%
    mutate(specification = .y)
)

all_results
