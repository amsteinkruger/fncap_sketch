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

# adding delta method output:

df <- df %>%
  mutate(any_prod = if_else(y > 0, 1, 0))

m1 <- feglm(
  any_prod ~ x1 + x2 + x3,
  data = df,
  family = "binomial",
  vcov = ~firm_id
)

m2 <- feols(
  y ~ x1 + x2 + x3,
  data = df %>% filter(y > 0),
  vcov = ~firm_id
)

# joint parameter vector

gamma_hat <- coef(m1)
beta_hat  <- coef(m2)

theta_hat <- c(gamma_hat, beta_hat)

# joint covariance matrix

V_gamma <- vcov(m1)
V_beta  <- vcov(m2)

V_theta <- bdiag(V_gamma, V_beta) %>%
  as.matrix()

# AME function

ame_function <- function(theta,
                         data,
                         var,
                         logit_names,
                         ols_names){
  
  # unpack parameter vectors
  gamma <- theta[logit_names]
  beta  <- theta[ols_names]
  
  X_logit <- model.matrix(
    ~ x1 + x2 + x3,
    data = data
  )
  
  X_ols <- model.matrix(
    ~ x1 + x2 + x3,
    data = data
  )
  
  # predictions
  eta <- X_logit %*% gamma
  p <- 1 / (1 + exp(-eta))
  
  mu <- X_ols %*% beta
  
  gamma_k <- gamma[var]
  beta_k  <- beta[var]
  
  me_i <-
    p * (1 - p) * gamma_k * mu +
    p * beta_k
  
  mean(me_i)
}

# numerical gradient

logit_names <- names(gamma_hat)
ols_names   <- names(beta_hat)

grad_k <- grad(
  func = ame_function,
  x = theta_hat,
  data = df,
  var = "x1",
  logit_names = logit_names,
  ols_names = ols_names
)

# delta method SE

ame_hat <- ame_function(
  theta_hat,
  data = df,
  var = "x1",
  logit_names = logit_names,
  ols_names = ols_names
)

se_hat <- sqrt(
  t(grad_k) %*%
    V_theta %*%
    grad_k
)

z_val <- ame_hat / se_hat

p_val <- 2 * (1 - pnorm(abs(z_val)))

results_x1 <- tibble(
  variable = "x1",
  AME = ame_hat,
  SE = se_hat[,1],
  z = z_val[,1],
  p_value = p_val[,1]
)

results_x1

# Across variables

vars <- c("x1", "x2", "x3")

delta_results <- map_dfr(vars, function(v){
  
  ame_hat <- ame_function(
    theta_hat,
    data = df,
    var = v,
    logit_names = logit_names,
    ols_names = ols_names
  )
  
  grad_k <- grad(
    func = ame_function,
    x = theta_hat,
    data = df,
    var = v,
    logit_names = logit_names,
    ols_names = ols_names
  )
  
  se_hat <- sqrt(
    t(grad_k) %*%
      V_theta %*%
      grad_k
  )
  
  z_val <- ame_hat / se_hat
  
  p_val <- 2 * (1 - pnorm(abs(z_val)))
  
  tibble(
    variable = v,
    AME = as.numeric(ame_hat),
    SE = as.numeric(se_hat),
    z = as.numeric(z_val),
    p_value = as.numeric(p_val)
  )
})

delta_results

# Across specifications

specs <- list(
  spec1 = c("x1", "x2"),
  spec2 = c("x1", "x2", "x3"),
  spec3 = c("x1", "x3", "x4")
)

run_two_hurdle_delta <- function(vars, data){
  
  rhs <- paste(vars, collapse = " + ")
  
  f1 <- as.formula(
    paste("any_prod ~", rhs)
  )
  
  f2 <- as.formula(
    paste("y ~", rhs)
  )
  
  # estimate models
  m1 <- feglm(
    f1,
    data = data,
    family = "binomial",
    vcov = ~firm_id
  )
  
  m2 <- feols(
    f2,
    data = data %>% filter(y > 0),
    vcov = ~firm_id
  )
  
  gamma_hat <- coef(m1)
  beta_hat  <- coef(m2)
  
  theta_hat <- c(gamma_hat, beta_hat)
  
  V_theta <- bdiag(
    vcov(m1),
    vcov(m2)
  ) %>%
    as.matrix()
  
  logit_names <- names(gamma_hat)
  ols_names   <- names(beta_hat)
  
  build_formula <- function(vv){
    as.formula(
      paste("~", paste(vv, collapse = " + "))
    )
  }
  
  X <- model.matrix(
    build_formula(vars),
    data = data
  )
  
  ame_fun <- function(theta, var){
    
    gamma <- theta[logit_names]
    beta  <- theta[ols_names]
    
    eta <- X %*% gamma
    p <- 1 / (1 + exp(-eta))
    
    mu <- X %*% beta
    
    gamma_k <- gamma[var]
    beta_k  <- beta[var]
    
    me_i <-
      p * (1 - p) * gamma_k * mu +
      p * beta_k
    
    mean(me_i)
  }
  
  map_dfr(vars, function(v){
    
    ame_hat <- ame_fun(theta_hat, v)
    
    G <- grad(
      func = ame_fun,
      x = theta_hat,
      var = v
    )
    
    se_hat <- sqrt(
      t(G) %*%
        V_theta %*%
        G
    )
    
    z_val <- ame_hat / se_hat
    
    p_val <- 2 * (1 - pnorm(abs(z_val)))
    
    tibble(
      variable = v,
      AME = as.numeric(ame_hat),
      SE = as.numeric(se_hat),
      z = as.numeric(z_val),
      p_value = as.numeric(p_val)
    )
  })
}

all_results <- imap_dfr(
  specs,
  ~ run_two_hurdle_delta(.x, df) %>%
    mutate(specification = .y)
)

all_results
