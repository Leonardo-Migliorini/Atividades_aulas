library(gamlss)
source("UQchen.R")

# Case 1: without regressors
set.seed(112)
n <- 500
mu_true <- 0.3
sigma_true <- 1.2
mu_result <- c()
sigma_result <- c()

for (i in 1:100) {
  y <- rUQChen(n, mu_true, sigma_true)
  fit1 <- gamlss(y ~ 1, family = UQC(), trace = F, method = CG())
  logit_link <- make.link("logit")
  log_link <- make.link("log")
  mu_result[i] <- logit_link$linkinv(fit1$mu.coefficients)
  # mu_result[i]<-fit1$mu.coefficients
  sigma_result[i] <- log_link$linkinv(fit1$sigma.coefficients)
}

result1 <- matrix(c(
  mu_true, mean(mu_result),
  sigma_true, mean(sigma_result)
), 2, 2)
colnames(result1) <- c("mu", "sigma")
rownames(result1) <- c("true value", "mean")
print(round(result1, 2))


# Case 2: with regressors
{
  set.seed(10)
  n <- 200
  X <- runif(n)
  logit_link <- make.link("logit")
  log_link <- make.link("log")
  b1 <- 1.6
  b2 <- -0.7
  mu_true <- logit_link$linkinv(b1 + b2 * X)
  g1 <- 0.5
  g2 <- -1
  sigma_true <- log_link$linkinv(g1 + g2 * X)
  R <- 100
  mu_result <- matrix(NA, R, 2)
  sigma_result <- matrix(NA, R, 2)
}

for (i in 1:R) {
  y <- rUQChen(n, mu_true, sigma_true)
  fit1 <- gamlss(y ~ X, sigma.formula = ~X, 
                 family = UQC(mu.link = "logit", sigma.link = "log"), trace = F, method = RS())
  mu_result[i, ] <- fit1$mu.coefficients
  sigma_result[i, ] <- fit1$sigma.coefficients
}

{
  true_values <- c(b1, b2, g1, g2)
  mean_values <- c(
    apply(mu_result, 2, mean),
    apply(sigma_result, 2, mean)
  )
  b_values <- (true_values - mean_values) / true_values * 100
  eqm_values <- c(
    apply(mu_result, 2, var),
    apply(sigma_result, 2, var)
  ) + (true_values - mean_values)^2
  result1 <- cbind(
    true_values,
    mean_values,
    b_values,
    eqm_values
  )
  colnames(result1) <- c("true value", "mean", "bias", "eqm")
  rownames(result1) <- c("b1", "b2", "g1", "g2")
  print(round(result1, 2))
}
