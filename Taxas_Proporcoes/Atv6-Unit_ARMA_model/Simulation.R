source("UCHARMA_gen.R")
source("UCHARMA_fit.R")

# Simulation -------------------------------------------------------------------

# Case 1: without regressors

set.seed(15) # setting a seed
n <- 250 # sample size
R <- 100 # monte carlo iterations
# model specification 
ma <- 1 
ar <- 1
# fixing true par vales
alpha_true <- 0.7
phi_true <- 0.2
theta_true <- 0.3
sigma_true <- 0.7

# simulation
alpha_result <- phi_result <- theta_result <- sigma_result <- c()
for (i in 1:R) {
  y <- simu.ucharma(n, alpha = alpha_true, phi = phi_true, theta = theta_true, sigma = sigma_true)
  fit1 <- try(ucharma.fit(y, ar = ar, ma = ma), silent = TRUE)
  alpha_result[i] <- fit1$alpha
  phi_result[i] <- fit1$phi
  theta_result[i] <- fit1$theta
  sigma_result[i] <- fit1$sigma
}

# compiling results
true_values <- c(alpha_true, phi_true, theta_true, sigma_true)
mean_values <- c(
  mean(alpha_result),
  mean(phi_result),
  mean(theta_result),
  mean(sigma_result)
)
b_values <- (true_values - mean_values) / true_values * 100
eqm_values <- c(
  var(alpha_result),
  var(phi_result),
  var(theta_result),
  var(sigma_result)
) + (true_values - mean_values)^2
result1 <- cbind(
  true_values,
  mean_values,
  b_values,
  eqm_values
)

rownames(result1) <- c("alpha","phi", "theta", "sigma")
colnames(result1) <- c("true value", "mean", "relative bias", "mse")
print(round(result1, 2))
