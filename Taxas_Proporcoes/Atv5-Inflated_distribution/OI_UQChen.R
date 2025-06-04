# initial values to test functions
# mu <- 0.58
# sigma <- 1
# nu <- 0.1
# tau <- 0.5
# x <- 0.7

# Unit Quantile Chen distribution functions ------------------------------------

# Function calls: dUQChen, pUQChen, qUQChen, rUQChen
# mu in (0,1) and sigma > 0
source("UQChen Functions.R")

# One-inflated Unit Quantile Chen distribution functions ----------------------

# density function
dOIUQC <- function(x, mu, sigma, nu, log = FALSE) {
  # Checking par values
  # mu value
  if (any(mu <= 0) | any(mu >= 1)) {
    stop(paste("mu must be beetwen 0 and 1 ", "\n", ""))
  }
  # sigma value
  if (any(sigma < 0)) {
    stop(paste("sigma must be positive", "\n", ""))
  }
  # nu value
  if (any(nu <= 0) | any(nu >= 1)) {
    stop(paste("nu must be beetwen 0 and 1 ", "\n", ""))
  }
  tau <- 0.5 # quantile to be modeled

  # log-like expression of one inflated unit quantile chen distribution
  log.uqchen <- log(log(tau) * sigma / ((1 - exp((-log(mu))^sigma)) * x) * (-log(x))^(sigma - 1) *
    exp((-log(x))^sigma) * exp(log(tau) / (1 - exp((-log(mu))^sigma)) * (1 - exp((-log(x))^sigma))))

  # calculating log-like value
  log.lik <- ifelse(x == 1, log(nu), log(1 - nu) + log.uqchen)

  # condtion to return log-like or probability distribution function value
  ifelse((log == FALSE), fx <- exp(log.lik), fx <- log.lik)

  # condition for x out of function domain
  fx <- ifelse(x <= 0 | x > 1, 0, fx)

  return(fx)
}

# checking dOIUQC function:
# dOIUQC(1, mu = 0.7, sigma = 2.1, nu = 0.1) # must return nu value
# Next 2 functions must return the same value
# dOIUQC(x, mu = 0.7, sigma = 2.1, nu = 0.1)
# (1 - nu) * dUQChen(x, mu = 0.7, sigma = 2.1)
# dOIUQC(0, mu = 0.7, sigma = 2.1, nu = 0.1) # must return 0, because it is not inflated in 0

# cumulative distribution function
pOIUQC <- function(q, mu, sigma, nu, lower.tail = TRUE, log.p = FALSE) {
  # Checking par values
  if (any(mu <= 0) | any(mu >= 1)) {
    stop(paste("mu must be beetwen 0 and 1 ", "\n", ""))
  }
  if (any(sigma < 0)) {
    stop(paste("sigma must be positive", "\n", ""))
  }
  if (any(nu <= 0) | any(nu >= 1)) {
    stop(paste("nu must be beetwen 0 and 1 ", "\n", ""))
  }
  if (any(q <= 0) | any(q > 1)) {
    stop(paste("q must be beetwen 0 and 1 ", "\n", ""))
  }
  tau <- 0.5 # quantile to be modeled

  # cumulative probability values
  cdf <- ifelse(
    (q > 0 & q < 1),
    (1 - nu) * exp(log(tau) / (1 - exp((-log(mu))^sigma)) * (1 - exp((-log(q))**sigma))),
    1
  )

  ifelse(lower.tail == TRUE, cdf, cdf <- 1 - cdf) # lower tail option
  ifelse(log.p == FALSE, cdf, cdf <- log(cdf)) # log-like option
  return(cdf)
}

# checking pOIUQC:
# x <- 0.7
# Next 2 functions must return the same value
# pOIUQC(x, mu = 0.7, sigma = 0.5, nu = 0.1)
# (1 - nu) * pUQChen(x, mu = 0.7, sigma = 0.5)
# pOIUQC(0.9999, mu = 0.7, sigma = 2.1, nu = 0.1) # must return 1 - nu
# pOIUQC(1, mu = 0.7, sigma = 2.1, nu = 0.1) # must return 1

# quantile function
qOIUQC <- function(p, mu, sigma, nu, lower.tail = TRUE, log.p = FALSE) {
  # Checking par values
  if (any(mu <= 0) | any(mu >= 1)) {
    stop(paste("mu must be beetwen 0 and 1 ", "\n", ""))
  }
  if (any(sigma < 0)) {
    stop(paste("sigma must be positive", "\n", ""))
  }
  if (any(nu <= 0) | any(nu >= 1)) {
    stop(paste("nu must be beetwen 0 and 1 ", "\n", ""))
  }
  if (any(p < 0) | any(p > 1)) {
    stop(paste("p must be between 0 and 1", "\n", ""))
  }
  tau <- 0.5 # quantile to be modeled

  ifelse((log.p == FALSE), p, p <- exp(p)) # log-like option
  ifelse((lower.tail == TRUE), p, p <- 1 - p) # lower tail option

  u <- (p) / (1 - nu) # transformation to then calculate quantile value

  # calculating quantile value
  q <- ifelse(
    (p > (1 - nu)),
    1,
    exp(-(log(1 - log(u) / log(tau) * (1 - exp((-log(mu))^sigma))))^(1 / sigma))
  )

  return(q)
}

# checking qOIUQC:
# x <- 0.7
# u <- pOIUQC(x, mu = 0.7, sigma = 0.5, nu = 0.2) # value to test qOIUQC function
# qOIUQC(u, mu = 0.7, sigma = 0.5, nu = 0.2) # must return the x value
# qUQChen((u) / (1 - 0.2), mu = 0.7, sigma = 0.5) # must return the x value as well

# inversion method for random generation
rOIUQC <- function(n, mu, sigma, nu) {
  # Checking par values
  if (any(mu <= 0) | any(mu >= 1)) {
    stop(paste("mu must be between 0 and 1", "\n", ""))
  }
  if (any(sigma < 0)) {
    stop(paste("sigma must be positive", "\n", ""))
  }
  if (any(nu <= 0) | any(nu >= 1)) {
    stop(paste("nu must be beetwen 0 and 1 ", "\n", ""))
  }
  if (any(n <= 0)) {
    stop(paste("n must be a positive integer", "\n", ""))
  }
  # transforming n into a integer if needed
  n <- ceiling(n)
  # generating random uniform distribution sample
  p <- runif(n)
  r <- qOIUQC(p, mu = mu, sigma = sigma, nu = nu)
  return(r)
}

# testing rOIUQC function
# (aa <- rOIUQC(200, 0.5, 0.4, 0.2))
# summary(aa)
# sort(aa)
# sum(aa==1)/200 # must be close to nu value