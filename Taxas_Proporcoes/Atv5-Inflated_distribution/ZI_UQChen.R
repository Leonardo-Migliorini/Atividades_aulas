# Loading packages -------------------------------------------------------------
library(gamlss)

# initial values to test functions
# mu <- 0.58
# sigma <- 1
# nu <- 0.1
# tau <- 0.5
# x <- 0.22

# Unit Quantile Chen distribution functions ------------------------------------

# Function calls: dUQChen, pUQChen, qUQChen, rUQChen
# mu in (0,1) and sigma > 0
source("UQChen Functions.R")

# Zero-inflated Unit Quantile Chen distribution functions ----------------------

# density function
dZIUQC <- function(x, mu, sigma, nu, log = FALSE) {
  if (any(mu <= 0) | any(mu >= 1)) {
    stop(paste("mu must be beetwen 0 and 1 ", "\n", ""))
  }
  if (any(sigma < 0)) {
    stop(paste("sigma must be positive", "\n", ""))
  }
  if (any(nu <= 0) | any(nu >= 1)) {
    stop(paste("nu must be beetwen 0 and 1 ", "\n", ""))
  }
  tau <- 0.5

  log.uqchen <- log(log(tau) * sigma / ((1 - exp((-log(mu))^sigma)) * x) * (-log(x))^(sigma - 1) *
    exp((-log(x))^sigma) * exp(log(tau) / (1 - exp((-log(mu))^sigma)) * (1 - exp((-log(x))^sigma))))

  log.lik <- ifelse(x == 0, log(nu), log(1 - nu) + log.uqchen)

  if (log == FALSE) {
    fx <- exp(log.lik)
  } else {
    fx <- log.lik
  }
  fx <- ifelse(x < 0 | x >= 1, 0, fx)
  fx
}

# checking dZIUQC function
# dZIUQC(0, mu = 0.7, sigma = 2.1, nu = 0.1) # = nu
# dZIUQC(x, mu = 0.7, sigma = 2.1, nu = 0.1)
# (1 - nu) * dUQChen(x, mu = 0.7, sigma = 2.1)
# dZIUQC(1, mu = 0.7, sigma = 2.1, nu = 0.1) # 0 because it is not inflated in one

# cumulative distribution function

pZIUQC <- function(q, mu, sigma, nu, lower.tail = TRUE, log.p = FALSE) {
  if (any(mu <= 0) | any(mu >= 1)) {
    stop(paste("mu must be beetwen 0 and 1 ", "\n", ""))
  }
  if (any(sigma < 0)) {
    stop(paste("sigma must be positive", "\n", ""))
  }
  if (any(nu <= 0) | any(nu >= 1)) {
    stop(paste("nu must be beetwen 0 and 1 ", "\n", ""))
  }

  tau <- 0.5

  cdf <- ifelse((q > 0 & q < 1),
    nu + (1 - nu) * exp(log(tau) / (1 - exp((-log(mu))^sigma)) * (1 - exp((-log(q))**sigma))),
    0
  )
  cdf <- ifelse((q == 0), nu, cdf)
  cdf <- ifelse((q >= 1), 1, cdf)
  if (lower.tail == TRUE) {
    cdf <- cdf
  } else {
    cdf <- 1 - cdf
  }
  if (log.p == FALSE) {
    cdf <- cdf
  } else {
    cdf <- log(cdf)
  }
  cdf <- ifelse(q < 0, 0, cdf)
  cdf <- ifelse(q >= 1, 1, cdf)
  cdf
}

# checking pZIUQC
# pZIUQC(0, mu = 0.7, sigma = 2.1, nu = 0.1)
# pZIUQC(x, mu = 0.7, sigma = 0.5, nu = 0.1)
# nu + (1 - nu) * pUQChen(x, mu = 0.7, sigma = 0.5)

# quantile function
qZIUQC <- function(p, mu, sigma, nu, lower.tail = TRUE, log.p = FALSE) {
  if (any(mu <= 0) | any(mu >= 1)) {
    stop(paste("mu must be beetwen 0 and 1 ", "\n", ""))
  }
  if (any(sigma < 0)) {
    stop(paste("sigma must be positive", "\n", ""))
  }
  if (any(nu <= 0) | any(nu >= 1)) {
    stop(paste("nu must be beetwen 0 and 1 ", "\n", ""))
  }
  if (log.p == TRUE) {
    p <- exp(p)
  } else {
    p <- p
  }
  if (lower.tail == TRUE) {
    p <- p
  } else {
    p <- 1 - p
  }
  u <- (p - nu) / (1 - nu)
  if (any(p < 0) | any(p > 1)) {
    stop(paste("p must be between 0 and 1", "\n", ""))
  }
  
  tau <- 0.5
  
  suppressWarnings(
    q <- ifelse((nu >= p), 0,
      exp(-(log(1 - log(u) / log(tau) * (1 - exp((-log(mu))^sigma))))^(1 / sigma))
    )
  )
  q
}

# checking qZIUQC
# u <- pZIUQC(x, mu = 0.4, sigma = 0.5, nu = 0.1)
# qZIUQC(u, mu = 0.4, sigma = 0.5, nu = 0.1)
# qUQChen((u - nu) / (1 - nu), mu = 0.4, sigma = 0.5)

# inversion method for random generation
rZIUQC <- function(n, mu, sigma, nu) {
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
  n <- ceiling(n)
  p <- runif(n)
  r <- qZIUQC(p, mu = mu, sigma = sigma, nu = nu)
  r
}

# (aa <- rZIUQC(200, 0.4,1,0.2))
# summary(aa)
