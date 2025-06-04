# Cumulative distribution function --------------------------------------------#

pquchen <- function(x, mu, sigma, tau = 0.5) {
  f <- exp(log(tau) / (1 - exp((-log(mu))^sigma)) * (1 - exp((-log(x))**sigma)))

  return(f)
}

# x <- seq(0.01, 0.99, by = 0.001)
# y <- pquchen(x,0.5,1,0.5)
# plot(x,y)

# Probability density function ------------------------------------------------#

dquchen <- function(x, mu, sigma, tau = 0.5, log = FALSE) {
  f <- log(tau) * sigma / ((1 - exp((-log(mu))^sigma)) * x) * (-log(x))^(sigma - 1) *
    exp((-log(x))^sigma) * exp(log(tau) / (1 - exp((-log(mu))^sigma)) * (1 - exp((-log(x))^sigma)))
  if (log == TRUE) {
    return(log(f))
  }
  return(f)
}

# x <- seq(0.01, 0.99, by = 0.001)
# y <- dquchen(x,0.5,1,0.5)
# y <- dquchen(x,0.5,1,0.5, log = TRUE)
# plot(x,y)
# integrate(dquchen,0,1,0.5,1,0.5)

# Quantile Function -----------------------------------------------------------#

qquchen <- function(p, mu, sigma, tau = 0.5) {
  q <- exp(-(log(1 - log(p) / log(tau) * (1 - exp((-log(mu))^sigma))))^(1 / sigma))

  return(q)
}

# x <- 0.9
# (y <- pquchen(x,0.5,1,0.5))
# (p <- qquchen(y,0.5,1,0.5))

# Random number generator function --------------------------------------------#

rquchen <- function(n, mu, sigma, tau = 0.5) {
  u <- runif(n)
  y <- exp(-(log(1 - log(u) / log(tau) * (1 - exp((-log(mu))^sigma))))^(1 / sigma))

  return(y)
}

# set.seed(12)
# aa <- rquchen(200, 0.5, 1, 0.5)

# Log-likelihood function -----------------------------------------------------#

loglike_quchen <- function(teta, y, tau = 0.5, m.optim = 1) {
  mu <- teta[1]
  sigma <- teta[2]

  lv <- sum(
    log(log(tau) * sigma / (1 - exp((-log(mu))^sigma))) - log(y) + (sigma - 1) * log(-log(y)) +
      (-log(y))^sigma + log(tau) / (1 - exp((-log(mu))^sigma)) * (1 - exp((-log(y))^sigma))
  )

  if (m.optim == -1) {
    return(-lv)
  }
  if (m.optim == 1) {
    return(lv)
  }
}

# set.seed(12)
# aa <- rquchen(200, 0.5, 1, 0.5)
# sum(dquchen(aa, 0.5, 1, 0.5, log = TRUE))
# loglike_quchen(c(0.5, 1), aa, 0.5)
