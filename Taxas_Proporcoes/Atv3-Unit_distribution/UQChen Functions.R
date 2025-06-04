# Cumulative distribution function --------------------------------------------#

pUQChen <- function(q, mu, sigma, tau = 0.5) {
  f <- exp(log(tau) / (1 - exp((-log(mu))^sigma)) * (1 - exp((-log(q))**sigma)))
  
  return(f)
}

# Probability density function ------------------------------------------------#

dUQChen <- function(y, mu, sigma, tau = 0.5, log = FALSE) {
  f <- log(tau) * sigma / ((1 - exp((-log(mu))^sigma)) * y) * (-log(y))^(sigma - 1) *
    exp((-log(y))^sigma) * exp(log(tau) / (1 - exp((-log(mu))^sigma)) * (1 - exp((-log(y))^sigma)))
  if (log == TRUE) {
    return(log(f))
  }
  return(f)
}


# Quantile Function -----------------------------------------------------------#

qUQChen <- function(u, mu, sigma, tau = 0.5) {
  q <- exp(-(log(1 - log(u) / log(tau) * (1 - exp((-log(mu))^sigma))))^(1 / sigma))
  
  return(q)
}

# Random number generator function --------------------------------------------#

rUQChen <- function(n, mu, sigma, tau = 0.5) {
  u <- runif(n)
  y <- exp(-(log(1 - log(u) / log(tau) * (1 - exp((-log(mu))^sigma))))^(1 / sigma))
  
  return(y)
}
