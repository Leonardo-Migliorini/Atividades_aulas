# Cumulative distribution function --------------------------------------------#

puchen <- function(x, lambda, beta){
  
  if(lambda <= 0 || beta <= 0) stop("the parameter values must be greater than 0.")
  if(any(x <= 0) || any(x >= 1)) stop("x values must be between (0,1).")
  
  return(exp(lambda*(1 - exp((-log(x))**beta))))
}

# x <- seq(0.01, 0.99, by = 0.001)
# y <- puchen(x,1,2)
# plot(x,y)

# Probability density function ------------------------------------------------#

duchen <- function(x, lambda, beta, log = FALSE){
  
  if(lambda <= 0 || beta <= 0) stop("the parameter values must be greater than 0.")
  if(any(x <= 0) || any(x >= 1)) stop("x values must be between (0,1).")
  
  f <- lambda * beta * exp(lambda * (1 - exp((-log(x))**beta)) + (- log(x))**beta) * (-log(x))**(beta - 1) / x
  
  if(log == TRUE){return(log(f))}
  return(
    f
  )
}

# x <- seq(0.01, 0.99, by = 0.001)
# y <- duchen(x,0.5,1)
# y <- duchen(x,0.5,1, log = TRUE)
# plot(x,y)
# integrate(duchen,0,1,0.5,1)

# Quantile Function -----------------------------------------------------------#

quchen <- function(p, lambda, beta){
  
  if(lambda <= 0 || beta <= 0) stop("the parameter values must be greater than 0.")
  if(any(p < 0) || any(p > 1)) stop("p values must be between (0,1).")
  
  return(
    exp(- (log(1 - log(p)/lambda)**(1/beta)))
  )
}

# x <- 0.5
# y <- puchen(x,1,2)
# p <- quchen(y,1,2)

# Random number generator function --------------------------------------------#

ruchen <- function(n, lambda, beta){
  
  if(lambda <= 0 || beta <= 0) stop("the parameter values must be greater than 0.")
  
  u <- runif(n)
  return(
    exp(- (log(1 - log(u)/lambda)**(1/beta)))
  )
}

# set.seed(12)
# aa <- ruchen(200,1,1)
