# Cumulative distribution function --------------------------------------------#

puchen <- function(y, mu, sigma){
  
  if(mu <= 0 || sigma <= 0) stop("the parameter values must be greater than 0.")
  if(any(y <= 0) || any(y >= 1)) stop("y values must be between (0,1).")
  
  return(exp(mu*(1 - exp((-log(y))**sigma))))
}

# x <- seq(0.01, 0.99, by = 0.001)
# y <- puchen(x,1,2)
# plot(x,y)

# Probability density function ------------------------------------------------#

duchen <- function(y, mu, sigma, log = FALSE){
  
  if(mu <= 0 || sigma <= 0) stop("the parameter values must be greater than 0.")
  if(any(y <= 0) || any(y >= 1)) stop("y values must be between (0,1).")
  
  f <- mu * sigma * exp(mu * (1 - exp((-log(y))**sigma)) + (- log(y))**sigma) * 
    (-log(y))**(sigma - 1) / y
  
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

quchen <- function(p, mu, sigma){
  
  if(mu <= 0 || sigma <= 0) stop("the parameter values must be greater than 0.")
  if(any(p < 0) || any(p > 1)) stop("p values must be between (0,1).")
  
  return(
    exp(- (log(1 - log(p)/mu)**(1/sigma)))
  )
}

# x <- 0.5
# y <- puchen(x,1,2)
# (p <- quchen(y,1,2))

# Random number generator function --------------------------------------------#

ruchen <- function(n, mu, sigma){
  
  if(mu <= 0 || sigma <= 0) stop("the parameter values must be greater than 0.")
  
  u <- runif(n)
  return(
    exp(- (log(1 - log(u)/mu)**(1/sigma)))
  )
}

# set.seed(12)
# (aa <- ruchen(200,1,1))
# summary(aa)

# Log-likelihood function -----------------------------------------------------#

# loglike_uchen <- function(teta, y, m.optim = 1){
#   
#   mu <- teta[1]
#   sigma <- teta[2]
#   
#   if(mu <= 0 || sigma <= 0) stop("the parameter values must be greater than 0.")
#   if(any(y <= 0) || any(y >= 1)) stop("y values must be between (0,1).")
#   
#   lv <- sum(
#     -log(y) + log(mu*sigma) + (sigma - 1) * log(- log(y)) + (- log(y))**sigma +
#       mu * (1 - exp((- log(y))**sigma))
#   )
#   
#   if(m.optim == -1){return(-lv)}
#   if(m.optim == 1){return(lv)}
# }

# set.seed(12)
# aa <- ruchen(200,1,1)
# sum(duchen(aa,1,1, log = TRUE))
# loglike_uchen(c(1,1),aa)
