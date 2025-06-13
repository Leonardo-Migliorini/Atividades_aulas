
# density function
dur<-function(y,mu,tau=.5)
{
  d<-2/y*log(tau)*log(y)/log(mu)^2*tau^(log(y)/log(mu))^2
  d
}

# cumulative distribution function
pur<-function(y,mu,tau=.5)
{
  p<- tau^(log(y)/log(mu))^2
  p
}

# quantile function
qur<-function(y,mu,tau=.5)
{
  q<- mu^((log(u)/log(tau))^(1/2))
  q
}

# inversion method for randon generation
rur<-function(n,mu,tau=.5)
{
  u<- runif(n)
  y<- mu^((log(u)/log(tau))^(1/2))
  y
}


