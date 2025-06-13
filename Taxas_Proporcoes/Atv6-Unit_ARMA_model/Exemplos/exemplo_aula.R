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
#############################
# implementando o ARMA(1,1) #
#############################
# valores iniciais
set.seed(1)
alpha<-.3;phi<-.2;theta<-.6
y<-c()
link <- make.link("logit")
#----------------------------------
## passo a passo ARMA(1,1), n=3
#----------------------------------
# inicializamos os termos AR e MA em 0
#t=1
eta1<-alpha # phi*0+theta*0
mu1<-link$linkinv(eta1)
y[1]<-round(rur(1,mu1),2)
#t=2
eta2<-alpha+
  phi*link$linkfun(y[1])+
  theta*(link$linkfun(y[1])-eta1)
mu2<-link$linkinv(eta2)
y[2]<-round(rur(1,mu2),2)
# t=3
eta3<-alpha+
  phi*link$linkfun(y[2])+
  theta*(link$linkfun(y[2])-eta2)
mu3<-link$linkinv(eta3)
y[3]<-round(rur(1,mu3),2)
# verificando o eta3
0.3+.2*link$linkfun(.43)+
  .6*(link$linkfun(.43)-link$linkfun(mu2))
eta3

# criando vetores com todas as quantidades necessárias
mu_passo_a_passo<-c(mu1,mu2,mu3)
eta_passo_a_passo<-c(eta1,eta2,eta3)
ynew_passo_a_passo<-link$linkfun(y)
error_passo_a_passo<-ynew_passo_a_passo[1:3]-eta_passo_a_passo

#----------------------------------
## uma maneira melhor 
#----------------------------------
# vetores iniciais
ar<-1; ma<-1 #ARMA(1,1)
m <- 1
n<-length(y)
ynew <-rep(link$linkfun(y[1]),(n))
mu <- link$linkinv(ynew)
error<-rep(ynew[1]-eta1,n) 
eta <- rep(NA,n)
for(i in (m+1):n)
{
  eta[i]  <- alpha + phi%*%ynew[i-1] + 
    theta%*%error[i-1]
  mu[i]   <- link$linkinv(eta[i])
  # y[i]    <- rur(1,mu[i])
  ynew[i] <- link$linkfun(y[i])
  error[i]<- ynew[i]-eta[i]   
}

#----------------------------------
## precisamos gerar y em cada réplica
## utilizando mu[i] e criar uma funcao
## para gerar a amostra
#----------------------------------
set.seed(1)

simu.urarma<-function(n,alpha=.3,phi=.2,theta=0.6,
                      tau=0.5,freq=12,link1="logit"){
  # vetores iniciais
  ar<-1:length(phi); ma<-1:length(theta)
  p <- max(ar)
  q <- max(ma)
  m <- 2*max(p,q)
  ynew <-rep(alpha,(n))
  mu <- link$linkinv(ynew)
  error<-rep(0,n+m) 
  eta <- rep(NA,n+m)
  link <- make.link(link1)
  for(i in (m+1):n+m)
  {
    eta[i]  <- alpha + phi%*%ynew[i-1] + 
      theta%*%error[i-1]
    mu[i]   <- link$linkinv(eta[i])
    y[i]    <- rur(1,mu[i])
    ynew[i] <- link$linkfun(y[i])
    error[i]<- ynew[i]-eta[i]   
  }
  return( ts(y[(m+1):(n+m)],frequency=freq) )
}
simu.urarma(n)





