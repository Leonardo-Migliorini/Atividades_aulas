# Função densidade de probabilidade --------------------------------------------

dexp <- function(x, lambda){ 
  
  f <- lambda * exp(- lambda * x)
  
  return(f)
}

# x <- seq(0.01, 5, by = 0.01)
# y <- dexp(x, 1)
# plot(x,y)

# Função de distribuição de probabilidade --------------------------------------

pexp <- function(x, lambda){
  
  f <- 1 - exp( - lambda * x)
  
  return(f)
}

x <- seq(0, 5, by = 0.01)
y <- pexp(x, 1)
plot(x,y)

# Função quantílica ------------------------------------------------------------

qexp <- function(q, lambda){
  
  f <- - log(1 - q) / lambda
  
  return(f)  
}

q <- pexp(0.3, 1)
qexp(q, 1)

# Função geradora de números aleatórios (método da inversão) -------------------

rexp <- function(n, lambda){
  
  u <- runif(n)
  
  f <- - log(1 - u) / lambda
  
  return(f)
}

# Fixando a semente de geração
set.seed(15)
# Gerando a amostra de uma Exp(1)
amostra <- rexp(1000, 1)

# Verificando se a amostra vem de uma distribuição Exp(1)
ks.test(amostra, "pexp", 1)
