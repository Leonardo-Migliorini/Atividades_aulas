# Função densidade de probabilidade --------------------------------------------

dpareto <- function(x, a, b){ # x >= b
  
  f <- a * b**a / x**(a + 1)
  
  return(f)
}

# x <- seq(1, 5, by = 0.01)
# y <- dpareto(x, 1.2, 1)
# plot(x,y)

# Função de distribuição de probabilidade --------------------------------------

ppareto <- function(x, a , b){
  
  f <- 1 - (b / x)**a
  
  return(f)
}

# x <- seq(1, 10, by = 0.01)
# y <- ppareto(x, 1.2, 1)
# plot(x,y)

# Função quantílica ------------------------------------------------------------

qpareto <- function(q, a, b){
  
  f <- b / (1 - q)**(1 / a)

  return(f)  
}

# q <- ppareto(0.3, 1.2, 1)
# qpareto(q, 1.2, 1)

# Função geradora de números aleatórios (método da inversão) -------------------

rpareto <- function(n, a, b){

  u <- runif(n)
    
  f <- b / (1 - u)**(1 / a)
  
  return(f)
}

# Fixando a semente de geração
set.seed(15)
# Gerando a amostra de uma Pareto(1,2)
amostra <- rpareto(200, 1, 2)

# Verificando se a amostra vem de uma distribuição Pareto(1,2)
ks.test(amostra, "ppareto", 1, 2)
