# Função densidade de probabilidade --------------------------------------------

dkuma <- function(x, alpha, beta){ 
  
  f <- alpha * beta * x**(alpha - 1) * (1 - x**alpha)**(beta - 1)
  return(f)
}

# x <- seq(0, 1, by = 0.001)
# y <- dkuma(x, 2, 1.2)
# plot(x,y)

# Função de distribuição de probabilidade --------------------------------------

pkuma <- function(x, alpha , beta){
  
  f <- 1 - (1 - x**alpha)**beta
  
  return(f)
}

# x <- seq(0, 1, by = 0.01)
# y <- pkuma(x, 1.2, 1)
# plot(x,y)

# Função quantílica ------------------------------------------------------------

qkuma <- function(q, alpha, beta){
  
  f <- (1 - (1 - q)**(1 / beta))**(1 / alpha)
  
  return(f)  
}

q <- pkuma(0.3, 1.2, 1)
qkuma(q, 1.2, 1)

# Função geradora de números aleatórios (método da inversão) -------------------

rkuma <- function(n, alpha, beta){
  
  u <- runif(n)
  
  f <- (1 - (1 - u)**(1 / beta))**(1 / alpha)
  
  return(f)
}

# Fixando a semente de geração
set.seed(15)
# Gerando a amostra de uma Kumaraswamy(1,2)
amostra <- rkuma(200, 1, 2)

# Verificando se a amostra vem de uma distribuição Kumaraswamy(1,2)
ks.test(amostra, "pkuma", 1, 2)
