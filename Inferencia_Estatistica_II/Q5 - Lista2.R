# Gerando 30 ocorrências aleatótias de uma Poisson(2):

aa <- rpois(n = 30, lambda = 2)

# Intervalo de confiança para uma Poisson de parâmetro θ:
IC <- function(x, alpha){
  lower_bound <- mean(x) - qnorm(1-alpha/2) * mean(x) / sqrt(length(x)) 
  upper_bound <- mean(x) + qnorm(1-alpha/2) * mean(x) / sqrt(length(x)) 
  
  return(
    paste0(
      "IC(θ,", 1 - alpha, ") =  [", round(lower_bound, digits = 2), ";", 
      round(upper_bound, digits = 2), "]."
    )
  )
}

# Determinando um intervalo com alpha = 0,10 para θ considerando a amostra gerada:

IC(aa, 0.1)

# Repetimento o procedimento várias vezes:

# k = número de réplicas de Monte Carlo
# n = tamanho amostral 
# 1 - alpha = nível de confiança

simulation <- function(k, n, theta, alpha){
  # contador para o número de intervalos contendo o real valor de θ:
  count <- 0 
  
  for(i in 1:k){
    x <- rpois(n = n, lambda = theta)
    lower_bound <- mean(x) - qnorm(1-alpha/2) * mean(x) / sqrt(length(x)) 
    upper_bound <- mean(x) + qnorm(1-alpha/2) * mean(x) / sqrt(length(x)) 
    
    # Verificando se o intervalo contem o real valor do parâmetro:
    if(theta <= upper_bound & theta >= lower_bound) count <- count + 1
    
  }
  return(count/k)
}

# Testando a função de simulação:

simulation(k = 5000, n = 30, theta = 2, alpha = 0.10)
