# Gerador Midsquare (John von Neumann, 1949) -----------------------------------

seed <- 2100 # semente de geração (precisa ser um número de 4 dígitos)
n <- 50 # tamanho da amostra desejada
x <- c() # vetor de valores gerados pelo algoritmo

# Algoritmo reprodutível n vezes
for(i in 1:n){
  
  # selecionando o valor para gerar a próxima iteração
  if(i == 1){ # ponto de partida
    x_temp <- seed**2
  }
  else{ # utilizando o valor da iteração anterior para gerar o próximo
    j <- i - 1
    x_temp <- x[j]**2
  }
  
  # verificando e ajustando o valor de x_temp para que ele tenha 8 dígitos
  if(stringr::str_length(x_temp) < 8){
    k <- 8 - stringr::str_length(x_temp)
    x_temp <- paste0(paste(rep("0", k), collapse = ""), x_temp)
  }
  
  # Salvando o valor gerado pela iteração 
  x[i] <- as.numeric(stringr::str_sub(x_temp, start = 3, end = 6))
    
}

# Calculando a amostra da uniforme (0,1) gerada.
U <- x/10000
print(U) # imprimindo os valores da amostra gerada.

# Definindo o gerador como uma função do R -------------------------------------

gerador_midsquare <- function(seed, n){
  
  x <- c() # vetor de valores gerados pelo algoritmo
  
  # Algoritmo reprodutível n vezes
  for(i in 1:n){
    
    # selecionando o valor para gerar a próxima iteração
    if(i == 1){ # ponto de partida
      x_temp <- seed**2
    }
    else{ # utilizando o valor da iteração anterior para gerar o próximo
      j <- i - 1
      x_temp <- x[j]**2
    }
    
    # verificando e ajustando o valor de x_temp para que ele tenha 8 dígitos
    if(stringr::str_length(x_temp) < 8){
      k <- 8 - stringr::str_length(x_temp)
      x_temp <- paste0(paste(rep("0", k), collapse = ""), x_temp)
    }
    
    # Salvando o valor gerado pela iteração 
    x[i] <- as.numeric(stringr::str_sub(x_temp, start = 3, end = 6))
    
  }
  
  # Calculando a amostra da uniforme (0,1) gerada.
  U <- x/10000
  return(U)
}

# Gere com x0 = 2100 e veja o que acontece. Qual ´e o per´ıodo com essa semente?

gerador_midsquare(seed = 2100, n = 100)

# A sequência de números 0.41, 0.81, 0.61, 0.21 se repete indefinidamente. 
# O período dessa semente é justamente a sequência 0.41, 0.81, 0.61, 0.21.
#### ---------------------------------------------------------------------- ####