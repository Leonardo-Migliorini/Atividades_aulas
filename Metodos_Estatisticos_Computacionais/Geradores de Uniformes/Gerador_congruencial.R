# Gerador Congruencial ---------------------------------------------------------

seed <- 0 # semente de geração dos valores
M <- 64 # valor do módulo
a <- 29 # valor do multiplicador
c <- 17 # valor do deslocamento
x <- c() # vetor de valores gerado pelo algoritmo

for(i in 1:M){
  
  # definindo o valor de x_i - 1
  if(i == 1){ # ponto de partida do gerador, ou seja, a seed
    x_temp <- seed
  }
  else{ # valor gerado no iteração anterior (i - 1)
    k <- i-1
    x_temp <- x[k]
  }
  
  x[i] <- (a * x_temp + c) %% M # gerando e armazendo o valor da iteração i
}

print(x) # imprimindo os valores gerados
length(x) # perceba que o número de valores gerados condiz com o valor de M

# Transformando os valores para satisfazerem uma sequência de uniformes
U <- x/M
print(U)

# Função do Gerador Congruencial -----------------------------------------------

gerador_congruencial <- function(seed, M, a, c){
  
  x <- c() # vetor de valores gerado pelo algoritmo
  
  for(i in 1:M){
    
    # definindo o valor de x_i - 1
    if(i == 1){ # ponto de partida do gerador, ou seja, a seed
      x_temp <- seed
    }
    else{ # valor gerado no iteração anterior (i - 1)
      k <- i-1
      x_temp <- x[k]
    }
    
    x[i] <- (a * x_temp + c) %% M # gerando e armazendo o valor da iteração i
  }
  
  return(x)
}

gerador_congruencial_uniforme <- function(seed, M, a, c){
  
  x <- c() # vetor de valores gerado pelo algoritmo
  
  for(i in 1:M){
    
    # definindo o valor de x_i - 1
    if(i == 1){ # ponto de partida do gerador, ou seja, a seed
      x_temp <- seed
    }
    else{ # valor gerado no iteração anterior (i - 1)
      k <- i-1
      x_temp <- x[k]
    }
    
    x[i] <- (a * x_temp + c) %% M # gerando e armazendo o valor da iteração i
  }
  
  return(x/M)
}

# Fazendo o exercício da apostila:

# O que podemos verificar nessa sequência??
gerador_congruencial(0, 16, 1, 1)
gerador_congruencial_uniforme(0, 16, 1, 1)

# Reposta:

# Podemos verificar que ele retorna a sequência de valores {1, 2, ..., M-1, 0}.

#### ---------------------------------------------------------------------- ####