# Aplicando o método da aceitação-rejeição em um exemplo -----------------------

# Geração de Gaussianas via Cauchy padrão

# Testando para uma observação
M <- 1.520347
# Gerando as amostras aleatórias
U <- runif(1)
y <- rcauchy(1)

# Calculando o possível valor da amostra
temp <- dnorm(y)/M*dcauchy(y)

# Verificando se o valor pertencera a amostra
if(U <= temp){
  x <- y
} else{ # Refazendo iterações até que um valor seja assumido para a amostra
  j <- 1
  while(U > temp){
    U <- runif(1)
    y <- rcauchy(1)
    temp <- dnorm(y)/M*dcauchy(y)
    j <- j + 1
  }
  x <- y
  paste0("Total de Iterações: ", j)
}

#  Função do gerador de Gaussianas através de Cauchy's pelo método da 
# aceitação-rejeição

gerador_gaussiana <- function(n, M = 1.520347){
  
  # Vetor para armazenar a amostra gerada
  x <- c()
  
  for(i in 1:n){
    # Gerando as amostras aleatórias
    U <- runif(1)
    y <- rcauchy(1)
    
    # Calculando o possível valor da amostra da iteração i
    temp <- dnorm(y)/M*dcauchy(y)
    
    # Verificando se o valor pertencera a amostra
    if(U <= temp){
      x[i] <- y
    } else{ # Refazendo iterações até que um valor seja assumido para a amostra
      while(U > temp){
        U <- runif(1)
        y <- rcauchy(1)
        temp <- dnorm(y)/M*dcauchy(y)
      }
      x[i] <- y
    }
  }
  
  return(x)
  
}

# Verificando se a amostra veio de uma distribuição Gaussiana
amostra <- gerador_gaussiana(50, 3)
hist(amostra)

shapiro.test(amostra)
