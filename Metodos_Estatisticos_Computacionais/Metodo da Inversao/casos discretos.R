# Testando o algoritmo para um exemplo -----------------------------------------

#  Seja X um v.a. discreta que assume valores em {1,2,3,4} com f.p. dada por
# p1 = 0.2, p2 = 0.15, p3 = 0.25, p4 = 0.4. Obtenha uma amostra de tamanho 100
# da distribuição de X.

# Gerando uma observação
U <- runif(1)
if(U < 0.2) x <- 1
if(U < 0.35 && U >= 0.2) x <- 2
if(U < 0.6 && U >= 0.35) x <- 3
if(U < 1 && U >= 0.6) x <- 4

# Gerando uma amostra de tamanho 100

x <- c() # vetor para armazenar os valores gerados

for(i in 1:100){
  U <- runif(1)
  if(U < 0.2) x[i] <- 1
  if(U < 0.35 && U >= 0.2) x[i] <- 2
  if(U < 0.6 && U >= 0.35) x[i] <- 3
  if(U < 1 && U >= 0.6) x[i] <- 4
}

# Tentando generalizar para a distribuição Poisson


gerador_poisson <- function(n, lambda){
  
  x <- c() # vetor para armazenar os valores gerados
  
  for(i in i:n){
    
    U <- runif(1) # gerando o valor das variáveis uniformes
    
    if (U < ppois(0,lambda)){ # verificando se x = 0
      
      x[i] <- 0
      
    } else{ # verificando qual o valor de x, quando x != 0
      
      j <- 0
      
      while(U > ppois((j+1), lambda) | U <= ppois(j, lambda)){
        
        j <- j + 1
        
      }
      
    }
      x[i] <- j + 1 # retornando o valor de x
  }
  
  return(x)
}

# Testando a função
set.seed(15)
amostra <- gerador_poisson(200, 2)

#### ---------------------------------------------------------------------- ####