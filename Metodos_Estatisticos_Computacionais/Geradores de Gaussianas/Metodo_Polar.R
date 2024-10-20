# Método Polar para uma ocorrênica ---------------------------------------------

#  Vamos gerar duas variáveis independentes U_1 e U_2 com distribuição U(0,1),
# utilizando os geradores implementados no R.

# Escolhendo o gerador:
RNGkind("Mersenne-Twister")
# Definindo a semente de geração:
# set.seed(21)
#  Não da para fixar uma semente de geração direito porque não teriamos como 
# gerar novos valores no caso em que w > 1.

# Gerando os valores da variáveis aleatórias uniformes
U_1 <- runif(1)
U_2 <- runif(1)

# Calculando as transformações do passo (2)
x <- 2 * U_1 - 1
y <- 2 * U_2 - 1

# Gerando os valores das variáveis aleatórias gaussianas
w <- x**2 + y**2

# Verificando se w > 1
if(w > 1){
  U_1 <- runif(1)
  U_2 <- runif(1)
  
  x <- 2 * U_1 - 1
  y <- 2 * U_2 - 1
  
  w <- x**2 + y**2
} else{
  Z_1 <- sqrt((- 2 * log(w))/w)*x
  Z_2 <- sqrt((- 2 * log(w))/w)*y
}

# Gerando múltiplas ocorrências

gerador_polar <- function(n){
  
  # criando alguns vetores para armazenar os resutados
  w <- c()
  x <- c()
  y <- c()
  
  # gerando os valores para uma amostra de tamanho n
  for(i in 1:n){
    U_1 <- runif(1)
    U_2 <- runif(1)
    
    x_temp <- 2 * U_1 - 1
    y_temp <- 2 * U_2 - 1
    
    w_temp <- x_temp**2 + y_temp**2
    
  # Verificando a condição de w > 1
    while(w_temp > 1){
      U_1 <- runif(1)
      U_2 <- runif(1)
      
      x_temp <- 2 * U_1 - 1
      y_temp <- 2 * U_2 - 1
      
      w_temp <- x_temp**2 + y_temp**2
    }
    
    x[i] <- x_temp
    y[i] <- y_temp
    w[i] <- w_temp
  }
  
  # Dividindo o tamanho amostra igualmente para cada transformação
  if(n %% 2 == 0){
    w_1 <- w[1:(n / 2)]
    w_2 <- w[(n / 2 + 1):n]
    x_1 <- x[1:(n / 2)]
    x_2 <- x[(n / 2 + 1):n]
    y_1 <- y[1:(n / 2)]
    y_2 <- y[(n / 2 + 1):n]
  }
  else{
    w_1 <- w[1:(n %/% 2)]
    w_2 <- w[(n %/% 2 + 1):n]
    x_1 <- x[1:(n %/% 2)]
    x_2 <- x[(n %/% 2 + 1):n]
    y_1 <- y[1:(n %/% 2)]
    y_2 <- y[(n %/% 2 + 1):n]
  }
  
  # Calculando os valores da variáveis gaussianas 
    Z_1 <- sqrt(- 2 * log(w_1) / w_1) * x_1
    Z_2 <- sqrt(- 2 * log(w_2) / w_2) * y_2
  
  # Juntando os valores em um único vetor
    Z <- c(Z_1, Z_2)
    
  return(Z)
}

# Testando a função

values <- gerador_polar(1000)

# Null hypothesis: The sample was generated from a normal distribution
hist(values)
shapiro.test(values)

#### ---------------------------------------------------------------------- ####