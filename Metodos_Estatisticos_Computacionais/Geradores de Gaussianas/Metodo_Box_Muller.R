# Método de Box-Muller para uma ocorrência -------------------------------------

#  Vamos gerar duas variáveis independentes U_1 e U_2 com distribuição U(0,1),
# utilizando os geradores implementados no R.

# Escolhendo o gerador:
RNGkind("Mersenne-Twister")
# Definindo a semente de geração:
set.seed(15)

U_1 <- runif(1)
U_2 <- runif(1)

# Aplicando a transformação para gerar os valores da Normal Padrão:

x <- sqrt(- 2 * log(U_1)) * cos(2 * pi * U_2)
y <- sqrt(- 2 * log(U_1)) * sin(2 * pi * U_2)

# Gerando múltiplas ocorrências 

gerador_box_muller <- function(seed, n){
  
  U_1 <- runif(n)
  U_2 <- runif(n)
 
  # Dividindo o tamanho amostra igualmente para cada transformação
  if(n %% 2 == 0){
    # Aplicando a transformação para gerar os valores da Normal Padrão:
    x_1 <- sqrt(- 2 * log(U_1[1:(n/2)])) * cos(2 * pi * U_2[1:(n/2)])
    x_2 <- sqrt(- 2 * log(U_1)[1:(n/2+1)]) * sin(2 * pi * U_2[1:(n/2+1)])
  }
  else{
    # Aplicando a transformação para gerar os valores da Normal Padrão:
    x_1 <- sqrt(- 2 * log(U_1[1:(n %/% 2)])) * cos(2 * pi * U_2[1:(n %/% 2)])
    x_2 <- sqrt(- 2 * log(U_1)[1:(n %/% 2)]) * sin(2 * pi * U_2[1:(n %/% 2)])
  }
    
    x <- c(x_1, x_2)
    
  return(x)
}

# Testando a função
values <- gerador_box_muller(15, 1000)

# Visualizando um histograma dos valores gerados
hist(values)

# Testando se a amostra é de uma distribuição Normal
# Null hypothesis: The sample was generated from a normal distribution
shapiro.test(values)

#### ---------------------------------------------------------------------- ####