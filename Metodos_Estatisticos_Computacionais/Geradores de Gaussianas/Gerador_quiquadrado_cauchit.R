#  Vamos utilizar o gerador de Box Muller para gerar os valores das variáveis 
# gaussimas

# Função do gerador de Box Muller
gerador_box_muller <- function(n, seed = NULL){
  
  # Verificando se o usuário deseja fixar uma seed de geração
  if(is.null(seed) != TRUE){
    set.seed(seed)
  }
  
  # Gerando os valores das variáveis com distribuição uniforme
  U_1 <- runif(n)
  U_2 <- runif(n)
  
  # Dividindo o tamanho amostra igualmente para cada transformação
  if(n %% 2 == 0){
    # Aplicando a transformação para gerar os valores da Normal Padrão:
    x_1 <- sqrt(- 2 * log(U_1[1:(n/2)])) * cos(2 * pi * U_2[1:(n/2)])
    x_2 <- sqrt(- 2 * log(U_1)[(n/2+1):n]) * sin(2 * pi * U_2[(n/2+1):n])
  }
  else{
    # Aplicando a transformação para gerar os valores da Normal Padrão:
    x_1 <- sqrt(- 2 * log(U_1[1:(n %/% 2)])) * cos(2 * pi * U_2[1:(n %/% 2)])
    x_2 <- sqrt(- 2 * log(U_1)[(n %/% 2 + 1):n]) * sin(2 * pi * U_2[(n %/% 2 + 1):n])
  }
  
  # Juntando os valores em um único vetor
  if(n == 1){   # Forçando o retorno de apenas um valor quando n = 1
    x <- c(x_1)
  }
  else{
    x <- c(x_1, x_2)
  }
  
  return(x)
}

# Gerando Qui-Quadrados com 1 grau de liberdade --------------------------------

# Gerando uma observação
z <- gerador_box_muller(1, 15)

chi_1 <- z**2

# Gerando um vetor de observações

z <- gerador_box_muller(15,200)

chi_1 <- z**2

#  Função para gerar um vetor de variáveis aleatórias com distribuição Qui-Quadrado
# com 1 grau de liberdade

gerador_qui1 <- function(n, seed = NULL, method = c("Box Muller", "R")){
  
  # Verificando se o usuário deseja fixar uma seed de geração
  if(is.null(seed) != TRUE){
    set.seed(seed)
  }
  
  # verificando o método de geração escolhido
  method <- match.arg(method) 
  
  if(method == "Box Muller"){
    x <- gerador_box_muller(n, seed = seed)
  }
  else{
    x <- rnorm(n)
  }
  
  x2 <- x**2
  
  return(x2)
  
}

chi_1 <- gerador_qui1(1000)

# Plotando o histograma dos valores gerados
hist(chi_1)
# Testando se a amostra segue uma distribuição Qui-Quadrado 1
ks.test(chi_1, "pchisq" ,1) 
# The null hypothesis that x and y were drawn from the same distribution is performed.

# Gerando qui-quadrados com m graus de liberdade -------------------------------

# Gerando uma observação

#  Perceba que agora o tamanho de amostro do gerador é igual a quatidade de
# graus de liberdade da Qui-Quadrado, ou seja n = m.

z <- gerador_box_muller(15, n = 10)

chi_10 <- sum(z**2)

# Função para gerar um vetor de Qui-Quadrados com m graus de liberdade

gerador_mqui <- function(n, m, seed = NULL, method = c("Box Muller", "R")){
  
  # Verificando se o usuário deseja fixar uma seed de geração
  if(is.null(seed) != TRUE){
    set.seed(seed)
  }
  
  method <- match.arg(method) # verificando o método de geração escolhido
  x2 <- c() # vetor da amostra da Qui-Quadrado de tamanho n
  
  if(method == "Box Muller"){
    # calculando a amostra da Qui-Quadrado com m graus de liberdade
    for(j in 1:n){
    x_temp <- gerador_box_muller(n = m)
    x2[j] <- sum(x_temp**2)
    }
  }
  else{
    # calculando a amostra da Qui-Quadrado com m graus de liberdade
    for(j in 1:n){
    x_temp <- rnorm(n = m)
    x2[j] <- sum(x_temp**2)
    }
  }
  
  return(x2)
}

chi_1000 <- gerador_mqui(1000, 4, method = "R")

# Plotando o histograma dos valores gerados
hist(chi_1000)
# Testando se a amostra segue uma distribuição Qui-Quadrado 1
ks.test(chi_1000, "pchisq", 4) # obs ajustar os graus de liberdade com os da amostra gerada
# The null hypothesis that x and y were drawn from the same distribution is performed.

# Gerando valore para uma variável aleatória com distribuição Cauchit ----------

# Gerando uma observação

z <- gerador_box_muller(2, 15)

cauchit <- z[1]/z[2]

# Função para gerar um vetor de cauchit

gerador_cauchit <- function(n, seed, method = c("Box Muller", "R")){
  
  # Verificando se o usuário deseja fixar uma seed de geração
  if(is.null(seed) != TRUE){
    set.seed(seed)
  }
  
  method <- match.arg(method) # verificando o método de geração escolhido
  
  if(method == "Box Muller"){
    # calculando as amostras das Gaussianas
    x1 <- gerador_box_muller(n = n)
    x2 <- gerador_box_muller(n = n)
  }
  else{
    # calculando as amostras das Gaussianas
    x1 <- rnorm(n)
    x2 <- rnorm(n)
  }
  
  # calculando a amostra da Cauchit
  cauchit <- x1/x2
  
  return(cauchit)
}

cauchit <- gerador_cauchit(1000, 15, "R")

# Plotando o histograma dos valores gerados
hist(cauchit)

# Testando se a amostra segue uma distribuição Cauchit
ks.test(cauchit, "pcauchy", 0, 1) 
# The null hypothesis that x and y were drawn from the same distribution is performed.

#### ---------------------------------------------------------------------- ####