
dados <- read.table("dados-trabalho1.txt", h = TRUE) 

n <- dim(dados)

summary(dados) # medidas descritivas do banco de dados

cor_dados <- cor(dados) 
corrplot::corrplot(cor_dados, 
                   type = "upper", 
                   method = "color", 
                   col = RColorBrewer::brewer.pal(n = 8, name = "RdYlBu"), # Color palette
                   addCoef.col = "black", # Add correlation coefficients
                   tl.col = "black", # Text label color
                   tl.srt = 45, # Text label rotation
                   number.cex = 0.7, # Size of the correlation coefficient numbers
                   cl.cex = 0.7, # Size of the color legend text
                   title = "Gráfico de correlação das variáveis", 
                   mar = c(0,0,1,0)) # Title margin


shapiro.test(dados$y)

fit <- lm(y ~ x1 + x2 + x3 + x4 + x5 + x6 + x7,data=dados) # ajustando o modelo
summary(fit) # printado a saída do modelo

step(fit) # selecionando o melhor modelo baseado no AIC e stepwise

# ---------------------------------------------------------------------------- #

load("Models_Data1.RData")
dados <- Models_Data |> 
  dplyr::mutate(
    # CO2 = log(CO2)
    CO2 = (CO2-mean(CO2))/sd(CO2)
  ) |> 
  dplyr::select(
    -c(1)
  )
attach(dados)
summary(dados)
hist(CO2)
shapiro.test(CO2)

cor_dados <- cor(dados) 
corrplot::corrplot(cor_dados, 
                   type = "upper", 
                   method = "color", 
                   col = RColorBrewer::brewer.pal(n = 8, name = "RdYlBu"), # Color palette
                   addCoef.col = "black", # Add correlation coefficients
                   tl.col = "black", # Text label color
                   tl.srt = 45, # Text label rotation
                   number.cex = 0.7, # Size of the correlation coefficient numbers
                   cl.cex = 0.7, # Size of the color legend text
                   title = "Gráfico de correlação das variáveis", 
                   mar = c(0,0,1,0)) # Title margin



fit <- lm(CO2 ~ UNNC + UCAC + GCCP + OR + DGCC + DOCC,data=dados) # ajustando o modelo
summary(fit)
step(fit)
