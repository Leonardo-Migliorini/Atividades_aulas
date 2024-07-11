# Pacotes Utilizados -----------------------------------------------------------

library(hnp) # pacote para envelope simulado
library(lmtest) # teste reset
library(car) # para teste de multicolinearidade (fatores de inflacao de variancia)
library(tseries) # teste de Jarque-Bera
library(ggplot2)

load("Dados//Models_Data.RData")
dados_1 <- Models_Data
dados <- Models_Data |> 
  dplyr::mutate(
    GHGC = log(GHGC)
  ) |>
  dplyr::select(
    -c(1)
  )
  
dados <- dados[-c(8,11,12,32,123),]
attach(dados)

fit <- lm(GHGC ~ UNNC + UCAC ,data=dados) # ajustando o modelo
summary(fit)
step(fit)

# Análise de diagnóstico -------------------------------------------------------

# com a seguinte funcao se obtem varias medidas de influencia
im2 <- influence.measures(fit)
n1 <- nrow(dados)

# Comando para salvar os Plots em pdf ------------------------------------------

# Tamanho da imagem
{width <- 3
height <- 3
mar_b<-2.5
mar_e<-2.5
mar_c<-0.5
mar_d<-0.5
dist_text<-1.5
dist_tick<-0.5
}

# Resíduo e tamanho de amostra
residuo <- rstudent(fit) # residuo studentizado
n <- nrow(dados)

# Plot resíduo vs índice
plot_data_resxind <- data.frame(Indice = seq(n), Residuos = residuo)

pdf(file = "ResXind.pdf",width = width, height = height,family = "Times")
par(mar=c(mar_b, mar_e, mar_c, mar_d)) 
par(mgp=c(dist_text, dist_tick, 0))
plot(ggplot(plot_data_resxind, aes(x = Indice, y = Residuos)) +
  geom_point(shape = "+", size = 3) +
  geom_hline(yintercept = c(-3, 3), linetype = "dashed", color = "black") +
  geom_hline(yintercept = c(-2, 2), linetype = "dotted", color = "black") +
  ylim(c(min(c(-4, res)), max(c(4, res)))) +
  labs(x = "Índice", y = "Resíduos") +
  ggplot2::theme_bw() +
  ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                 panel.grid.minor = ggplot2::element_blank()))
dev.off()


# Plot Densidade dos resíduos

plot_data_dens <- data.frame(Residuos = res)


pdf(file = "densidade4.pdf",width = width, height = height,family = "Times")
par(mar=c(mar_b, mar_e, mar_c, mar_d)) 
par(mgp=c(dist_text, dist_tick, 0))
plot(ggplot(plot_data_dens, aes(x = Residuos)) +
  geom_density(aes(y = ..density..), size = 1) +  # Increased size for density line
  stat_function(fun = dnorm, args = list(mean = mean(res), sd = sd(res)), linetype = "dashed", size = 1, color = "blue") +  # Increased size for normal line
  ylim(0, 0.55) +
  labs(x = "Intervalo", y = "Densidade") +
  ggplot2::theme_bw() +
  ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                 panel.grid.minor = ggplot2::element_blank()) +
  theme(legend.position = "top") +
  guides(colour = guide_legend(override.aes = list(size = 5))) +
    annotate("segment", x = 0, xend = 0.5, y = 0.53, yend = 0.53, color = "black", size = 0.7) +
    annotate("segment", x = 0, xend = 0.5, y = 0.48, yend = 0.48, linetype = "dashed", color = "blue", size = 0.7) +  # Increase the size of legend keys
    annotate("text", x = 0, y = 0.55, label = "Densidade Estimada", hjust = 1.1, vjust = 1.5, size = 3) +
    annotate("text", x = 0, y = 0.5, label = "Normal Padrão", hjust = 1.1, vjust = 1.5, size = 3)) 
dev.off()

# Plot Envelope Simulado 

pdf(file = "Envelope_simulado.pdf",width = width, height = height,family = "Times")
par(mar=c(mar_b, mar_e, mar_c, mar_d)) 
par(mgp=c(dist_text, dist_tick, 0))
plot(res |> 
       as.data.frame() |> 
       ggplot2::ggplot(ggplot2::aes(sample = res)) +
       qqplotr::geom_qq_band(
         alpha = 0.5, fill = "white", col = "black", B = 100,
         bandType = "boot"
       ) +
       qqplotr::stat_qq_point(size = 0.8) +
       ggplot2::scale_fill_discrete("Bandtype") +
       ggplot2::labs(x = "Quantis Teóricos", y = "Resíduos") +
       ggplot2::theme_bw() +
       ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                      panel.grid.minor = ggplot2::element_blank()))
dev.off()

# Plot Distância de cook
cooks_dist <- cooks.distance(fit)
limite_cook <- 4 / (n - fit$rank)
plot_data_ck <- data.frame(Index = 1:length(cooks_dist), CooksDistance = cooks_dist)

pdf(file = "Cook_dist1.pdf",width = width, height = height,family = "Times")
par(mar=c(mar_b, mar_e, mar_c, mar_d)) 
par(mgp=c(dist_text, dist_tick, 0))
plot(ggplot(plot_data_ck, aes(x = Index, y = CooksDistance)) +
  geom_point(size = 1.5, shape = 1) +  # Increase the size of the points
  geom_hline(yintercept = limite_cook, linetype = "dashed", color = "blue") +
  labs(x = "Índice", y = "Distância de Cook") +
    ggplot2::theme_bw() +
    ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank()))
dev.off()

# Plot Alavancagem
h_bar <- fit$rank/n
hatvalues <- hatvalues(fit)
limite_ala <- 2*h_bar
plot_data_alav <- data.frame(Index = 1:length(hatvalues), hatvalues = hatvalues)

pdf(file = "Alavancagem.pdf",width = width, height = height,family = "Times")
par(mar=c(mar_b, mar_e, mar_c, mar_d)) 
par(mgp=c(dist_text, dist_tick, 0))
plot(ggplot(plot_data_alav, aes(x = Index, y = hatvalues)) +
       geom_point(size = 1.5, shape = 1) +  # Increase the size of the points
       geom_hline(yintercept = limite_ala, linetype = "dashed", color = "blue") +
       labs(x = "Índice", y = "Alavancagem") +
       ggplot2::theme_bw() +
       ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                      panel.grid.minor = ggplot2::element_blank()))
dev.off()

# Plot DFFITS
dffits <- dffits(fit)
limite_dffits <- 2*sqrt(fit$rank / n)

plot_data_dffit <- data.frame(Index = 1:length(dffits), dffits = dffits)

pdf(file = "DFFITS.pdf",width = width, height = height,family = "Times")
par(mar=c(mar_b, mar_e, mar_c, mar_d)) 
par(mgp=c(dist_text, dist_tick, 0))
plot(ggplot(plot_data_dffit, aes(x = Index, y = dffits)) +
       geom_point(size = 1.5, shape = 1) +  # Increase the size of the points
       geom_hline(yintercept = limite_dffits, linetype = "dashed", color = "blue") +
       geom_hline(yintercept = -limite_dffits, linetype = "dashed", color = "blue") +
       labs(x = "Índice", y = "DFFITS") +
       ggplot2::theme_bw() +
       ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                      panel.grid.minor = ggplot2::element_blank()))
dev.off()

# Plot DFBETAS
dfbetas <- dfbetas(fit) # cada beta tem seu DF
limite_dfbeta <- 2/sqrt(n)

# Plot DFBETA_0
plot_data_dfbeta0 <- data.frame(Index = 1:length(dfbetas[,1]), dfbetas = dfbetas[,1])

pdf(file = "DFBETA0.pdf",width = width, height = height,family = "Times")
par(mar=c(mar_b, mar_e, mar_c, mar_d)) 
par(mgp=c(dist_text, dist_tick, 0))
plot(ggplot(plot_data_dfbeta0, aes(x = Index, y = dfbetas)) +
       geom_point(size = 1.5, shape = 1) +  # Increase the size of the points
       geom_hline(yintercept = limite_dfbeta, linetype = "dashed", color = "blue") +
       geom_hline(yintercept = -limite_dfbeta, linetype = "dashed", color = "blue") +
       labs(x = "Índice", y = "DFBETA 0") +
       ggplot2::theme_bw() +
       ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                      panel.grid.minor = ggplot2::element_blank()))
dev.off()

# Plot DFBETA_1
plot_data_dfbeta1 <- data.frame(Index = 1:length(dfbetas[,2]), dfbetas = dfbetas[,2])

pdf(file = "DFBETA1.pdf",width = width, height = height,family = "Times")
par(mar=c(mar_b, mar_e, mar_c, mar_d)) 
par(mgp=c(dist_text, dist_tick, 0))
plot(ggplot(plot_data_dfbeta1, aes(x = Index, y = dfbetas)) +
       geom_point(size = 1.5, shape = 1) +  # Increase the size of the points
       geom_hline(yintercept = limite_dfbeta, linetype = "dashed", color = "blue") +
       geom_hline(yintercept = -limite_dfbeta, linetype = "dashed", color = "blue") +
       labs(x = "Índice", y = "DFBETA 1") +
       ggplot2::theme_bw() +
       ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                      panel.grid.minor = ggplot2::element_blank()))
dev.off()

# Plot DFBETA_2
plot_data_dfbeta2 <- data.frame(Index = 1:length(dfbetas[,3]), dfbetas = dfbetas[,3])

pdf(file = "DFBETA2.pdf",width = width, height = height,family = "Times")
par(mar=c(mar_b, mar_e, mar_c, mar_d)) 
par(mgp=c(dist_text, dist_tick, 0))
plot(ggplot(plot_data_dfbeta2, aes(x = Index, y = dfbetas)) +
       geom_point(size = 1.5, shape = 1) +  # Increase the size of the points
       geom_hline(yintercept = limite_dfbeta, linetype = "dashed", color = "blue") +
       geom_hline(yintercept = -limite_dfbeta, linetype = "dashed", color = "blue") +
       labs(x = "Índice", y = "DFBETA 2") +
       ggplot2::theme_bw() +
       ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                      panel.grid.minor = ggplot2::element_blank()))
dev.off()


# Plot DFBETAS
abline(plot(dfbetas(fit)[,1],ylab="DFBETA 1"),
       col=c("red","blue","red"), h=c(-limite_dfbeta,0,limite_dfbeta),lty=c(2,1,2))

abline(plot(dfbetas(fit)[,2],ylab="DFBETA 2"),
       col=c("red","blue","red"), h=c(-limite_dfbeta,0,limite_dfbeta),lty=c(2,1,2))

abline(plot(dfbetas(fit)[,3],ylab="DFBETA 3"),
       col=c("red","blue","red"), h=c(-limite_dfbeta,0,limite_dfbeta),lty=c(2,1,2))

# Pontos possivelmente influentes ----------------------------------------------


print("Alavancagem")
which(hatvalues(fit) > limite_ala)

print("DFFITS")
which(abs(dffits(fit))>limite_dffits)

print("DFBETAS")
for(i in 1:ncol(dfbetas)){
  print(paste0("Beta", i))
  print(which(abs(dfbetas(fit))[,i]>limite_dfbeta))
}

print("Distância de Cook")
which(abs(cooks.distance(fit))>limite_cook)

print("Resíduo")
which(abs(residuo)>3)

