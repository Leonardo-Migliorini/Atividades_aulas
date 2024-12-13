# Carregando pacotes
library(hnp) # fazer envelopes simulados
library(MASS) # para poder utilizar o Modelo Binomial Negativa
library(modEvA) # Calcular R2 de Nagelkerke
library(ggplot2)

# Inclusão de nova covariável
load("C:/Users/leona/Dropbox/Faculdade/6° Semestre/Modelos Lineares Generalizados/Trabalho Final/Ajustes/Models_Data_new.RData") # CO2

dados_temp <- dados_new |> 
  dplyr::select(
    -c(1)
  )
summary(dados_temp)

# Arrumando as variáveis Dummies
continent <- fastDummies::dummy_cols(as.character(dados_temp[,1]))[,-1]

colnames(continent)<-c("Africa","America","Asia","Europe","Ocenia")

# Caso precise remover medidas influentes

dados <- cbind(dados_temp,continent)
# summary(dados)

# Análise Descritiva dos dados -------------------------------------------------

# Comando para salvar os Plots em pdf 

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

# Corrplot
dados_corr <- dados |> 
  dplyr::select(
    GHGC, UNNC, UCAC, DOCC
  ) 

pdf(file = "corrplot.pdf",width = width, height = height,family = "Times")
par(mar=c(mar_b, mar_e, mar_c, mar_d)) 
par(mgp=c(dist_text, dist_tick, 0))
plot(GGally::ggcorr(dados_corr,label=1, digits = 4))
dev.off()

# Histograma

# GHGC
pdf(file = "hist.GHGC.pdf",width = width, height = height,family = "Times")
par(mar=c(mar_b, mar_e, mar_c, mar_d)) 
par(mgp=c(dist_text, dist_tick, 0))
plot(ggplot(dados_corr, aes(x = GHGC)) +
  geom_histogram(binwidth = 3, color = "black", fill = "#97C9EE") +
  labs(
    x = "GHGC",
    y = "Frequência"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 12, family = "serif"),
    axis.title.y = element_text(angle = 90)
  ))
dev.off()

# UNNC
pdf(file = "hist.UNNC.pdf",width = width, height = height,family = "Times")
par(mar=c(mar_b, mar_e, mar_c, mar_d)) 
par(mgp=c(dist_text, dist_tick, 0))
plot(ggplot(dados_corr, aes(x = UNNC)) +
  geom_histogram(binwidth = 30, color = "black", fill = "#97C9EE") +
  labs(
    x = "UNNC",
    y = "Frequência"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 12, family = "serif"),
    axis.title.y = element_text(angle = 90)
  ))
dev.off()

# UCAC
pdf(file = "hist.UCAC.pdf",width = width, height = height,family = "Times")
par(mar=c(mar_b, mar_e, mar_c, mar_d)) 
par(mgp=c(dist_text, dist_tick, 0))
plot(ggplot(dados_corr, aes(x = UCAC)) +
  geom_histogram(binwidth = 0.1, color = "black", fill = "#97C9EE") +
  labs(
    x = "UCAC",
    y = "Frequência"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 12, family = "serif"),
    axis.title.y = element_text(angle = 90)
  ))
dev.off()

# DOCC
pdf(file = "hist.DOCC.pdf",width = width, height = height,family = "Times")
par(mar=c(mar_b, mar_e, mar_c, mar_d)) 
par(mgp=c(dist_text, dist_tick, 0))
plot(ggplot(dados_corr, aes(x = DOCC)) +
  geom_histogram(binwidth = 0.5, color = "black", fill = "#97C9EE") +
  labs(
    x = "DOCC",
    y = "Frequência"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 12, family = "serif"),
    axis.title.y = element_text(angle = 90)
  ))
dev.off()

# Modelo Ajustado --------------------------------------------------------------

# Removendo pontos possivelmente influentes
dados <- dados[-c(20,79,11,66,28,8,27,73,53,68,43,82),]

# Modelo Nulo
null_fit <- glm(GHGC ~ 1 , family = Gamma(link = "log"), data=dados) # ajustando o modelo

# Modelo ajustado
fit <- glm(GHGC ~ UNNC + UCAC + DOCC , family = Gamma(link = "log"), data=dados) # ajustando o modelo
summary(fit)
BIC(fit)

# Teste tipo reset
eta.hat2 <- fitted(fit)^2
reset_test <- glm(GHGC ~ UNNC + UCAC + DOCC + eta.hat2, family = Gamma(link = "log"), data=dados) # ajustando o modelo
summary(reset_test)

# Análise de Desvio
D = round(deviance(fit), 2) # desvio
n = nrow(dados) # tamanho amostral
gl = n - fit$rank # graus de liberdade
quantil <- round(qchisq(0.95, gl),2)

# Caso queira comparar dois modelos ou mais (ANODEV)
anova(null_fit, fit, test="Chisq")

# R2 de Nagelkerke
r2 <- round(modEvA::RsqGLM(fit)$Nagelkerke, 4)

# observado versus ajustados
pdf(file = "obsxfitted.pdf",width = width, height = height,family = "Times")
par(mar=c(mar_b, mar_e, mar_c, mar_d)) 
par(mgp=c(dist_text, dist_tick, 0))
plot(ggplot(data = dados, aes(x = GHGC, y = fitted(fit))) +
  geom_point(color = "black") +  # Add points to the plot
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "blue") +  # Add identity line
  labs(
    x = "Observado",
    y = "Ajustado"
  ) +
  ggplot2::theme_bw() +
  ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                 panel.grid.minor = ggplot2::element_blank()))
dev.off()

# Histograma dos Resíduos
pdf(file = "hist_resid.pdf",width = width, height = height,family = "Times")
par(mar=c(mar_b, mar_e, mar_c, mar_d)) 
par(mgp=c(dist_text, dist_tick, 0))
hist(td, xlab = "Resíduo", ylab = "Frequência", main = "")
dev.off()

# Resíduo e tamanho de amostra
n <- nrow(dados)

# Plot resíduo vs índice
plot_data_resxind <- data.frame(Indice = seq(n), Residuos = td)

pdf(file = "ResXind.pdf",width = width, height = height,family = "Times")
par(mar=c(mar_b, mar_e, mar_c, mar_d)) 
par(mgp=c(dist_text, dist_tick, 0))
plot(ggplot(plot_data_resxind, aes(x = Indice, y = Residuos)) +
       geom_point(shape = "+", size = 3) +
       geom_hline(yintercept = c(-3, 3), linetype = "dashed", color = "black") +
       geom_hline(yintercept = c(-2, 2), linetype = "dotted", color = "black") +
       ylim(c(min(c(-4, td)), max(c(4, td)))) +
       labs(x = "Índice", y = "Resíduos") +
       ggplot2::theme_bw() +
       ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                      panel.grid.minor = ggplot2::element_blank()))
dev.off()


# Plot Densidade dos resíduos

plot_data_dens <- data.frame(Residuos = td)

pdf(file = "densidade_res.pdf",width = width, height = height,family = "Times")
par(mar=c(mar_b, mar_e, mar_c, mar_d)) 
par(mgp=c(dist_text, dist_tick, 0))
plot(ggplot(plot_data_dens, aes(x = Residuos)) +
       geom_density(aes(y = ..density..), size = 1) +  # Increased size for density line
       stat_function(fun = dnorm, args = list(mean = mean(td), sd = sd(td)), linetype = "dashed", size = 1, color = "blue") +  # Increased size for normal line
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
plot(td |> 
       as.data.frame() |> 
       ggplot2::ggplot(ggplot2::aes(sample = td)) +
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

# Plot cov.ratio
covratio <- covratio(fit)
lim_inf <- 1 - 3*4/n
lim_sup <- 1 + 3*4/n
plot_data_alav <- data.frame(Index = 1:length(covratio), hatvalues = covratio)

pdf(file = "covratio.pdf",width = width, height = height,family = "Times")
par(mar=c(mar_b, mar_e, mar_c, mar_d)) 
par(mgp=c(dist_text, dist_tick, 0))
plot(ggplot(plot_data_alav, aes(x = Index, y = hatvalues)) +
       geom_point(size = 1.5, shape = 1) +  # Increase the size of the points
       geom_hline(yintercept = lim_inf, linetype = "dashed", color = "blue") +
       geom_hline(yintercept = lim_sup, linetype = "dashed", color = "blue") +
       labs(x = "Índice", y = "Cov Ratio") +
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


# Plot DFBETA_3
plot_data_dfbeta2 <- data.frame(Index = 1:length(dfbetas[,4]), dfbetas = dfbetas[,4])

pdf(file = "DFBETA3.pdf",width = width, height = height,family = "Times")
par(mar=c(mar_b, mar_e, mar_c, mar_d)) 
par(mgp=c(dist_text, dist_tick, 0))
plot(ggplot(plot_data_dfbeta2, aes(x = Index, y = dfbetas)) +
       geom_point(size = 1.5, shape = 1) +  # Increase the size of the points
       geom_hline(yintercept = limite_dfbeta, linetype = "dashed", color = "blue") +
       geom_hline(yintercept = -limite_dfbeta, linetype = "dashed", color = "blue") +
       labs(x = "Índice", y = "DFBETA 3") +
       ggplot2::theme_bw() +
       ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                      panel.grid.minor = ggplot2::element_blank()))
dev.off()
