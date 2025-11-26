# Importando pacotes -----------------------------------------------------------

library(forecast)

# Importando dados -------------------------------------------------------------

# Pegamos uma janela de uma semana dos dados para conseguirmos fazer a modelagem
# A semana escolhida, foi a com os dados mais recentes[

# Link para obter os dados:
# https://www.kaggle.com/datasets/podsyp/production-quality/data


# Informações sobre a qualidade
dados <- readr::read_delim("data_Y.csv")[29017:29183,]

# Variáveis referentes ao processo de torra (não utilizadas na modelagem)
dados2 <- readr::read_delim("data_X.csv")[29017:29183,]

# Modelando os dados -----------------------------------------------------------

summary(dados$quality)
sd(dados$quality)

# autocorrelação
pdf("plot_acf.pdf", width = 7, height = 5)
acf(dados$quality, main = "Função de Autocorrelação")
dev.off()

# autocorrelação parcial

pdf("plot_pacf.pdf", width = 7, height = 5)
pacf(dados$quality, main = "Função de Autocorrelação Parcial")
dev.off()

# plot dos dados
pdf("plot_serie.pdf", width = 7, height = 5)
ts.plot(dados$quality, ylab = "Quality")
dev.off()
# testes de hipóteses:

seasonality_test(dados$quality)
# Resultados indicam ausência de sazonalidade

trend_test(dados$quality)
# Resultados indicam ausência de tendência 

unitroot_test(dados$quality)
# Resultados indicam ausência de raiz unitária

# CEP tradicional
pdf("cep_tradicional.pdf", width = 7, height = 5)
individuais <- qcc(
  data = dados$quality, type = "xbar.one", std.dev = "MR", k = 2, nsigmas = 3,
  title = "Gráfico para valores individuais",
  xlab = "Amostras", ylab = "Observações"
)
dev.off()

# Ajustando o modelo ARIMA:
(modelo <- arima(dados$quality, order = c(2, 0, 1)))

pdf("raizes.pdf", width = 7, height = 5)
plot(modelo)
dev.off()

fcast <- forecast(modelo, h = 24)
pdf("previsao.pdf", width = 7, height = 5)
plot(fcast)
dev.off()

autoplot(fcast)


# Verificação da independência

pdf("plot_residuo.pdf", width = 7, height = 5)
plot(modelo$residuals, ylab = "resíduos")
dev.off()

pdf("acf_residuo.pdf", width = 7, height = 5)
acf(modelo$residuals, lag.max = 36)
dev.off()

pdf("pacf_residuo.pdf", width = 7, height = 5)
pacf(modelo$residuals, lag.max = 36)
dev.off()

tsdiag(modelo)

# Verificação da normalidade
pdf("hist_residuo.pdf", width = 7, height = 5)
hist(modelo$residuals)
dev.off()

shapiro.test(modelo$residuals)
# p-valor maior que 5% indica que os resíduos são normais

# Aplicando CEP nos resíduos
pdf("controle_residuo.pdf", width = 7, height = 5)
residuos <- qcc(
  data = modelo$residuals, type = "xbar.one", std.dev = "MR",
  k = 2, nsigmas = 3,
  title = "Gráfico para os resíduos",
  xlab = "Amostras", ylab = "Observações"
)
dev.off()

# Removendo causas especiais:

refit <- dados$quality[-c(5,81,160)]

(modelo2 <- arima(refit, order = c(2, 0, 1)))

# Cálculando novos limites de controle
residuos2 <- qcc(
  data = modelo2$residuals, type = "xbar.one", std.dev = "MR",
  k = 2, nsigmas = 3,
  title = "Gráfico para os resíduos",
  xlab = "Amostras", ylab = "Observações"
)

# Aplicando CEP nos resíduos com novos limites de controle
pdf("controle_residuo_recalculado.pdf", width = 7, height = 5)
residuos <- qcc(
  data = modelo$residuals, type = "xbar.one",
  k = 2, nsigmas = 3,
  center = residuos2$center,
  limits = residuos2$limits,
  title = "Gráfico para os resíduos",
  xlab = "Amostras", ylab = "Observações"
)
dev.off()
