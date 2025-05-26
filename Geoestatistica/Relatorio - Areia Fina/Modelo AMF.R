# Loading packages -------------------------------------------------------------
library(geoR)

# Loading data -----------------------------------------------------------------

(geodados <- read.geodata("AMF.txt", h = T))

# Estatística Descritiva -------------------------------------------------------
summary(geodados)
(CV <- (sd(geodados$data) / mean(geodados$data)) * 100)

# Análise gráfica
plot(geodados, lowes = TRUE)
plot(geodados, trend = "1st", lowes = TRUE)
plot(geodados, trend = "2nd", lowes = TRUE)


# boxplot
boxplot(geodados$data, xlab = "Dados")

# boxplot nos residuos do 1st
reg1 <- lm(geodados$data ~ geodados$coords[, 1] + geodados$coords[, 2])
residuo1 <- reg1$res
boxplot(residuo1)

# boxplot nos residuos do 2nd
reg2 <- lm(geodados$data ~ geodados$coords[, 1] + geodados$coords[, 2] + I(geodados$coords[, 1]^2)
  + I(geodados$coords[, 2]^2) + I(geodados$coords[, 1] * geodados$coords[, 2]))
residuo2 <- reg2$res
boxplot(residuo2)

# Análise variográfica ---------------------------------------------------------

# Estimando o semivariograma (Semivariograma amostral)
hmax1 <- summary(geodados)[[3]][[2]] * 0.9
hmax1 # 90%

hmax2 <- summary(geodados)[[3]][[2]] * 0.8
hmax2 # 80%

vario1 <- variog(geodados, trend = "1st", estimator = "classical", max.dist = hmax1)
vario2 <- variog(geodados, trend = "1st", estimator = "classical", max.dist = hmax2)
vario1$n # já possui todos os lags com mais de 30 pares!
vario2$n

vario3 <- variog(geodados, trend = "1st", estimator = "modulus", max.dist = hmax1)
vario4 <- variog(geodados, trend = "1st", estimator = "modulus", max.dist = hmax2)
vario3$n # já possui todos os lags com mais de 30 pares!
vario4$n

par(mfrow = c(2, 2))
plot(vario1, xlab = "Distância (h)", ylab = "Semivariância", sub = "hmax 90% - clássico")
plot(vario2, xlab = "Distância (h)", ylab = "Semivariância", sub = "hmax 80% - clássico")
plot(vario3, xlab = "Distância (h)", ylab = "Semivariância", sub = "hmax 90% - robusto")
plot(vario4, xlab = "Distância (h)", ylab = "Semivariância", sub = "hmax 80% - robusto")

# Envelopes para verificar a existência de dependência espacial

par(mfrow = c(1, 1))
vario.env <- variog.mc.env(geodados, obj.var = vario2)
plot(vario2, envelope = vario.env)

# Avaliação da anisotropia:
var4 <- variog4(geodados, trend = "1st", max.dist = hmax2) # Gera semivariogramas em 4 direções principais
plot(var4)

# Ajuste de modelo esférico em cada direção:

vario <- variog(geodados, trend = "1st", estimator = "classical", max.dist = hmax2, direction = "omnidirectional") # geral
sph <- variofit(vario, cov.model = "sph", weights = "npairs")

vario1 <- variog(geodados, trend = "1st", estimator = "classical", max.dist = hmax2, direction = 0) # direção 0º
sph1 <- variofit(vario1, cov.model = "sph", weights = "npairs")

vario2 <- variog(geodados, trend = "1st", estimator = "classical", max.dist = hmax2, direction = pi / 4) # direção 45º
sph2 <- variofit(vario2, cov.model = "sph", weights = "npairs")

vario3 <- variog(geodados, trend = "1st", estimator = "classical", max.dist = hmax2, direction = pi / 2) # direção 90º
sph3 <- variofit(vario3, cov.model = "sph", weights = "npairs")

vario4 <- variog(geodados, trend = "1st", estimator = "classical", max.dist = hmax2, direction = 3 * pi / 4) # direção 135º
sph4 <- variofit(vario4, cov.model = "sph", weights = "npairs")

par(mfrow = c(1, 1))
plot(vario, xlab = "Distância (h)", ylab = "Semivariância")
lines(sph, lwd = 1, lty = 1, col = "gray")
lines(sph1, lwd = 1, lty = 1, col = "black")
lines(sph2, lwd = 1, lty = 1, col = "red")
lines(sph3, lwd = 1, lty = 1, col = "green")
lines(sph4, lwd = 1, lty = 1, col = "blue")

# plot de cada semivariograma direcional e do semivariograma geral:
par(mfrow = c(3, 2))

plot(vario1, xlab = "Distância (h)", ylab = "Semivariância", sub = "0º")
lines(sph1, lwd = 1, lty = 1, col = "red")

env1 <- variog.mc.env(geodados, obj.var = vario1)
lines(env1, lty = 1, col = "red")
plot(vario2, xlab = "Distância (h)", ylab = "Semivariância", sub = "45º")
lines(sph2, lwd = 1, lty = 1, col = "red")

env2 <- variog.mc.env(geodados, obj.var = vario2)
lines(env2, lty = 1, col = "red")
plot(vario3, xlab = "Distância (h)", ylab = "Semivariância", sub = "90º")
lines(sph3, lwd = 1, lty = 1, col = "red")

env3 <- variog.mc.env(geodados, obj.var = vario3)
lines(env3, lty = 1, col = "red")
plot(vario4, xlab = "Distância (h)", ylab = "Semivariância", sub = "135º")
lines(sph4, lwd = 1, lty = 1, col = "red")

env4 <- variog.mc.env(geodados, obj.var = vario4)
lines(env4, lty = 1, col = "red")
plot(vario, xlab = "Distância (h)", ylab = "Semivariância", sub = "omnidirecional")
lines(sph, lwd = 1, lty = 1, col = "red")

env <- variog.mc.env(geodados, obj.var = vario)
lines(env, lty = 1, col = "red")

# Avaliação dos parâmetros estimados (Alcance prático - Anisotropia geométrica):
# 0º
sph1[[8]]
# 45º
sph2[[8]]
# 90º
sph3[[8]]
# 135º
sph4[[8]]
# omnidirecional
sph[[8]]

# Identificação do maior alcance (a_max) e do menor alcance (a_min):
(a_max <- max(sph1[[8]], sph2[[8]], sph3[[8]], sph4[[8]]))
(a_min <- min(sph1[[8]], sph2[[8]], sph3[[8]], sph4[[8]]))

# Razão de anisotropia geometrica:
(Fa <- a_max / a_min)

# Avaliação dos parâmetros estimados (Patamar - Anisotropia zonal):
# 0º
sph1[[1]] + sph1[[2]][[1]]
# 45º
sph2[[1]] + sph2[[2]][[1]]
# 90º
sph3[[1]] + sph3[[2]][[1]]
# 135º
sph4[[1]] + sph4[[2]][[1]]
# omnidirecional
sph[[1]] + sph[[2]][[1]]

# Identificação do maior patamar (C_max) e do menor patamar (C_min):
(C_max <- max(
  sph1[[1]] + sph1[[2]][[1]], sph2[[1]] + sph2[[2]][[1]],
  sph3[[1]] + sph3[[2]][[1]], sph4[[1]] + sph4[[2]][[1]]
))
(C_min <- min(
  sph1[[1]] + sph1[[2]][[1]], sph2[[1]] + sph2[[2]][[1]],
  sph3[[1]] + sph3[[2]][[1]], sph4[[1]] + sph4[[2]][[1]]
))

# Razão de anisotropia zonal:
(FC <- C_max / C_min)

# Correção da anisotropia geométrica:
# Transformação das coordenadas de acrodo com os parâmetros de anisotropia:

coordsA <- coords.aniso(geodados$coords, aniso.pars = c(pi / 2, Fa))
# plot com as coordenadas originais (anisotropia) e o plot das
# 	coordenadas corrigidas (isotropia):
par(mfrow = c(1, 2))
plot(geodados$coords)
plot(coordsA)

# Novo geodados:
dados <- cbind(coordsA, geodados$data)
geodadosc <- as.geodata(dados)
geodadosc
plot(geodadosc, lowes = TRUE)
plot(geodadosc, trend = "1st", lowes = TRUE)
summary(geodadosc)

# Semivariogramas nas 4 direções principais após correção da anisotropia:
hmaxc <- summary(geodadosc)[[3]][[2]] * 0.8
var4c <- variog4(geodadosc, trend = "1st", estimator = "classical", max.dist = hmaxc)

# 4 direções com coordenadas originais e 4 direções com coordenadas corrigidas:
par(mfrow = c(1, 2))
plot(var4)
plot(var4c)

# Ajuste final (após correção da anisotropia - Semivariograma omnidirecional):
par(mfrow = c(1, 1))
varioc <- variog(geodadosc, trend = "1st", estimator = "classical", max.dist = hmaxc, direction = "omnidirectional")
sphc <- variofit(varioc, cov.model = "sph", weights = "npairs")
plot(varioc, xlab = "Distancia (h)", ylab = "Semivariancia", sub = "final")
lines(sphc, lwd = 1, lty = 1, col = "red")

# Ajuste 15 Modelos ------------------------------------------------------------

# Ajuste de modelos por Mínimos Quadrados Ordinários:
sph.ols <- variofit(varioc, cov.model = "sph", weights = "equal")

exp.ols <- variofit(varioc, cov.model = "exp", weights = "equal")

gaus.ols <- variofit(varioc, cov.model = "gaus", weights = "equal")

# Ajuste de modelos por Mínimos Quadrados Ponderados (Nº de pares por lag):
sph.wls1 <- variofit(varioc, cov.model = "sph", weights = "npairs")

exp.wls1 <- variofit(varioc, cov.model = "exp", weights = "npairs")

gaus.wls1 <- variofit(varioc, cov.model = "gaus", weights = "npairs")

# Ajuste de modelos por Mínimos Quadrados Ponderados (proposta de Cressie):
sph.wls2 <- variofit(varioc, cov.model = "sph", weights = "cressie")

exp.wls2 <- variofit(varioc, cov.model = "exp", weights = "cressie")

gaus.wls2 <- variofit(varioc, cov.model = "gaus", weights = "cressie")

# Ajuste de modelos por Máxima Verossimilhança:
sph.ml <- likfit(geodadosc, trend = "1st", ini.cov.pars = c(2, 130), nugget = 0, cov.model = "sph", lik.method = "ML")

exp.ml <- likfit(geodadosc, trend = "1st", ini.cov.pars = c(2, 130), nugget = 0, cov.model = "exp", lik.method = "ML")

gaus.ml <- likfit(geodadosc, trend = "1st", ini.cov.pars = c(2, 130), nugget = 0, cov.model = "gaus", lik.method = "ML")

# Ajuste de modelos por Máxima Verossimilhança Restrita:
sph.reml <- likfit(geodadosc, trend = "1st", ini.cov.pars = c(2, 130), nugget = 0, cov.model = "sph", lik.method = "REML")

exp.reml <- likfit(geodadosc, trend = "1st", ini.cov.pars = c(2, 130), nugget = 0, cov.model = "exp", lik.method = "REML")

gaus.reml <- likfit(geodadosc, trend = "1st", ini.cov.pars = c(2, 130), nugget = 0, cov.model = "gaus", lik.method = "REML")

# Avaliação dos ajustes dos modelos por meio de um grupo de critérios:

# sph.ml
(-2) * (sph.ml$loglik) + 2 * 6 # AIC    (6 parâmetros devido ao trend="1st": beta0, beta1, beta2, EP, CT, AL)
(-2) * (sph.ml$loglik) + 6 * (log(summary(geodados)[[1]])) # BIC
# exp.ml
(-2) * (exp.ml$loglik) + 2 * 6 # AIC    (6 parâmetros devido ao trend="1st": beta0, beta1, beta2, EP, CT, AL)
(-2) * (exp.ml$loglik) + 6 * (log(summary(geodados)[[1]])) # BIC
# gaus.ml
(-2) * (gaus.ml$loglik) + 2 * 6 # AIC    (6 parâmetros devido ao trend="1st": beta0, beta1, beta2, EP, CT, AL)
(-2) * (gaus.ml$loglik) + 6 * (log(summary(geodados)[[1]])) # BIC
# sph.reml
(-2) * (sph.reml$loglik) + 2 * 6 # AIC    (6 parâmetros devido ao trend="1st": beta0, beta1, beta2, EP, CT, AL)
(-2) * (sph.reml$loglik) + 6 * (log(summary(geodados)[[1]])) # BIC
# exp.reml
(-2) * (exp.reml$loglik) + 2 * 6 # AIC    (6 parâmetros devido ao trend="1st": beta0, beta1, beta2, EP, CT, AL)
(-2) * (exp.reml$loglik) + 6 * (log(summary(geodados)[[1]])) # BIC
# gaus.reml
(-2) * (gaus.reml$loglik) + 2 * 6 # AIC    (6 parâmetros devido ao trend="1st": beta0, beta1, beta2, EP, CT, AL)
(-2) * (gaus.reml$loglik) + 6 * (log(summary(geodados)[[1]])) # BIC


# Validação cruzada:
# Medidas de avaliação da qualidade de ajuste (EM, ER, DPem, Ser, EA) discutidas em Faraco et al. (2008):
# sph.ols
vc.sph.ols <- xvalid(geodadosc, model = sph.ols)
mean(vc.sph.ols$error) # media do erro (EM)
mean(vc.sph.ols$std.error) # erro medio reduzido (ER)
sqrt(mean(abs(vc.sph.ols$error))) # desvio padrão dos erros (DPem)
sqrt(mean(abs(vc.sph.ols$std.error))) # Desvio Padrao dos erros reduzidos (Ser)
sum(abs(vc.sph.ols$data - vc.sph.ols$predicted)) # Erro absoluto (EA)
reg.sph.ols <- lm(vc.sph.ols$data ~ vc.sph.ols$predicted)
summary(reg.sph.ols)[[4]][[1]] # estimativa de beta0
summary(reg.sph.ols)[[4]][[2]] # estimativa de beta1

# exp.ols
vc.exp.ols <- xvalid(geodadosc, model = exp.ols)
mean(vc.exp.ols$error) # media do erro (EM)
mean(vc.exp.ols$std.error) # erro medio reduzido (ER)
sqrt(mean(abs(vc.exp.ols$error))) # desvio padrão dos erros (DPem)
sqrt(mean(abs(vc.exp.ols$std.error))) # Desvio Padrao dos erros reduzidos (Ser)
sum(abs(vc.exp.ols$data - vc.exp.ols$predicted)) # Erro absoluto (EA)
reg.exp.ols <- lm(vc.exp.ols$data ~ vc.exp.ols$predicted)
summary(reg.exp.ols)[[4]][[1]] # estimativa de beta0
summary(reg.exp.ols)[[4]][[2]] # estimativa de beta1

# gaus.ols
vc.gaus.ols <- xvalid(geodadosc, model = gaus.ols)
mean(vc.gaus.ols$error) # media do erro (EM)
mean(vc.gaus.ols$std.error) # erro medio reduzido (ER)
sqrt(mean(abs(vc.gaus.ols$error))) # desvio padrão dos erros (DPem)
sqrt(mean(abs(vc.gaus.ols$std.error))) # Desvio Padrao dos erros reduzidos (Ser)
sum(abs(vc.gaus.ols$data - vc.gaus.ols$predicted)) # Erro absoluto (EA)
reg.gaus.ols <- lm(vc.gaus.ols$data ~ vc.sph.ols$predicted)
summary(reg.gaus.ols)[[4]][[1]] # estimativa de beta0
summary(reg.gaus.ols)[[4]][[2]] # estimativa de beta1

# sph.wls1
vc.sph.wls1 <- xvalid(geodadosc, model = sph.wls1)
mean(vc.sph.wls1$error) # media do erro (EM)
mean(vc.sph.wls1$std.error) # erro medio reduzido (ER)
sqrt(mean(abs(vc.sph.wls1$error))) # desvio padrão dos erros (DPem)
sqrt(mean(abs(vc.sph.wls1$std.error))) # Desvio Padrao dos erros reduzidos (Ser)
sum(abs(vc.sph.wls1$data - vc.sph.wls1$predicted)) # Erro absoluto (EA)
reg.sph.wls1 <- lm(vc.sph.wls1$data ~ vc.sph.wls1$predicted)
summary(reg.sph.wls1)[[4]][[1]] # estimativa de beta0
summary(reg.sph.wls1)[[4]][[2]] # estimativa de beta1

# exp.wls1
vc.exp.wls1 <- xvalid(geodadosc, model = exp.wls1)
mean(vc.exp.wls1$error) # media do erro (EM)
mean(vc.exp.wls1$std.error) # erro medio reduzido (ER)
sqrt(mean(abs(vc.exp.wls1$error))) # desvio padrão dos erros (DPem)
sqrt(mean(abs(vc.exp.wls1$std.error))) # Desvio Padrao dos erros reduzidos (Ser)
sum(abs(vc.exp.wls1$data - vc.exp.wls1$predicted)) # Erro absoluto (EA)
reg.exp.wls1 <- lm(vc.exp.wls1$data ~ vc.exp.wls1$predicted)
summary(reg.exp.wls1)[[4]][[1]] # estimativa de beta0
summary(reg.exp.wls1)[[4]][[2]] # estimativa de beta1

# gaus.wls1
vc.gaus.wls1 <- xvalid(geodadosc, model = gaus.wls1)
mean(vc.gaus.wls1$error) # media do erro (EM)
mean(vc.gaus.wls1$std.error) # erro medio reduzido (ER)
sqrt(mean(abs(vc.gaus.wls1$error))) # desvio padrão dos erros (DPem)
sqrt(mean(abs(vc.gaus.wls1$std.error))) # Desvio Padrao dos erros reduzidos (Ser)
sum(abs(vc.gaus.wls1$data - vc.gaus.wls1$predicted)) # Erro absoluto (EA)
reg.gaus.wls1 <- lm(vc.gaus.wls1$data ~ vc.sph.wls1$predicted)
summary(reg.gaus.wls1)[[4]][[1]] # estimativa de beta0
summary(reg.gaus.wls1)[[4]][[2]] # estimativa de beta1

# sph.wls2
vc.sph.wls2 <- xvalid(geodadosc, model = sph.wls2)
mean(vc.sph.wls2$error) # media do erro (EM)
mean(vc.sph.wls2$std.error) # erro medio reduzido (ER)
sqrt(mean(abs(vc.sph.wls2$error))) # desvio padrão dos erros (DPem)
sqrt(mean(abs(vc.sph.wls2$std.error))) # Desvio Padrao dos erros reduzidos (Ser)
sum(abs(vc.sph.wls2$data - vc.sph.wls2$predicted)) # Erro absoluto (EA)
reg.sph.wls2 <- lm(vc.sph.wls2$data ~ vc.sph.wls2$predicted)
summary(reg.sph.wls2)[[4]][[1]] # estimativa de beta0
summary(reg.sph.wls2)[[4]][[2]] # estimativa de beta1

# exp.wls2
vc.exp.wls2 <- xvalid(geodadosc, model = exp.wls2)
mean(vc.exp.wls2$error) # media do erro (EM)
mean(vc.exp.wls2$std.error) # erro medio reduzido (ER)
sqrt(mean(abs(vc.exp.wls2$error))) # desvio padrão dos erros (DPem)
sqrt(mean(abs(vc.exp.wls2$std.error))) # Desvio Padrao dos erros reduzidos (Ser)
sum(abs(vc.exp.wls2$data - vc.exp.wls2$predicted)) # Erro absoluto (EA)
reg.exp.wls2 <- lm(vc.exp.wls2$data ~ vc.exp.wls2$predicted)
summary(reg.exp.wls2)[[4]][[1]] # estimativa de beta0
summary(reg.exp.wls2)[[4]][[2]] # estimativa de beta1

# gaus.wls2
vc.gaus.wls2 <- xvalid(geodadosc, model = gaus.wls2)
mean(vc.gaus.wls2$error) # media do erro (EM)
mean(vc.gaus.wls2$std.error) # erro medio reduzido (ER)
sqrt(mean(abs(vc.gaus.wls2$error))) # desvio padrão dos erros (DPem)
sqrt(mean(abs(vc.gaus.wls2$std.error))) # Desvio Padrao dos erros reduzidos (Ser)
sum(abs(vc.gaus.wls2$data - vc.gaus.wls2$predicted)) # Erro absoluto (EA)
reg.gaus.wls2 <- lm(vc.gaus.wls2$data ~ vc.sph.wls2$predicted)
summary(reg.gaus.wls2)[[4]][[1]] # estimativa de beta0
summary(reg.gaus.wls2)[[4]][[2]] # estimativa de beta1

# sph.ml
vc.sph.ml <- xvalid(geodadosc, model = sph.ml)
mean(vc.sph.ml$error) # media do erro (EM)
mean(vc.sph.ml$std.error) # erro medio reduzido (ER)
sqrt(mean(abs(vc.sph.ml$error))) # desvio padrão dos erros (DPem)
sqrt(mean(abs(vc.sph.ml$std.error))) # Desvio Padrao dos erros reduzidos (Ser)
sum(abs(vc.sph.ml$data - vc.sph.ml$predicted)) # Erro absoluto (EA)
reg.sph.ml <- lm(vc.sph.ml$data ~ vc.sph.ml$predicted)
summary(reg.sph.ml)[[4]][[1]] # estimativa de beta0
summary(reg.sph.ml)[[4]][[2]] # estimativa de beta1

# exp.ml
vc.exp.ml <- xvalid(geodadosc, model = exp.ml)
mean(vc.exp.ml$error) # media do erro (EM)
mean(vc.exp.ml$std.error) # erro medio reduzido (ER)
sqrt(mean(abs(vc.exp.ml$error))) # desvio padrão dos erros (DPem)
sqrt(mean(abs(vc.exp.ml$std.error))) # Desvio Padrao dos erros reduzidos (Ser)
sum(abs(vc.exp.ml$data - vc.exp.ml$predicted)) # Erro absoluto (EA)
reg.exp.ml <- lm(vc.exp.ml$data ~ vc.exp.ml$predicted)
summary(reg.exp.ml)[[4]][[1]] # estimativa de beta0
summary(reg.exp.ml)[[4]][[2]] # estimativa de beta1

# gaus.ml
vc.gaus.ml <- xvalid(geodadosc, model = gaus.ml)
mean(vc.gaus.ml$error) # media do erro (EM)
mean(vc.gaus.ml$std.error) # erro medio reduzido (ER)
sqrt(mean(abs(vc.gaus.ml$error))) # desvio padrão dos erros (DPem)
sqrt(mean(abs(vc.gaus.ml$std.error))) # Desvio Padrao dos erros reduzidos (Ser)
sum(abs(vc.gaus.ml$data - vc.gaus.ml$predicted)) # Erro absoluto (EA)
reg.gaus.ml <- lm(vc.gaus.ml$data ~ vc.sph.ml$predicted)
summary(reg.gaus.ml)[[4]][[1]] # estimativa de beta0
summary(reg.gaus.ml)[[4]][[2]] # estimativa de beta1

# sph.reml
vc.sph.reml <- xvalid(geodadosc, model = sph.reml)
mean(vc.sph.reml$error) # media do erro (EM)
mean(vc.sph.reml$std.error) # erro medio reduzido (ER)
sqrt(mean(abs(vc.sph.reml$error))) # desvio padrão dos erros (DPem)
sqrt(mean(abs(vc.sph.reml$std.error))) # Desvio Padrao dos erros reduzidos (Ser)
sum(abs(vc.sph.reml$data - vc.sph.reml$predicted)) # Erro absoluto (EA)
reg.sph.reml <- lm(vc.sph.reml$data ~ vc.sph.reml$predicted)
summary(reg.sph.reml)[[4]][[1]] # estimativa de beta0
summary(reg.sph.reml)[[4]][[2]] # estimativa de beta1

# exp.reml
vc.exp.reml <- xvalid(geodadosc, model = exp.reml)
mean(vc.exp.reml$error) # media do erro (EM)
mean(vc.exp.reml$std.error) # erro medio reduzido (ER)
sqrt(mean(abs(vc.exp.reml$error))) # desvio padrão dos erros (DPem)
sqrt(mean(abs(vc.exp.reml$std.error))) # Desvio Padrao dos erros reduzidos (Ser)
sum(abs(vc.exp.reml$data - vc.exp.reml$predicted)) # Erro absoluto (EA)
reg.exp.reml <- lm(vc.exp.reml$data ~ vc.exp.reml$predicted)
summary(reg.exp.reml)[[4]][[1]] # estimativa de beta0
summary(reg.exp.reml)[[4]][[2]] # estimativa de beta1

# gaus.reml
vc.gaus.reml <- xvalid(geodadosc, model = gaus.reml)
mean(vc.gaus.reml$error) # media do erro (EM)
mean(vc.gaus.reml$std.error) # erro medio reduzido (ER)
sqrt(mean(abs(vc.gaus.reml$error))) # desvio padrão dos erros (DPem)
sqrt(mean(abs(vc.gaus.reml$std.error))) # Desvio Padrao dos erros reduzidos (Ser)
sum(abs(vc.gaus.reml$data - vc.gaus.reml$predicted)) # Erro absoluto (EA)
reg.gaus.reml <- lm(vc.gaus.reml$data ~ vc.sph.reml$predicted)
summary(reg.gaus.reml)[[4]][[1]] # estimativa de beta0
summary(reg.gaus.reml)[[4]][[2]] # estimativa de beta1

# Índice DE proposto por Biondi et al. (1994):
# Estimativas dos parâmetros do semivariograma:
C0 <- # efeito pepita
  C1 <- # contribuição
  #
  I <- if (C1 == 0) {
    I <- 0
  } else {
    I <- C1 / (C0 + C1)
  }

# DE
DE <- I * 100
DE
# Classificação do DE (adaptada de Cambardella et al. (1994))
if (DE < 25) {
  cat("Fraca dependência espacial")
} else if (DE >= 25 && DE < 75) {
  cat("Moderada dependência espacial")
} else {
  cat("Forte dependência espacial")
}
# Índice IDE proposto por Seidel e Oliveira (2014, 2016):
# Estimativas dos parâmetros do semivariograma:
C0 <- # efeito pepita
  C1 <- # contribuição
  a <- # alcance prático
  MD <- # máxima distância
  I <- if (C1 == 0) {
    I <- 0
  } else {
    I <- C1 / (C0 + C1)
  }
II <- if (a >= (0.5 * MD)) {
  II <- 1
} else {
  II <- a / (0.5 * MD)
}
# IDE no modelo Esférico
IDE_esf <- 0.375 * I * II * 100
IDE_esf
# Classificação do IDE_esf
if (IDE_esf < 7) {
  cat("Fraca dependência espacial")
} else if (IDE_esf >= 7 && IDE_esf < 15) {
  cat("Moderada dependência espacial")
} else {
  cat("Forte dependência espacial")
}
# IDE no modelo Exponencial
IDE_exp <- 0.317 * I * II * 100
IDE_exp
# Classificação do IDE_exp
if (IDE_exp < 6) {
  cat("Fraca dependência espacial")
} else if (IDE_exp >= 6 && IDE_exp < 13) {
  cat("Moderada dependência espacial")
} else {
  cat("Forte dependência espacial")
}
# IDE no modelo Gaussiano
IDE_gaus <- 0.504 * I * II * 100
IDE_gaus
# Classificação do IDE_gaus
if (IDE_gaus < 9) {
  cat("Fraca dependência espacial")
} else if (IDE_gaus >= 9 && IDE_gaus < 20) {
  cat("Moderada dependência espacial")
} else {
  cat("Forte dependência espacial")
}
