# Carregando Pacotes -----------------------------------------------------------

library(survival)

# Importando dados -------------------------------------------------------------
temp <- c(65, 156, 100, 134, 16, 108, 121, 4, 39, 143, 56, 26, 22, 1, 1, 5, 65)
cens <- rep(1, 17)
lwbc <- c(
  3.36, 2.88, 3.63, 3.41, 3.78, 4.02, 4.00, 4.23, 3.73, 3.85, 3.97, 4.51, 4.54,
  5.00, 5.00, 4.72, 5.00
)
dados <- cbind(temp, cens, lwbc)
dados <- as.data.frame(dados)
i <- order(dados$temp)
dados <- dados[i, ]
attach(dados)

# Estimando o Kaplan Meyer
ekm <- survfit(Surv(temp, cens) ~ 1)
summary(ekm)
st <- ekm$surv
time <- ekm$time

# Aplicando a linearização para testar os modelos paramétricos
par(mfrow = c(1, 3))

invst <- qnorm(st)
plot(time, -log(st),
  pch = 16, xlab = "Tempos",
  ylab = "-log(S(t))"
)
plot(log(time), log(-log(st)),
  pch = 16, xlab = "log(tempos)",
  ylab = "log(-log(S(t))"
)
plot(log(time), invst,
  pch = 16, xlab = "log(tempos)",
  ylab = expression(Phi^-1 * (S(t)))
)

# Ajustando a regressão para o modelo exponencial
(fite <- survreg(Surv(temp, cens) ~ lwbc, dist = "exponential"))

# Ajustando a regressão para o modelo weibull
(fitw <- survreg(Surv(temp, cens) ~ lwbc, dist = "weibull"))

# Testando se o modelo exponencial é preferível ao modelo weibull
gama <- 1 / fitw$scale
trv <- 2 * (fitw$loglik[2] - fite$loglik[2])
(pvalor <- 1 - pchisq(trv, 1))

# Análise de Resíduos

# Vetor de estimativas

Xbeta <- fite$coefficients[1] + fite$coefficients[2] * lwbc

# Resı́duos de Cox-Snell
ei <- temp * exp(-Xbeta)
ekm1 <- survfit(Surv(ei, cens) ~ 1)
t <- ekm1$time
st <- ekm1$surv
sexp <- exp(-t)
par(mfrow = c(1, 2))

plot(st, sexp,
  xlab = "S(ei): Kaplan-Meier",
  ylab = "S(ei): Exponencial padrão",
  pch = 16
)
plot(ekm1, conf.int = F, mark.time = F, xlab = "Resı́duos de
Cox-Snell", ylab = "Sobrevivência estimada")
lines(t, sexp, lty = 4)
legend(1.0, 0.8, lty = c(1, 4), c(
  "Kaplan-Meier",
  "Exponencial padrão"
), cex = 0.8, bty = "n")

# Teste de Significância de Beta_1

trvbeta1 <- 2 * (fite$loglik[2] - fite$loglik[1])
(pvalorbeta1 <- 1 - pchisq(trvbeta1, 1))

# Interpretações do modelo
t <- seq(0, 156, 0.01)
sexp4 <- exp(-t / exp(fite$coefficients[1] + fite$coefficients[2] * 4))
sexp3 <- exp(-t / exp(fite$coefficients[1] + fite$coefficients[2] * 3))
plot(t, sexp4,
  lty = 4, col = 1, lwd = 1, xlab = "tempos",
  ylab = "Sobreviv^
 encia estimada", type = "l"
)
lines(t, sexp3, lty = 4, col = 2)
legend(100, 1.0,
  lty = c(4, 4), col = c(1, 2), c("lwbc=4", "lwbc=3"),
  cex = 0.8, bty = "n"
)

