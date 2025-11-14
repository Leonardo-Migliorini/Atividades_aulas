# carregando pacotes -----------------------------------------------------------
library(survival)
library(flexsurv)
library(ggfortify)
library(survminer)

# Importando dados da atividade 3 ----------------------------------------------

dados_atv3 <- data.frame(
  temp = c(
    1, 2, 2, 2, 2, 6, 8, 8, 9, 9, 13, 13, 16, 17, 22, 29, 34, 36, 43, 45,
    1, 2, 5, 7, 7, 11, 12, 19, 22, 30, 35, 39, 42, 46, 55
  ),
  cens = c(
    1, 1, 1, 1, 0, 1, 1, 1, 1, 0, 1, 0, 1, 1, 0, 1, 1, 1, 0, 0,
    1, 1, 1, 1, 0, 0, 1, 1, 1, 1, 0, 1, 1, 1, 1
  ),
  tratA = c(rep(1, 20), rep(0, 15)),
  tratB = c(rep(0, 20), rep(1, 15))
)

# Estimando o Kaplan Meyer
ekm <- survfit(Surv(dados_atv3$temp, dados_atv3$cens) ~ 1)
summary(ekm)
st <- ekm$surv
time <- ekm$time

# Aplicando a linearização para testar os modelos paramétricos
par(mfrow = c(1, 2))

# Exponencial
plot(time, -log(st),
  pch = 16, xlab = "Tempos",
  ylab = "-log(S(t))"
)

# Weibull
plot(log(time), log(-log(st)),
  pch = 16, xlab = "log(tempos)",
  ylab = "log(-log(S(t))"
)

# Ajustando a regressão para o modelo exponencial
(fite <- survreg(Surv(dados_atv3$temp, dados_atv3$cens) ~ dados_atv3$tratB, dist = "exponential"))

# Ajustando a regressão para o modelo weibull
(fitw <- survreg(Surv(dados_atv3$temp, dados_atv3$cens) ~ dados_atv3$tratB, dist = "weibull"))

# Testando se o modelo exponencial é preferível ao modelo weibull
trv <- 2 * (fitw$loglik[2] - fite$loglik[2])
(pvalor <- 1 - pchisq(trv, 1))

# Importando dados da atividade 4 ----------------------------------------------

dados_atv4 <- data.frame(
  temp = c(
    1, 1, 1, 2, 2, 2, 2, 3, 4, 9, 9, 9, 10, 10, 10, 15, 27, 27, 27, 27,
    1, 2, 7, 9, 9, 11, 12, 25, 30, 30, 35, 39, 42, 46, 55
  ),
  cens = c(
    1, 1, 1, 1, 0, 1, 1, 1, 1, 0, 1, 0, 1, 1, 0, 1, 1, 1, 0, 0,
    1, 1, 1, 1, 0, 0, 1, 1, 1, 1, 0, 1, 1, 1, 1
  ),
  tratA = c(rep(1, 20), rep(0, 15)),
  tratB = c(rep(0, 20), rep(1, 15))
)

# Covariáveis: Estágio
fit <- coxph(Surv(dados_atv4$temp, dados_atv4$cens) ~ dados_atv4$tratB,
  data = dados_atv4, x = T, method = "breslow"
)

summary(fit)
