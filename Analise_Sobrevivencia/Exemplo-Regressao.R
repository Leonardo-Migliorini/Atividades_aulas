# Carregando Pacotes -----------------------------------------------------------

library(survival)

# Importando dados -------------------------------------------------------------

dados <- data.frame(
  temp = c(65, 156, 100, 134, 16, 108, 121, 4, 39, 143, 56, 26, 22, 1, 1, 5, 65),
  cens = rep(1, 17),
  lwbc = c(3.36, 2.88, 3.63, 3.41, 3.78, 4.02, 4.00, 4.23, 3.73, 3.85, 3.97, 
           4.51, 4.54, 5.00, 5.00, 4.72, 5.00))

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

# Log-normal
plot(log(time), qnorm(st),
  pch = 16, xlab = "log(tempos)",
  ylab = expression(Phi^-1 * (S(t)))
)

# Melhores canditadas são as distribuições exponencial e Weibull.

# Ajustando a regressão para o modelo exponencial
(fite <- survreg(Surv(temp, cens) ~ lwbc, dist = "exponential"))

# Ajustando a regressão para o modelo weibull
(fitw <- survreg(Surv(temp, cens) ~ lwbc, dist = "weibull"))

# Testando se o modelo exponencial é preferível ao modelo weibull
gama <- 1 / fitw$scale
trv <- 2 * (fitw$loglik[2] - fite$loglik[2])
(pvalor <- 1 - pchisq(trv, 1))

# Pelo teste da razão de verossimilhanças, é preferível o modelo exponencial

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

# Plot dos resíduos vs função taxa de falha acumulada
# Deseja-se observer uma reta com inclinação 1
plot(st, sexp,
  xlab = "S(ei): Kaplan-Meier",
  ylab = "S(ei): Exponencial padrão",
  pch = 16
)
# Observeando o plot, temos uma reta bem próxima do comportamento desejado.

# Plot da curva de sobrevivência estimada (modelo exponencial) vs o estimador de 
# Kaplan Meier
plot(ekm1, conf.int = F, mark.time = F, xlab = "Resı́duos de
Cox-Snell", ylab = "Sobrevivência estimada")
lines(t, sexp, lty = 4)
legend(1.0, 0.8, lty = c(1, 4), c(
  "Kaplan-Meier",
  "Exponencial padrão"
), cex = 0.8, bty = "n")
# Note que a curva da exponencial se sobressai bem a curva do Kaplan Meier.

# Teste de Significância de Beta_1

trvbeta1 <- 2 * (fite$loglik[2] - fite$loglik[1])
(pvalorbeta1 <- 1 - pchisq(trvbeta1, 1))

# Como p-valo < 0.05, rejeita-se H0, portanto conclui-se que a contagem de 
# glóbulos brancos é uma variável importante para explicar a variabilidade do
# tempo de sobrevivência dos pacientes.

# Interpretações do modelo

# Como beta_1 é negativo, então quanto maior o valor de x_1, menor a probabilidade
# de sobrevivência estimada.
t <- seq(0, 156, 0.01)
sexp4 <- exp(-t / exp(fite$coefficients[1] + fite$coefficients[2] * 4))
sexp3 <- exp(-t / exp(fite$coefficients[1] + fite$coefficients[2] * 3))

# Curvas estimadas para pacientes com x_1 =3 e x_1 = 4:
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

