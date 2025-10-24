# carregando pacotes -----------------------------------------------------------
library(survival)
library(flexsurv)

# Exemplo Pacientes com Câncer de Bexiga

tempo <- c(3, 5, 6, 7, 8, 9, 10, 10, 12, 15, 15, 18, 19, 20, 22, 25, 28, 30, 40, 45)
cens <- c(rep(1, 7), 0, 1, 1, 0, rep(1, 8), 0)

# ajuste exponencial
(fit1 <- survreg(Surv(tempo, cens) ~ 1, dist = "exponen"))

(alphaehat <- exp(fit1$icoef[1]))

# ajuste Weibull - ajusta valor extremo
(fit2 <- survreg(Surv(tempo, cens) ~ 1, dist = "weibull"))

(alphawhat <- exp(fit2$icoef[1]))

(gamahat <- 1 / fit2$scale)

# ajuste Lognormal
(fit3 <- survreg(Surv(tempo, cens) ~ 1, dist = "lognorm"))

(muhat <- fit3$icoef[1])

(sigmahat <- fit3$scale)

# Sobrevivências estimadas usando Kaplan Meier e os modelos paramétricos -------
(ekm <- survfit(Surv(tempo, cens) ~ 1))
(time <- ekm$time)
# S(t)hat Kaplan-Meier
(st <- ekm$surv)

# S(t)hat Exponencial
(s_exp <- exp(-time / alphaehat))

# S(t)hat Weibull
(s_weib <- exp(-(time / alphawhat)^(gamahat)))

# S(t)hat Lognormal
(s_lnorm <- pnorm((-log(time) + muhat) / sigmahat))

# Matriz com as estimativas de todos os modelos considerados
sobrevivenciahat <- round(cbind(time, st, s_exp, s_weib, s_lnorm), 3)
sobrevivenciahat

# Plotando gráficos das curvas de sobrevicência de cada modelo -----------------

par(mfrow = c(1, 3))

# modelo exponencial
plot(st, s_exp,
  pch = 16, ylim = range(c(0, 1)), xlim = range(c(0, 1)),
  xlab = "S(t): Kaplan-Meier", ylab = "S(t): Exponencial"
)
lines(c(0, 1), c(0, 1), type = "l", lty = 1)

# modelo Weibull
plot(st, s_weib,
  pch = 16, ylim = range(c(0, 1)), lim = range(c(0, 1)),
  xlab = "S(t): Kaplan-Meier", ylab = "S(t): Weibull"
)
lines(c(0, 1), c(0, 1), type = "l", lty = 1)

# modelo Lognormal
plot(st, s_lnorm,
  pch = 16, ylim = range(c(0, 1)), xlim = range(c(0, 1)),
  xlab = "S(t): Kaplan-Meier", ylab = "S(t): log-normal"
)
lines(c(0, 1), c(0, 1), type = "l", lty = 1)

# Gráficos do Método 2: linearização da função de sobrevivência.

# modelo exponencial
plot(time, -log(st), pch = 16, xlab = "tempos", ylab = "-log(S(t))")

# modelo Weibull
plot(log(time), log(-log(st)),
  pch = 16, xlab = "log(tempos)",
  ylab = "log(-log(S(t)))"
)

# modelo Lognormal
invst <- qnorm(st)
plot(log(time), invst,
  pch = 16, xlab = "log(tempos)",
  ylab = expression(Phi^1 * S(t))
)

# Teste de adequação do modelo -------------------------------------------------

# Ajuste distribuição gamma generalizada

fitG <- flexsurvreg(Surv(tempo, cens) ~ 1, dist = "gengamma")
fitG

# Razão de verossimilhanças
2 * (fitG$loglik - fit1$loglik[2])
2 * (fitG$loglik - fit2$loglik[2])
2 * (fitG$loglik - fit3$loglik[2])

# pvalores do teste
(p.valor1 <- 1 - pchisq(2 * (fitG$loglik - fit1$loglik[2]), 2))
(p.valor2 <- 1 - pchisq(2 * (fitG$loglik - fit2$loglik[2]), 1))
(p.valor3 <- 1 - pchisq(2 * (fitG$loglik - fit3$loglik[2]), 1))

# Pelos resultados dos testes da razão de verossimilhanças para o ajuste pela
# exponencial a hipótese nula é rejeitada, considerando significância de 0.10,
# logo o modelo não é adequado para este conjunto de dados. Já para a
# distribuição Weibull e também log-normal a hipótese nula não é rejeitada,
# logo os ajustes Weibull e log-normal são adequados. Confirmando a análise
# gráfica realizada.

# Gráficos para interpretação das estimativas  ---------------------------------

# Kaplan-Meier + S(t) estimada - Exponecial
plot(ekm, conf.int = F, xlab = "tempos", ylab = "S(t)")
lines(c(0, time), c(1, s_exp), lty = 2)
legend("topright",
  lty = c(1, 2), c("Kaplan-Meier", "Exponencial"),
  lwd = 1, bty = "n"
)

# Kaplan-Meier + S(t) estimada - Weibull
plot(ekm, conf.int = F, xlab = "tempos", ylab = "S(t)")
lines(c(0, time), c(1, s_weib), lty = 2)
legend("topright",
  lty = c(1, 2), c("Kaplan-Meier", "Weibull"),
  lwd = 1, bty = "n"
)

# Kaplan-Meier + S(t) estimada - Lognormal
plot(ekm, conf.int = F, xlab = "tempos", ylab = "S(t)")
lines(c(0, time), c(1, s_lnorm), lty = 2)
legend("topright",
  lty = c(1, 2), c("Kaplan-Meier", "log-normal"),
  lwd = 1, bty = "n"
)

# Estimação de quantidades de interesse ----------------------------------------

## Esperança ou tempo medio

# Tempo médio em meses até a reincidência do câncer:
(e_weibul <- alphawhat * (gamma(1 + 1 / gamahat)))
(e_lnorm <- exp(muhat + (sigmahat^2 / 2)))

## Tempo mediano

# Tempo mediano em meses até a reincidência do câncer:
(m_weibull <- alphawhat * (-log(1 - 0.5))^(1 / gamahat))
(m_lnorm <- exp(muhat + (sigmahat * qnorm(0.5))))

# Baseada na Tabela das sobrevivências estimadas podemos ainda observar que
# 1) A probabilidade do câncer não ter reincidido em um ano pelo ajuste Weibull
# é 0.663 e log-normal é 0.619.
# 2) A probabilidade do câncer não ter reincidido em 2 anos pelo ajuste Weibull
# é 0.302 e log-normal é 0.273.

# sobrevivência 1 ano
(s_w1 <- exp(-(12 / alphawhat)^(gamahat)))

(s_lnorm1 <- pnorm((-log(12) + muhat) / sigmahat))

# sobrevivência 2 anos:
(s_w2 <- exp(-(24 / alphawhat)^(gamahat)))

(s_lnorm2 <- pnorm((-log(24) + muhat) / sigmahat))

### ------------------------------------------------------------------------ ###
