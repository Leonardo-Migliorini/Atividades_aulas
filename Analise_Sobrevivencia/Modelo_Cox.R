# Carregando pacotes -----------------------------------------------------------
library(survival)
library(ggfortify)
library(survminer)

# Descrição do estudo ----------------------------------------------------------

# Um estudo, descrito em Klein e Moeschberger (1997), foi realizado com 90
# pacientes do sexo masculino diagnosticados no período de 1970-1978 com câncer
# de laringe. Eles foram acompanhados até o óbito ou a data de 01/01/1983.
# Variável resposta: tempo até o óbito em meses.
# Para cada paciente foram registrados, no diagnóstico, a idade (em anos) e o
# estágio da doença (I = tumor primário, II = envolvimento de nódulos,
# III = metástases e IV = combinações dos 3 estágios anteriores).
# Os estágios encontram-se ordenados pelo grau de seriedade da doença (menos
# sério para mais sério).

# Importando banco de dados ----------------------------------------------------

laringe <- read.table("https://docs.ufpr.br/~giolo/Livro/ApendiceA/laringe.txt", h = T)

X <- data.frame(laringe$estagio) |>
  dplyr::mutate(
    estagio = factor(laringe.estagio)
  )
X <- model.matrix(~estagio, data = X)

estagioII <- X[, 2]
estagioIII <- X[, 3]
estagioIV <- X[, 4]

# Modelagem dos dados ----------------------------------------------------------

# Estimando o Kaplan Meyer
ekm <- survfit(Surv(tempos, cens) ~ estagio, data = laringe)

# Plot das curvas de sobrevivência por Kaplan Meyer segregada por estágio
autoplot(ekm, conf.int = FALSE) + xlab("Tempo em meses") +
  ylab("Sobrevivência")

# Ajuste do Modelo de Cox

# Modelo Nulo
fit1 <- coxph(Surv(tempos, cens) ~ 1, data = laringe, x = T, method = "breslow")

# Covariáveis: Estágio
fit2 <- coxph(Surv(tempos, cens) ~ factor(estagio),
  data = laringe, x = T,
  method = "breslow"
)

# Covariáveis: Estágio e Idade
fit3 <- coxph(Surv(tempos, cens) ~ estagioII + estagioIII + estagioIV + idade,
  data = laringe,
  x = T, method = "breslow"
)

# Covariáveis: Estágio e Idade e interação (Estágio * Idade)
x5 <- estagioII * laringe$idade
x6 <- estagioIII * laringe$idade
x7 <- estagioIV * laringe$idade
fit4 <- coxph(Surv(tempos, cens) ~ estagioII + estagioIII + estagioIV + idade + x5 + x6 + x7,
  data = laringe, x = T, method = "breslow"
)

# Teste de Razão de Verossimilhanças

# testando se a inclusão da interação é significativa
TRV4 <- 2 * (fit4$loglik[2] - fit3$loglik[2])
1 - pchisq(TRV4, 3)
# testando se a covariável X2 é significativa
TRV3 <- 2 * (fit3$loglik[2] - fit1$loglik[1])
1 - pchisq(TRV3, 1)
# testando se as covariável X1 é significativa
TRV2 <- 2 * (fit2$loglik[2] - fit1$loglik[1])
1 - pchisq(TRV2, 3)

summary(fit3)
summary(fit4)

# Análise de Resíduos

# Os resíduos devem seguir uma exponencial padrão
resm <- resid(fit4, type = "martingale")
res <- laringe$cens - resm # resíduo de cox-Snell
ekm <- survfit(Surv(res, laringe$cens) ~ 1)
summary(ekm)

plot(ekm,
  mark.time = F, conf.int = F, xlab = "Resíduos",
  ylab = "S(e) estimada"
)
res <- sort(res)
exp1 <- exp(-res)
lines(res, exp1, lty = 3)
legend(1, 0.8,
  lty = c(1, 3), c("Kaplan Meier", "Exponencial(1)"),
  lwd = 1, bty = "n", cex = 0.7
)

# Deve formar uma linha aproximadamente reta
st <- ekm$surv
t <- ekm$time
sexp1 <- exp(-t)
plot(st, sexp1,
  xlab = "S(e): Kaplan-Meier",
  ylab = "S(e): Exponencial(1)", pch = 16
) 

# Suposição de riscos proporcionais

# Ho: riscos proporcionais
test.ph4 <- cox.zph(fit4, transform = "identity")
# Logo somente para estagioIII não temos evidências de riscos proporcionais

ggcoxdiagnostics(fit4,
  type = "scaledsch", linear.predictions = FALSE,
  ggtheme = theme_bw()
)

# Ho: riscos proporcionais
test.ph3 <- cox.zph(fit3, transform = "identity")
# Logo, temos evidência de riscos proporcionais para todas as variáveis

ggcoxdiagnostics(fit3,
  type = "scaledsch", linear.predictions = FALSE,
  ggtheme = theme_bw()
)


# Avaliação dos resíduos - Pontos atípicos
ggcoxdiagnostics(fit4,
  type = "deviance",
  linear.predictions = TRUE, ggtheme = theme_bw()
)
ggcoxdiagnostics(fit4,
  type = "martingale",
  linear.predictions = TRUE, ggtheme = theme_bw()
)

# Avaliação dos resíduos - Pontos influentes
ggcoxdiagnostics(fit4,
  type = "dfbetas",
  linear.predictions = FALSE, ggtheme = theme_bw()
)
