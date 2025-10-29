# Carregando pacotes -----------------------------------------------------------
library(survival)
library(ggfortify)
library(flexsurv)

# Importando banco de dados ----------------------------------------------------

# ---
# Tempos de sobrevivência, em semanas, de 33 pacientes com leucemia aguda.
# Covariáveis:
# Contagem de glóbulos brancos (WBC) registrados na dato do diagnóstico e com os
# respectivos logaritmos na base 10.
# Grupos:
# apresentaram o antígeno Calla (antigo LLA comum) na superfície dos blastos (Ag+)
# e que não expressaram este antígeno na superfície (Ag-).
# ---

dados <- data.frame(
temp = c(65, 156, 100, 134, 16, 108, 121, 4, 39, 143, 56, 26, 22, 1, 1, 5, 65,
          56, 65, 17, 7 ,16, 22, 3, 4, 2, 3, 8, 4, 3, 30, 4, 43),
cens = c(rep(1,17), rep(1,16)),
lwbc = c(3.36, 2.88, 3.63, 3.41, 3.78, 4.02, 4, 4.23, 3.73, 3.85, 3.97, 4.51,
          4.54, 5, 5, 4.72, 5, 3.64, 3.48, 3.6, 3.18, 3.95, 3.72, 4, 4.28, 4.43,
          4.45, 4.49, 4.41, 4.32, 4.90, 5, 5),
grupo = c(rep(0,17), rep(1,16)))

# Calculando o estimador de Kaplan Meier
ekm1 <- survfit(Surv(dados$temp,dados$cens)~dados$grupo)
summary(ekm1)
autoplot(ekm1)

# Armazenando as estimativas por grupos
st1 <- ekm1[1]$surv
time1 <- ekm1[1]$time

st2 <- ekm1[2]$surv
time2 <- ekm1[2]$time

# Plotando os ajustes das distribuições

# Distribuição exponencial
par(mfrow = c(1,3))
plot(time1, -log(st1), pch = 16, xlab = "tempos",
     ylab = "-log(S(t))", main = "Exponencial")
points(time2, -log(st2))
legend(100, 0.6, pch = c(16,1), c("Ag+", "Ag-"), bty = "n")

# Distribuição Weibull 
plot(log(time1), log(-log(st1)), pch = 16, xlab = "log(tempos)",
     ylab = "log(-log(S(t)))", main = "Weibull")
points(log(time2), log(-log(st2)))
legend(3, -1.5, pch = c(16,1), c("Ag+", "Ag-"), bty = "n")

# Distribuição Log-normal
invst1 <- qnorm(st1)
invst2 <- qnorm(st2)
plot(log(time1), invst1, pch = 16, xlab = "log(tempos)",
     ylab = expression(Phi^-1*(S(t))), main = "Log-normal")
points(log(time2), invst2)
legend(0.5, -1, pch = c(16,1), c("Ag+", "Ag-"), bty = "n")

# Ajustando distribuições

fitg <- flexsurvreg(Surv(dados$temp, dados$cens)~1, dist = "gengamma")
fite <- survreg(Surv(dados$temp, dados$cens)~1, dist = "exponential")
fitw <- survreg(Surv(dados$temp, dados$cens)~1, dist = "weibull")
fitln <- survreg(Surv(dados$temp, dados$cens)~1, dist = "lognormal")

pvalor.e <- 1 - pchisq(2*(fitg$loglik[1] - fite$loglik[1]), 2)
pvalor.w <- 1 - pchisq(2*(fitg$loglik[1] - fitw$loglik[1]), 2)
pvalor.ln <- 1 - pchisq(2*(fitg$loglik[1] - fitln$loglik[1]), 2)
pvalor.e2 <- 1 - pchisq(2*(fitw$loglik[1] - fite$loglik[1]), 2)

# Pelo teste da razão de verossimilhanças, optamos por escolher o modelo 
# exponencial para a modelagem dos dados

# Ajustando modelo de regressão exponencial

fit1 <- survreg(Surv(dados$temp, dados$cens)~1, dist = "exponential")
fit2 <- survreg(Surv(dados$temp, dados$cens)~dados$lwbc, dist = "exponential")
fit3 <- survreg(Surv(dados$temp, dados$cens)~dados$grupo, dist = "exponential")
fit4 <- survreg(Surv(dados$temp, dados$cens)~dados$lwbc + dados$grupo, dist = "exponential")
fit5 <- survreg(Surv(dados$temp, dados$cens)~dados$lwbc * dados$grupo, dist = "exponential")

# Testando a significância das covariáveis

TRV1 <- 2*(fit5$loglik[2]-fit4$loglik[2])
TRV2 <- 2*(fit2$loglik[2]-fit1$loglik[2])
TRV3 <- 2*(fit4$loglik[2]-fit2$loglik[2])

pvalor.i <- 1 - pchisq(2*(fit5$loglik[2]-fit4$loglik[2]),1)
pvalor.x1 <- 1 - pchisq(2*(fit2$loglik[2]-fit1$loglik[2]),1) 
pvalor.x2x1 <- 1 - pchisq(2*(fit4$loglik[2]-fit2$loglik[2]),1)

# Pelos pvalores, conclui-se que a interação não é significativa para o modelo,
# mas x1 e x2 são significativas.

# Análise de resíduos - Cox Snell

X <- cbind(1, dados$lwbc, dados$grupo) # matriz de covariáveis
beta <- fit4$coefficients # vetor de regressores estimados

res <- dados$temp * exp(-X%*%(beta))
res1 <- sort(res)
exp1 <- exp(-res1)

ekm <- survfit(Surv(res,dados$cens)~1, type = c("kaplan-meier"))

# Plot dos resíduos vs sobrevivência ajustada para os resíduos
par(mfrow = c(1, 1))
plot(ekm, conf.int = F, lty = c(1, 1),
     xlab = "residuos", ylab = "S(res) estimada")
lines(res1, exp1, lty = 3)
legend(1.2, 1, lty = c(1, 3), c("Kaplan-Meier", "Exponencial(1)"), lwd = 1, 
       bty = "n", cex = 0.8)

# Resíduo KM

st <- ekm$surv
t <- ekm$time
sexp1 <- exp(-t)
plot(st, sexp1, xlab = "S(res): kaplan-meier", ylab = "S(res): Exponencial(1)", 
     pch = 16)

# Resíduo padronizado

resp <- log(dados$temp) - X%*%(beta)
plot(resp)

# Resíduo martingal
m <- dados$cens - res

plot(dados$lwbc, m)
lines(lm(m~dados$lwbc), col = 1, lty = 1)

# Resíduo deviance

d <- sign(m)*sqrt(-2*(m+dados$cens*log(dados$cens-m)))
rd <- resid(fit4, type = "deviance")
plot(d)
plot(rd)

# Análise de diagnóstico

plot(dados$temp,resid(fit4, type="dfbetas")[,1])
# identify(dados$temp,resid(fit4, type="dfbetas")[,1], n=1)

plot(dados$temp,resid(fit4, type="dfbetas")[,2])
# identify(dados$temp,resid(fit4, type="dfbetas")[,2], n=1)

plot(dados$temp,resid(fit4, type="dfbetas")[,3])
# identify(dados$temp,resid(fit4, type="dfbetas")[,3], n=1)

plot(dados$temp,resid(fit4, type="ldresp"))
# identify(dados$temp,resid(fit4, type="ldresp"), n=1)

plot(dados$temp,resid(fit4, type="ldshape"))
# identify(dados$temp,resid(fit4, type="ldshape"), n=1)

# Pela análise de diagnóstico, temos indícios de que a observação 33 é um 
# ponto influente no modelo

dado33 <- data.frame(dados[-33,])
fit33 <- survreg(Surv(temp,cens)~lwbc + grupo, data=dado33, dist="exponential")
summary(fit33)
(1-fit4$coefficients/fit33$coefficients)*100

# Interpretação do Modelo:

# beta_1 é negativo, então quanto maior o valor de x1, menor a probabilidade de 
# sobrevivência estimada. 
# exp(beta_1) = 0.496, o aumento de uma unidade no valor de x1 diminui em 50% o 
# tempo mediano.
# beta_2 é negativo, então pacientes do grupo Ag- (x2 = 1) apresentam 
# probabilidade de sobrevivência estimada menor do que pacientes no grupo Ag+ 
# (x2 = 0).
# exp(beta_2) = 0.361. O tempo mediano de sobrevida diminui 63.86% para o grupo Ag-

par(mfrow = c(1,2))
t <- seq(0, 156, 0.01)
Ag_minus4 <- exp(-t / exp(fit33$coefficients[1] + fit33$coefficients[2] * 4 + fit33$coefficients[3]*1))
Ag_minus3 <- exp(-t / exp(fit33$coefficients[1] + fit33$coefficients[2] * 3 + fit33$coefficients[3]*1))

# Curvas estimadas para pacientes com x_1 =3 e x_1 = 4 e x_2 = 1:
plot(t, Ag_minus4,
     lty = 4, col = 1, lwd = 1, xlab = "tempos",
     ylab = "Sobrevivência estimada", type = "l",
     main = "Ag-"
)
lines(t, Ag_minus3, lty = 4, col = 2)
legend(100, 1.0,
       lty = c(4, 4), col = c(1, 2), c("lwbc=4", "lwbc=3"),
       cex = 0.8, bty = "n"
)

Ag_plus4 <- exp(-t / exp(fit33$coefficients[1] + fit33$coefficients[2] * 4 ))
Ag_plus3 <- exp(-t / exp(fit33$coefficients[1] + fit33$coefficients[2] * 3 ))

# Curvas estimadas para pacientes com x_1 =3 e x_1 = 4 e x_2 = 1:
plot(t, Ag_plus4,
     lty = 4, col = 1, lwd = 1, xlab = "tempos",
     ylab = "Sobrevivência estimada", type = "l",
     main = "Ag+"
)
lines(t, Ag_plus3, lty = 4, col = 2)
legend(100, 1.0,
       lty = c(4, 4), col = c(1, 2), c("lwbc=4", "lwbc=3"),
       cex = 0.8, bty = "n"
)

# Conclusões:

# Dos resultados apresentados verificou-se que o modelo de regressão exponencial 
# se ajustou satisfatoriamente aos dados dos tempos de sobrevivência dos 
# pacientes com leucemia aguda. De maneira geral pode-se ainda concluir que o 
# tempo de sobrevida estimado dos pacientes diminui `a medida que, no 
# diagnósstico, são observadas contagens crescentes de glóbulos brancos e para
# pacientes do grupo Ag-.

