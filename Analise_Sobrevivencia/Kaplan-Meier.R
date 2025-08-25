# Carregando pacotes -----------------------------------------------------------

library(survival)

# Exemplo dados hepatite

tempo = c(1, 1, 1, 1, 4, 5, 7, 8, 10, 10, 12, 16, 16, 16)
censura = c(1, 1, 1, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0)
dados=cbind(tempo, censura)

# comando para calcular o estimador de Kaplan-Meier
ekm=survfit(Surv(tempo, censura)~1, conf.type="plain")
summary(ekm)

# Plot do Kaplan-Meier para ambos os grupos
plot(ekm, xlab="Tempo em semanas", ylab = "S(t) estimada")

# Considerando uma estimativa da curva de sobrevivência para cada grupo
tempo = c(1, 2, 3, 3, 3, 5, 5, 16, 16, 16, 16, 16, 16,
          16, 16, 1, 1, 1, 1, 4, 5, 7, 8, 10, 10, 12,
          16, 16, 16)
censura = c(0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            1, 1, 1, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0)
grupo = c(rep(1, 15), rep(2, 14))
dados=cbind(tempo, censura, grupo)

# calculando as estimativas para S(t) por grupo
ekm2=survfit(Surv(tempo, censura)~grupo, conf.type="log-log")
summary(ekm2)

# Plotando as curvas
plot(ekm2, lty=c(2,1), xlab="Tempo em semanas",
     ylab = "S(t) estimada")
legend(1, 0.3, lty=c(2,1), c("Controle", "Esteróide"),
       lwd=1, bty="n")

# Podemos ver que as curvas não se interceptam em nenhum momento e que apresentam
# comportamentos bem distintos, sugerindo que há diferença na curva de sobrevivência
# entre os indivíduos do grupo controle e grupo esteróide.

