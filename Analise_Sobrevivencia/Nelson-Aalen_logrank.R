# Estimador de Nelson-Aalen ----------------------------------------------------

# carregando pacotes
library(survival)

# carregando dados
tempos <- c(1,2,3,3,3,5,5,16,16,16,16,16,16,16,16,1,1,1,1,4,5,7,8,10,10,12,16,16,16)
censura <- c(0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,1,1,1,0,0,1,1,1,1,0,0,0,0,0)
grupos <- c(rep(1,15),rep(2,14))

# Calculando as estimativas de Nelson-Aalen (Função de Sobrevivência) 
ss <- survfit(coxph(Surv(tempos[grupos==2],
  censura[grupos==2])~1,method = "breslow"))
summary(ss)
plot(ss)

# Calculando as estimativas da função taxa de falha
racum <- -log(ss$surv)
racum
plot(racum)

# Teste logrank ----------------------------------------------------------------

# H0: S1(t) = S2(t)

# função para realizar o teste de logrank
survdiff(Surv(tempos,cens)~grupos,rho=0)

# Teste para múltiplos grupos

# Exemplo dados de Malária
tempo <-  c(7, rep(8, 4), 12, 12, 17, 18, 22, rep(30, 6), 8, 8, 9, 10, 10, 14, 15,
          15, 18, 19, 21, 22, 22,23, 25, rep(8, 6), 9, rep(10, 3), 11, 17, 19)
censura <-  c(rep(1, 10), rep(0, 6), rep(1, 28))
grupo <- c(rep(1, 16), rep(2, 15), rep(3, 13))
dados <- cbind(tempo, censura, grupo)

survdiff(Surv(tempo,censura)~grupo,rho=0)

# Verificando se há diferenças duas a duas
survdiff(Surv(tempo[1:31],censura[1:31])~grupo[1:31],rho=0)
survdiff(Surv(tempo[17:44],censura[17:44])~grupo[17:44],rho=0)
survdiff(Surv(c(tempo[1:16],tempo[32:44]),c(censura[1:16],censura[32:44]))~c(
  grupo[1:16],grupo[32:44]),rho=0)

# Plotando as curvas
ekm <- survfit(Surv(tempo,censura)~grupo)
summary(ekm)
plot(ekm, lty=c(1,4,2), xlab="Tempo",ylab="S(t) estimada")
legend(1,0.3,lty=c(1,4,2),c("Grupo 1","Grupo 2", "Grupo 3"),
       lwd=1, bty="n",cex=0.8)

# Exemplo de gráficos aprimorados
library(ggplot2)
library(ggfortify)

tempo <- c(28, 89, 175, 195, 309, 377, 393, 421, 447, 462, 709, 744, 770, 1106, 
           1206, 34, 88, 137, 199, 280, 291, 299, 300, 309, 351, 358, 369, 369, 
           370, 375, 382, 392, 429,451, 1119)
censura <- c(rep(1, 5), rep(0, 4), 1, rep(0, 5), rep(1, 6),rep(0, 2), rep(1, 9), 0, 1, 0)
grupo <- c(rep(1, 15), rep(0,20))


eKM <- survfit(Surv(tempo, censura)~grupo)
summary(eKM)

autoplot(eKM)+labs( y = "S(t) estimada ",
                    x = "Tempo em dias")

survdiff(Surv(tempo,censura)~grupo,rho=0)