#-------------------------------------------------------------------------------
#Pacotes Capítulo 5
#-------------------------------------------------------------------------------
suppressMessages(library(tseries))
suppressMessages(library(forecast))
suppressMessages(library(ggplot2))
suppressMessages(library(plotly))
suppressMessages(library(magrittr))
suppressMessages(library(ggseas))
suppressMessages(library(lmtest))
suppressMessages(library(ggpubr))
suppressMessages(library(fma))
suppressMessages(library(TTR))
suppressMessages(library(readxl))
suppressMessages(library(tidyquant))
suppressMessages(library(dplyr))
suppressMessages(library(astsa))
suppressMessages(library(L1pack))
suppressMessages(library(webr))
suppressMessages(library(randtests))
suppressMessages(library(Kendall))
suppressMessages(library(fpp))
suppressMessages(library(trend))
suppressMessages(library(moments))

#------------------------------------------------------------------
#Suavização Exponencial Simples - SES
#------------------------------------------------------------------
#Série temporal referente às médias anuais das temperaturas na cidade
#de Nova York durante os anos de 1912 e 1971. Vamos ajustar um modelo
#SES a esta serie temporal e tentar prever a temperatura nos próximos
#12 anos.
#------------------------------------------------------------------
temperatura <- read_excel("C://Users//leona//Dropbox//Faculdade//6° Semestre//Séries Temporais I//Dados//temperatura.xls")
dados<-temperatura$Temperatura
dados<-ts(temperatura$Temperatura,start=c(1912),frequency = 1)
autoplot(dados)
#----------------------------
tmp<-ses(dados,h=12)
#----------------------------
names(tmp)

summary(tmp)
#----------------------------
# Plotando a previsão do modelo de suavização exponencial simples
autoplot(tmp)+ labs(y = "Temperatura", x = "Ano",title="Predição - Previsão - SES")+theme_minimal()

#----------------------------
# Plotando a predição do modelo de suavização exponencial simples
fig<-autoplot(tmp)+ labs(y = "Temperatura", x = "Ano",title="Predição - Previsão - SES")+theme_minimal()
fig<-fig+autolayer(tmp$fitted)
fig
#----------------------------
# Resíduo
res_ses<-tmp$residuals
#----------------------------
#Análise dos Resíduos
ggtsdisplay(res_ses,plot.type="scatter", theme=theme_bw())
#  Podemos ver que os erros de previsão são não autocorrelacionados e 
# aparentemente normais, já que estão entre (-3,3) no gráfico 1 e 3.

ggtsdisplay(res_ses,plot.type="histogram", theme=theme_bw())
# Pelo histograma podemos ver mais facilmente a distribuição dos erros de previsão
#----------------------------
#FAC dos Resíduos
ggAcf(res_ses, lag.max=100,type = c("correlation"))+labs(y = "FAC Amostral Resíduos SES",title="")+
  theme_minimal()
# Mais uma vez, temos indícios de que os erros de previsão são não autocorrelacionados

#----------------------------
#QQ Plot dos Resíduos
ggqqplot(res_ses)+ggtitle("Resíduos Modelo SES")
#  Pelo QQ Plot, vemos que todos os pontos estão dentro dos intervalos de confiança,
# o que nos leva a acreditar que os erros de previsão são normalmente distribuídos

#----------------------------
#Densidade dos Resíduos
plot(density(res_ses),main="Random Error")
#----------------------------
#Teste de Normalidade dos Resíduos
shapiro.test(res_ses)
#  Pelo teste de Shapito Wilk, ao nível de significância de 5%, temos que os 
# erros de previsão são normalmente distribuídos

#----------------------------
# Teste de Box.test
Box.test(res_ses,lag=20)
# Hipótese Nula: Não autocorrelacionado
#  Logo, pelo Box test, ao nível de significância de 5%, temos que os erros de 
# previsão são não autocorrelacionados

#------------------------------------------------------------------
#Suavização Exponencial de Holt - SEH
#------------------------------------------------------------------
#Número anual de passageiros das companhias aéreas australianas.
#------------------------------------------------------------------

# Plot da série temporal
air <- window(ausair, start=1990)
autoplot(air) +
  ggtitle("Plot da Série") + xlab("Ano") +
  ylab("Air passengers in Australia (millions)")+theme_minimal()

# Gráfico de autocorrelação da série
ggAcf(air, lag.max=20,type = c("correlation"))+labs(y = "FAC Amostral",title="")+
  theme_minimal()
#----------------------------
mod1<-holt(air, h=10,damped=FALSE)#Clássico
mod2<-holt(air, h=10,damped=TRUE, phi = NULL)#Amortecido
#----------------------------
summary(mod1)
summary(mod2)

csm.mod1<-c(mod1$model$aic,mod1$model$bic,mod1$model$aicc)
csm.mod2<-c(mod2$model$aic,mod2$model$bic,mod2$model$aicc)
scm.mod<-rbind(csm.mod1,csm.mod2)
colnames(scm.mod)<-c("AIC","BIC","AICC")
rownames(scm.mod)<-c("H_C","H_A")
knitr::kable(scm.mod)
#  Segundo os critérios de selação de modelos, temos que o modelo de Holt Clássico
# é melhor.
#----------------------------
#Análise de Resíduos para cada modelo
mod<-mod2#<-------------------Troca do Modelo
# Previsão do modelo Amortecido de Holt
autoplot(mod)+theme_minimal()
names(mod)
summary(mod)
#----------------------------
#Resíduos
res_holt<-mod$residuals
#----------------------------
#Análise de Resíduos 
ggtsdisplay(res_holt,plot.type="scatter", theme=theme_bw())
#----------------------------
#FAC dos Resíduos
ggAcf(res_holt, lag.max=100,type = c("correlation"))+labs(y = "FAC Amostral Resíduos SEH",title="")+
  theme_minimal()
#----------------------------
#QQ Plot dos Resíduos
ggqqplot(res_holt)+ggtitle("Resíduos Modelo SEH")
#----------------------------
#Densidade dos Resíduos
plot(density(res_holt),main="Random Error")
#----------------------------
#Teste de Normalidade dos Resíduos
shapiro.test(res_holt)
#----------------------------
#Teste de Corelações
Box.test(res_holt,lag=10)
# Pelas análise, temos que os erros de previsão são normais e não correlacionados

#----------------------------
#Previsão dos dois modelos no mesmo gráfico
autoplot(air) +
  autolayer(mod1, series="Método de Holt", PI=FALSE) +
  autolayer(mod2, series="Métodos de Holt Amortecido", PI=FALSE) +
  ggtitle("Previsao Pelo Metodo de Holt") + xlab("Ano") +
  ylab("Air passengers in Australia (millions)") +
  guides(colour=guide_legend(title="Previsão"))+theme_minimal()
#------------------------------------------------------------------


#------------------------------------------------------------------
#Ajuste de 3 modelos e escolha do "melhor" modelo via CritC)rios de
#SeleC'C#o de Modelos e Medidas de Acurácia
#------------------------------------------------------------------
#Numeros anuais de gado ovino na Asia (em milhoes de cabeC'as)
#------------------------------------------------------------------
autoplot(livestock) +
  xlab("Year") + ylab("Livestock, sheep in Asia (millions)")+theme_minimal()
mod1 <-ses(livestock, h=12) 
mod2<-holt(livestock, h=12, damped=FALSE)#ClCássico
mod3<-holt(livestock, h=12, damped=TRUE, phi = NULL)#Amortecido  
#----------------------------
#Critérios de Seleção de Modelos
csm.mod1<-c(mod1$model$aic,mod1$model$bic,mod1$model$aicc)
csm.mod2<-c(mod2$model$aic,mod2$model$bic,mod2$model$aicc)
csm.mod3<-c(mod3$model$aic,mod3$model$bic,mod3$model$aicc)
scm.mod<-rbind(csm.mod1,csm.mod2,csm.mod3)
colnames(scm.mod)<-c("AIC","BIC","AICC")
rownames(scm.mod)<-c("SES","SEH_C","SEH_A")
knitr::kable(scm.mod)

#  Pelos critérios de seleção dos modelo, deveriamos optar pelo modelo de Holt 
# Clássico

#----------------------------
#Medidas de Acurácia
ac.mod1<-accuracy(mod1)
ac.mod2<-accuracy(mod2)
ac.mod3<-accuracy(mod3)
ac.mod<-rbind(ac.mod1,ac.mod2,ac.mod3)
rownames(ac.mod)<-c("SES","SEH_C","SEH_A")
knitr::kable(ac.mod[,2:6])

#  Pelas medidas de acurácia, deveriamos optar pelo modelo de Holt Clássico, ou
# seja o modelo 2

#----------------------------
#Detalhes do Melhor Modelo
summary(mod2)

# Plot do modelo 2, com a previsão da série
autoplot(mod2)+theme_minimal()


#------------------------------------------------------------------
#Modelo de Holt-Winters - HW
#------------------------------------------------------------------
#Análise da Série temporal do Numero de turistas internacionais na Australia.
#------------------------------------------------------------------
#aust <- window(austourists,start=2005)
aust <- austourists
autoplot(aust) +
  xlab("Ano") +
  ylab("Numero de Visitantes (millions)") +
  ggtitle("Numero de turistas internacionais na Australia")+theme_minimal()

#  Podemos ver que as séries apresentam sazonalidade, inclusive no gráfico de 
# autocorrelações. Podemos ver que a sazonalidade é trimestral.
ggAcf(aust, lag.max=20,type = c("correlation"))+labs(y = "FAC Amostral",title="")+
  theme_minimal()
#----------------------------
#Modelos de Holt-Winters
mod1 <- hw(aust,seasonal="additive")
mod2 <- hw(aust,seasonal="multiplicative")
mod3 <- hw(aust,seasonal="additive",damped=TRUE)
mod4 <- hw(aust,seasonal="multiplicative",damped=TRUE) 
autoplot(mod2) +
  xlab("Ano") +
  ylab("Numero de Visitantes (millions)") +
  ggtitle("Numero de turistas internacionais na Australia") +
  guides(colour=guide_legend(title="Forecast"))+theme_minimal()
#----------------------------
#Comparando as PrediC'C5es
fig <- plot_ly(y = aust, name = 'Dados', type = 'scatter', mode = 'lines') 
fig <- fig %>% add_trace(y = mod1$fitted, name = 'HW-A', mode = 'lines') 
fig <- fig %>% add_trace(y = mod2$fitted, name = 'HW-AA', mode = 'lines')
fig <- fig %>% add_trace(y = mod3$fitted, name = 'HW-M', mode = 'lines')
fig <- fig %>% add_trace(y = mod4$fitted, name = 'HW-MA"', mode = 'lines')
fig <- fig %>% layout(title =paste0("Predição Modelos Holt-Winters"),
                      xaxis = list(title = 'Tempo'),
                      yaxis = list (title = 'Dados'))

fig
#----------------------------
#Critérios de Seleção de Modelos
csm.mod1<-c(mod1$model$aic,mod1$model$bic,mod1$model$aicc)
csm.mod2<-c(mod2$model$aic,mod2$model$bic,mod2$model$aicc)
csm.mod3<-c(mod3$model$aic,mod3$model$bic,mod3$model$aicc)
csm.mod4<-c(mod4$model$aic,mod4$model$bic,mod4$model$aicc)
scm.mod<-rbind(csm.mod1,csm.mod2,csm.mod3,csm.mod4)
colnames(scm.mod)<-c("AIC","BIC","AICC")
rownames(scm.mod)<-c("HW-A","HW-M","HW-AA","HW-MA")
knitr::kable(scm.mod)
#----------------------------
#Medidas de Acurácia
ac.mod1<-accuracy(mod1)
ac.mod2<-accuracy(mod2)
ac.mod3<-accuracy(mod3)
ac.mod4<-accuracy(mod4)
ac.mod<-rbind(ac.mod1,ac.mod2,ac.mod3,ac.mod4)
rownames(ac.mod)<-c("HW-A","HW-M","HW-AA","HW-MA")
knitr::kable(ac.mod[,2:6])
#ou
v<-c(2,3,5,6)
knitr::kable(ac.mod[,v])


#----------------------------
#ResC-duo
res<-mod4$residuals
#----------------------------
#AnC!lise dos Resíduos
ggtsdisplay(res,plot.type="scatter", theme=theme_bw())
#----------------------------
#FAC dos Resíduos
ggAcf(res, lag.max=100,type = c("correlation"))+labs(y = "FAC Amostral Resíduos",title="")+
  theme_minimal()
#----------------------------
#QQ Plot dos Resíduos
ggqqplot(res)+ggtitle("ResC-duo Modelo")
#----------------------------
#Densidade dos Resíduos
plot(density(res),main="Random Error")
#----------------------------
#Teste de Normalidade dos Resíduos
shapiro.test(res)
#----------------------------
#Teste de CorreC'C5es
Box.test(res,lag=10)
#----------------------------
#------------------------------------------------------------------
#AnC!lise da Serie temporal de Producao de Bens Intermediarios.
#------------------------------------------------------------------
library(BETS)
data <- BETSget(21864)
save(data, file = "SERIE_PBI.RData")
load("C://Users//leona//Dropbox//Faculdade//6° Semestre//Séries Temporais I//Dados//SERIE_PBI.RData")
plot(data, ylab = "PBI (Numero Indice)", main = "")
abline(v = seq(2002,2024,1), col = "gray60", lty = 3)
#----------------------------
#Conjuntos de Treino e Teste
data.tr <- window(data,start=2002, end=c(2022,12))
data.te <- window(data,start=c(2023,1))
#----------------------------
autoplot(data.tr) +
  xlab("Ano") +
  ylab("PBI (Numero Indice)") +
  ggtitle("ProduC'C#o de Bens Intermediarios")+theme_minimal()
ggAcf(data.tr, lag.max=100,type = c("correlation"))+labs(y = "FAC Amostral",title="")+
  theme_minimal()
#----------------------------
#Modelos de Holt-Winters
mod1 <- hw(data.tr,seasonal="additive",h=length(data.te))
mod2 <- hw(data.tr,seasonal="multiplicative",h=length(data.te))
mod3 <- hw(data.tr,seasonal="additive",damped =TRUE,h=length(data.te))
mod4 <- hw(data.tr,seasonal="multiplicative",damped =TRUE,h=length(data.te))
#----------------------------
#Critérios de Seleção de Modelos
csm.mod1<-c(mod1$model$aic,mod1$model$bic,mod1$model$aicc)
csm.mod2<-c(mod2$model$aic,mod2$model$bic,mod2$model$aicc)
csm.mod3<-c(mod3$model$aic,mod3$model$bic,mod3$model$aicc)
csm.mod4<-c(mod4$model$aic,mod4$model$bic,mod4$model$aicc)
scm.mod<-rbind(csm.mod1,csm.mod2,csm.mod3,csm.mod4)
colnames(scm.mod)<-c("AIC","BIC","AICC")
rownames(scm.mod)<-c("HW-A","HW-M","HW-AA","HW-MA")
kable(scm.mod)
#----------------------------
#Medidas de Acurácia - Conjunto Treino
ac.mod1<-accuracy(mod1)
ac.mod2<-accuracy(mod2)
ac.mod3<-accuracy(mod3)
ac.mod4<-accuracy(mod4)
ac.mod<-rbind(ac.mod1,ac.mod2,ac.mod3,ac.mod4)
rownames(ac.mod)<-c("HW-A","HW-M","HW-AA","HW-MA")
kable(ac.mod[,1:6])
v<-c(2,3,5,6)
kable(ac.mod[,v])
#----------------------------
#Medidas de Acurácia - Conjunto Teste
mod1.for<-forecast(mod1)
mod2.for<-forecast(mod2)
mod3.for<-forecast(mod3)
mod4.for<-forecast(mod4)
ac.mod1.for<-accuracy(mod1.for$mean,data.te)
ac.mod2.for<-accuracy(mod2.for$mean,data.te)
ac.mod3.for<-accuracy(mod3.for$mean,data.te)
ac.mod4.for<-accuracy(mod4.for$mean,data.te)
ac.mod.for<-rbind(ac.mod1.for,ac.mod2.for,ac.mod3.for,ac.mod4.for)
rownames(ac.mod.for)<-c("HW-A","HW-M","HW-AA","HW-MA")
kable(ac.mod.for[,1:6])
v<-c(2,3,5)
kable(ac.mod.for[,v])
#----------------------------
#Melhor Modelo Treino/Teste
clrs <- c("darkgreen","black", "blue3", "darkred")
autoplot(mod4) +
  autolayer(data.tr, series='Data-Train') + 
  autolayer(data.te, series='Data-Test') +
  autolayer(fitted(mod4), series='Fitted-Train') + 
  autolayer(mod4$mean, series="Forecast") + 
  xlab("Meses") +
  ylab("PBI") +
  ggtitle("Predição-Previsão - Modelo Holt-Winters Multiplicativo")+
  guides(colour=guide_legend(title="Data series"), 
         fill=guide_legend(title="Prediction interval"))+
  scale_color_manual(values=clrs)+theme_minimal()
#----------------------------
#Grafico Predição-Previsão - Melhor Modelo 
bestmod<-hw(data,seasonal="multiplicative",damped =TRUE,h=24)
bestmod.for<-forecast(bestmod,h=24)
cd<-rep("NA",length(data))
fig <- plot_ly(y = data, name = 'Dados', type = 'scatter', mode = 'lines') 
fig <- fig %>% add_trace(y = bestmod$fitted, name = 'Predição', mode = 'lines') 
fig <- fig %>% add_trace(y = c(cd,bestmod$lower[,2]), name = 'LI', mode = 'lines',line = list(color = 'green')) 
fig <- fig %>% add_trace(y = c(cd,bestmod$upper[,2]), name = 'LS', mode = 'lines',line = list(color = 'green'))
fig <- fig %>% add_trace(y = c(cd,bestmod$mean), name = 'Previsão', mode = 'lines',line = list(color = 'red'))
fig <- fig %>% layout(title =paste0("Predição-Previsão Modelo\n(Holt-Winters Multiplicativo)"),
                      xaxis = list(title = 'Tempo'),
                      yaxis = list (title = 'Dados'))
fig
#----------------------------
clrs <- c("darkgreen","black", "blue3", "darkred")
autoplot(bestmod.for) +
  autolayer(bestmod$mean, series="Forecast") +
  autolayer(fitted(bestmod), series='Fitted') + 
  xlab("Meses") +
  ylab("PBI") +
  ggtitle("Predição-Previsão - Modelo Holt-Winters Multiplicativo")+
  guides(colour=guide_legend(title="Data series"), 
         fill=guide_legend(title="Prediction interval"))+
  scale_color_manual(values=clrs)+theme_minimal()
#----------------------------

#------------------------------------------------------------------
#Modelos ETS 
#------------------------------------------------------------------
#AnC!lise da Serie temporal do Numero de turistas internacionais na Australia.
#------------------------------------------------------------------
#library(fpp)
aust <- window(austourists, start=1999)
autoplot(aust)
#----------------------------
#Modelo
mod <- ets(aust)
summary(mod)
#----------------------------
#Grafico Previsão +IC
autoplot(mod)+theme_minimal()
mod %>% forecast(h=8) %>%
  autoplot() +
  ylab("Numero de turistas internacionais na Australia (millions)")+
  theme_minimal()
#----------------------------
accuracy(mod)
mod.for<-forecast(mod)
autoplot(mod.for)+theme_minimal()
#----------------------------
#ComparaC'C#o Resíduos e erros de Previsão
cbind('Residuals' = residuals(mod),
      'Forecast errors' = residuals(mod,type='response')) %>%
  autoplot(facet=TRUE) + xlab("Year") + ylab("")+theme_minimal()
#----------------------------
#ResC-duo
res_ets<-residuals(mod)
#----------------------------
#AnC!lise dos Resíduos
ggtsdisplay(res_ets,plot.type="scatter", theme=theme_bw())
#----------------------------
#FAC dos Resíduos
ggAcf(res_ets, lag.max=100,type = c("correlation"))+labs(y = "FAC Amostral Resíduos SEH",title="")+
  theme_minimal()
#----------------------------
#QQ Plot dos Resíduos
ggqqplot(res_ets)+ggtitle("ResC-duo Modelo")
#----------------------------
#Densidade dos Resíduos
plot(density(res_ets),main="Random Error")
#----------------------------
#Teste de Normalidade dos Resíduos
shapiro.test(res_ets)
adf.test(res_ets)
#----------------------------
#Teste de CorrelaC'C5es
Box.test(res_ets,lag=10)
#----------------------------
hist(res_ets)

plot.ts(cbind(mod$fitted,aust),plot.type="single",col=c("red","blue"))
accuracy(mod)
mod.for<-forecast(mod)
autoplot(mod.for)

#------------------------------------------------------------------
#Modelos Com Duas Sazonalidades
#------------------------------------------------------------------
#DecomposiC'C#o 
taylor %>% autoplot()+theme_minimal()
ts(taylor[1:800]) %>% autoplot()+theme_minimal()
mstl(taylor) %>% autoplot()+theme_minimal()
#----------------------------
#Modelo DSHW
mod<-dshw(taylor)
mod %>% autoplot()+theme_minimal()
summary(mod)
#----------------------------
clrs <- c("darkgreen","black", "blue3", "darkred")
autoplot(mod) +
  autolayer(taylor, series="Data") +
  autolayer(fitted(mod), series="Fitted")+
  autolayer(mod$mean, series="Forecast") +
  xlab("Tempo") +
  ylab("Taylor") +
  ggtitle("Predição-Previsão - Modelo DS Holt-Winters")+
  guides(colour=guide_legend(title="Data Series"))+
  scale_color_manual(values=clrs)+theme_minimal()
