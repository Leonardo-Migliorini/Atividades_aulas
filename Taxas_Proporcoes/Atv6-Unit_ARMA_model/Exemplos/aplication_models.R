# Implementado por Renata Rojas Guerra em 24 out 2023
# carregando os pacotes que serão usados
library(forecast)
library(BTSR)
library(readr)
# carregando os dados
dados <-  read_delim("ipeadata-tx-des.csv", 
                 delim = ";", escape_double = FALSE, 
                 locale = locale(decimal_mark = ","), 
                 col_names = c("Data","tx_desocup","c"),
                 skip=1,
                 trim_ws = TRUE)[,-3]
# definindo a variável de interesse
y <-ts(dados$tx_desocup/100,freq=12,start = c(2012,3))
# # graficos interessantes
w1<-4.5 # width for plots 
h11<-4.5 # height for plots
setEPS()
postscript("serie.eps",width = w1, height = h11,family = "Times")
plot(y,main="",
     cex.axis=1.2,xlab="Tempo",
     lwd=1,ylab="",cex.lab=1.2, cex.main=1.2, cex.sub=1.2) 
dev.off()

postscript("monthplot.eps",width = w1, height = h11,family = "Times")
monthplot(y,main="",
     cex.axis=1.2,xlab="Tempo",
     lwd=1,ylab="",cex.lab=1.2, cex.main=1.2, cex.sub=1.2) 
dev.off()

postscript("acf-serie.eps",width = w1, height = h11,family = "Times")
acf(y,main="",
          cex.axis=1.2,xlab="Tempo",
          lwd=1,ylab="",cex.lab=1.2, cex.main=1.2, cex.sub=1.2) 
dev.off()

postscript("pacf-serie.eps",width = w1, height = h11,family = "Times")
acf(y,main="",
    cex.axis=1.2,xlab="Tempo",
    lwd=1,ylab="",cex.lab=1.2, cex.main=1.2, cex.sub=1.2) 
dev.off()

# hist(y)
# plot(y)
# monthplot(y)
# acf(y)
# pacf(y)
# criando conjnto de treino
n<-length(y)
ntest<-10
y_train<-y[1:(n-ntest)]
y_test<-y[(n-ntest+1):n]
# ajustando o modelo ARIMA
fit1<-auto.arima(y_train)
fitted1<-fit1$fitted
forecast1<- predict(fit1,ntest)$pred
# ajustando o modelo barma
cont<-0
order<-matrix(NA,15,4)
for(i in 0:3){
  for(j in 0:3){
    if(i==0 && j==0) next
    cont<-cont+1
    print(c("i=",i,"j=",j))
    barma<-summary(BARFIMA.fit(y_train,p=i,d=F,q=j,
                       report=F,info=T))
    karma<-try(summary(KARFIMA.fit(y_train,p=i,d=F,q=j,
                               report=F,info=T)),silent = T)
    if(class(karma)=="try-error") # a classe dos objetos que cont?m o erro, 
    {suppressWarnings(karma$aic<-1e50)}
    order[cont,]<-c(i,j,barma$aic,karma$aic)
  }
}

print(order)
order<-order[1:14,]

orbarma<-order[which(order[,3]==min(order[,3])),c(1:3)]
orkarma<-order[which(order[,4]==min(order[,4])),c(1:2,4)]

barma<-BARFIMA.fit(y_train,p=orbarma[1],d=F,q=orbarma[2],
                   info=T,report=F)
forecastb<- predict(barma,nnew=ntest)$forecast

karma<-KARFIMA.fit(y_train,p=orkarma[1],d=F,q=orkarma[2],
                   info=T,report=F)
forecastk<- predict(karma,nnew=ntest)$forecast

results_insample<-rbind(
  forecast::accuracy(fit1$fitted, y_train),
  forecast::accuracy(barma$fitted.values, y_train),
  forecast::accuracy(karma$fitted.values, y_train)
)[,c(3,2,5)]

results_insample

results<-rbind(
  forecast::accuracy(forecast1, y_test),
  forecast::accuracy(forecastb, y_test),
  forecast::accuracy(forecastk, y_test)
)[,c(3,2,5)]

rownames(results)<-c("ARIMA(0,3)","BARMA(0,3)","KARMA(1,2)")

xtable::xtable(results,digits = 3)

postscript("serie-fitted.eps",width = w1, height = h11,family = "Times")
plot(y,main="",
     cex.axis=1.2,xlab="Tempo",ylim=c(.06,.2),
     lwd=1,ylab="",cex.lab=1.2, cex.main=1.2, cex.sub=1.2) 
lines(ts(c(fit1$fitted,forecast1),freq=12,start = c(2012,3)),col=2)
lines(ts(c(barma$fitted.values,forecastb),
           freq=12,start = c(2012,3)),col=3)
lines(ts(c(karma$fitted.values[-1],forecastk),
           freq=12,start = c(2012,4)),col=4)
abline(v=2022.75)
legend("topleft",
          c("Original","ARIMA","BARMA","KARMA"),
          col=1:4, cex=1.2,
          lwd=rep(.5,4),
          ncol=2,
          bty="n")
dev.off()

postscript("acf-resid.eps",width = w1, height = h11,family = "Times")
acf(fit1$residuals,main="",
    cex.axis=1.2,xlab="Tempo",
    lwd=1,ylab="",cex.lab=1.2, cex.main=1.2, cex.sub=1.2) 
dev.off()

postscript("pacf-resid.eps",width = w1, height = h11,family = "Times")
pacf(fit1$residuals,main="",
    cex.axis=1.2,xlab="Tempo",
    lwd=1,ylab="",cex.lab=1.2, cex.main=1.2, cex.sub=1.2) 
dev.off()

