#--
library(tidyverse)
library(latex2exp)
library(ggplot2)
library(tibble)
library(purrr)
library(viridis)

source("UQChen Functions.R")

#----------------------------------- Variação do sigma -------------------------
{
  x<-seq(0,1,length.out = 1000)
  mu<-0.8
  sigma<-c(0.3,0.7,1,1.5,2)
  data<-map_df(sigma, ~ tibble(v=dUQChen(x,.,mu=mu), x=x, sigma=.))
}
#http://sape.inf.usi.ch/quick-reference/ggplot2/linetype
#myDot<-c("blank", "solid", "dashed", "dotted", "dotdash", "longdash", "twodash", "1F", "F1", "4C88C488", "12345678")
{
  myDot<-c("solid", "dotdash", "dotted", "twodash","longdash")
  mycolor<-rep("black",5)
  
  last_plot<-ggplot(data,aes(x = x, y = v, colour =as.factor(sigma), linetype=as.factor(sigma)))+xlim(0,1)+ylim(0,7)+geom_line(size = 0.8)+
    scale_linetype_manual(name= "Legenda", values = myDot,labels=lapply(sprintf(r'($\sigma = %f$)', sigma), TeX))+
    scale_color_manual(name="Legenda",values=mycolor,labels=lapply(sprintf(r'($\sigma = %f$)', sigma), TeX))+
    labs(y = "Função Densidade de Probabilidade",x = "y",title="")+theme_minimal(base_size = 16)+theme(legend.position = c(0.2, 0.75))
  
  last_plot
}

# max(data$v)+0.5
#-------------------------------- Salvar Plot ----------------------------------

ggsave("dens_plot_sigma.pdf",plot = last_plot, width = 13, height = 12, units = "cm")

#----------------------------------- Variação do mu ------------------------
{
  x<-seq(0,1,length.out = 1000)
  mu<-c(0.1,0.3,0.5,0.7,0.9)
  sigma<-c(1.2)
  data<-map_df(mu, ~ tibble(v=dUQChen(x,.,sigma=sigma), x=x, mu=.))
}
#http://sape.inf.usi.ch/quick-reference/ggplot2/linetype
#myDot<-c("blank", "solid", "dashed", "dotted", "dotdash", "longdash", "twodash", "1F", "F1", "4C88C488", "12345678")
{
  myDot<-c("solid", "dotdash", "dotted", "twodash","longdash")
  mycolor<-rep("black",5)
  
  last_plot<-ggplot(data,aes(x = x, y = v, colour =as.factor(mu), linetype=as.factor(mu)))+xlim(0,1)+ylim(0,7)+geom_line(size = 0.8)+
    scale_linetype_manual(name= "Legenda", values = myDot,labels=lapply(sprintf(r'($\mu = %f$)', mu), TeX))+
    scale_color_manual(name="Legenda",values=mycolor,labels=lapply(sprintf(r'($\mu = %f$)', mu), TeX))+
    labs(y = "Função Densidade de Probabilidade", x = "y",title="")+theme_minimal(base_size = 16)+theme(legend.position = c(0.3, 0.75))
  
  last_plot
}

#-------------------------------- Salvar Plot ----------------------------------

ggsave("dens_plot_mu.pdf",plot = last_plot, width = 13, height = 12, units = "cm")
