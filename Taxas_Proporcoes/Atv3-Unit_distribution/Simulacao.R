# Pacotes Necessários ----------------------------------------------------------
library(foreach)
library(knitr)
library(xtable)

# Funções Chen Unitaria --------------------------------------------------------
source("Unit Chen Functions.R")

# Funções para algumas verificações e paralelismo ------------------------------

{
#Valores Positivo-----
is.positive<-function(a)
{
  #todas as posições do vetor são positivas
  k<-length(a)
  tmp<-sum(a>0)
  return(k==tmp)
}

#Testa optim----
test.fun<-function(object){
  if(class(object)=="list"){
    if(object$convergence == 0){
      parameters<-try(object$par,T)
      hess<-try(object$hessian,T)
      var.coef<-try(diag(solve(-hess)),T)
      if(is.numeric(parameters)==TRUE){
        if(is.numeric(var.coef)==TRUE){
          if(is.positive(var.coef)==TRUE){
            z<-c(parameters,var.coef)
            return(z)
          }else{return(FALSE)}
        }else{return(FALSE)}
      }else{return(FALSE)}
    }else{return(FALSE)}
  }else{return(FALSE)}
}

#Testa Gosolnp----
test.fun.gosolnp<-function(object){
  if(class(object)=="list"){
    parameters<-try(object$pars,T)
    if(is.numeric(parameters)==TRUE){return(TRUE)}
    else{return(FALSE)}
  }
}

# Paralelismo----
progresso<-function(iterations){
  iterations <- RS  # used for the foreach loop  
  
  pb <- progress::progress_bar$new(
    format = "[:bar] :elapsed | Faltam: :eta",
    total = iterations,    # 100 
    width = 60)
  
  progress_letter <- rep(LETTERS[1:1], 1)  # token reported in progress bar
  
  # allowing progress bar to be used in foreach 
  progress <- function(n){
    pb$tick(tokens = list(letter = progress_letter[n]))
  } 
  
  opts <- list(progress = progress)
  return(opts)
}
}

# Função para encontrar o valor dos estimadores (MLE) --------------------------
mle_UC<-function(y,method=c("Nelder-Mead","BFGS","CG","SANN","Brent")){
  
  #Start Point and Limits For the Parameters
  p<-2
  inf<-rep(0,length.out=p)
  sup<-rep(10,length.out=p)
  k1<-2*p
  nsp<-5
  mint<-100
  
  mod.gosolnp<-try(Rsolnp::gosolnp(fun=loglike_uchen,n.restarts=nsp,LB=inf,UB=sup,
                                   n.sim=mint,y=y,m.optim=-1,control=list(trace=F)),T)
  #-----------------------------------------------------------------------------
  tmp<-test.fun.gosolnp(mod.gosolnp)
  if(tmp==TRUE){
    start.theta<-try(mod.gosolnp$pars,T)
    #------Estimação------------------------------------------------------------
    mod<-try(optim(par=start.theta,fn=loglike_uchen,method=method,hessian=TRUE,
                   control=list(fnscale=-1),y=y,m.optim=1.0),T)
    #------Testes estimativas e variância---------------------------------------
    tmp2<-test.fun(mod)
    if(class(tmp2)=="numeric"){coef<-tmp2}
    else{coef<-rep(NA,k1)}
  }#Fim IF gosolnp
  # else{coef<-rep(NA,k1)}
  return(coef)
}

# Função para calcular medidas de avaliação das estimações (média, viés, eqm, assimetria, curtose, IC e TH)

SIM.COMP<-function(coef,par,names.pars=NULL,conf=c(0.95),nsign=c(0.05),asymptotic=TRUE,arquivo=NULL){
  
  #-----------------------------------------------------------------------------
  #nsign<-nível de significância do Teste de Wald
  theta0<-par
  thetaH0<-par#as.vector(rep(0,length(theta0)))
  re<-length(coef[,1])
  ne<-length(coef[1,])/2
  coeff<-coef[,1:ne]
  var.coef<-coef[,(ne+1):length(coef[1,])]
  contIC<-matrix(0,ncol=length(theta0),nrow=length(conf))
  WTest<-matrix(0,ncol=length(theta0),nrow=length(nsign))
  q.norm<-qnorm((1-conf)/2,lower.tail = FALSE)
  #-------Calculation of accuracy measures--------------------------------------
  vmean<-colMeans(coeff)
  vbias<-vmean-theta0
  vsd<-apply(coeff,2,sd)
  vmse<-vbias^2+vsd^2
  vac<-moments::skewness(coeff)
  vk<-moments::kurtosis(coeff)
  #-----------IC e TH-----------------------------------------------------------
  for(i in 1:re){
    zstat<-as.numeric(abs((coeff[i,]-thetaH0)/sqrt(var.coef[i,])))
    pvalues<-2*(1-pnorm(zstat))
    for(v in 1:length(conf)){
      LS<-coeff[i,]+q.norm[v]*sqrt(var.coef[i,])
      LI<-coeff[i,]-q.norm[v]*sqrt(var.coef[i,])
      contIC[v,]<-contIC[v,]+(LI<=theta0 & theta0<=LS)
    }
    for(v in 1:length(nsign)){
      WTest[v,]<-WTest[v,]+(pvalues<nsign[v])
    }
  }
  tc<-contIC/re
  Wth<-WTest/re
  results<-matrix(0,nrow=6+length(conf)+length(nsign),ncol=length(theta0))
  results<-round(rbind(vmean,vbias,vsd,vmse,vac,vk,tc,Wth),4)
  if(!is.null(names.pars)){colnames(results)<-names.pars}
  P<-c("P1","P2","P3","P4","P5","P6","P7","P8","P9","P10","P11")
  if(is.null(names.pars)){colnames(results)<-P[1:ne]}
  NIC<-paste0("IC",conf*100,"%")
  NTH<-paste0("TH",nsign*100,"%")
  rownames(results)<-c("Mean","Bias","SD","MSE","AC","K",NIC,NTH)
  if(!is.null(arquivo)){
    #salva a Tabela de Resultados no arquivo------------------------------------
    #write(c("#---------------------------"),file=arquivo,append=TRUE,ncolumns=1)
    #print(xtable(results,type="latex",digits = 4),file=arquivo,append=TRUE)
    write(c("#---------------------------"),file=arquivo,append=TRUE,ncolumns=1)
    print(xtable(t(results),type="latex",digits = 4),file=arquivo,append=TRUE)
    #---------------------------------------------------------------------------
  }else{
    return(kable(results))
  }
}


# Simulação de Monte Carlo -----------------------------------------------------
{
  # Fixação de Cenários
  lambda <- data.frame(c(1.2))
  beta <- c(1.5)
  ns <- data.frame(c(50,100,150,200,250,300))
  df1 <- data.frame(merge(lambda,beta))
  df <- merge(ns,df1)
  colnames(df) <- c("n","lambda","beta")
  rm(ns,df1,lambda,beta)
  
  # Número de réplicas de Monte Carlo 
  RS <- 500 #Réplicas de Monte Carlo Simuladas
  RA <- 270 #Réplicas de Monte Carlo Analisadas
  RR <- 200 #Réplicas de Monte Carlo Requeridas
}

# Alocação de Núcleos Paralelismo
cores <- parallel::detectCores()
cl <- parallel::makeCluster(cores) 
doSNOW::registerDoSNOW(cl)

# Loop de Monte Carlo
for(k in 1:nrow(df)){
  
  # Definindo cenário da iteração
  {
    n <- df$n[k]
    lambda <- df$lambda[k]
    beta <- df$beta[k]
    method <- "CG" # Escolha do método de otimização
    arquivo <- "MLE_UC_λ=_β=.txt" # Nome do arquivo para salvar os resultados
  }
  theta0 <- c(lambda,beta)
  rm(lambda,beta)
  
  # Gerando as amostras aleatórias
  sample <- matrix(nrow=RS,ncol=n)
  for(j in 1:RS){ 
    sample[j,] <- as.numeric(ruchen(n=n,lambda=theta0[1],beta=theta0[2]))
  }
  
  # Estimando o valor dos parêmetros com paralelismo
  opts <- progresso(iterations = RS)
  estimate <- bigstatsr::FBM(nrow=RS,ncol=2*length(theta0))
  time <- system.time( 
    foreach(j=1:RS, .packages=c("foreach"),.combine=rbind,.options.snow = opts) %dopar%{
      source("UC_MLE.R")
      estimate[j,]<-try(suppressWarnings(mle_UC(y=sample[j,],method=method)))
    }
  )
  
  # Salvando as estimativas de Monte Carlo
  output<-estimate[]
  output<-na.omit(output)
  
  output<-output[1:RR,]
  if(n<100){
    write(c("#---------------------------"),file=arquivo,append=TRUE,ncolumns=1)
    save(output,file=paste0("MLE_UC_n0",n,"_λ=",theta0[1],"_β=",theta0[2],".RData"))
    write(c("#n=",n,"λ=",theta0[1],"β=",theta0[2]),file=arquivo,append=TRUE, ncolumns=12)
    names_pars<-c("λ","β")
    # Cálculo estatísticas e tabela .tex
    SIM.COMP(coef=output,par=theta0,names.pars=names_pars,conf=c(0.90,0.95,0.99),nsign=c(0.01,0.05,0.1),
             arquivo=arquivo)
  }
  if(n>=100){
    write(c("#---------------------------"),file=arquivo,append=TRUE,ncolumns=1)
    save(output,file=paste0("MLE_UMOC_n",n,"_λ=",theta0[1],"_β=",theta0[2],".RData"))
    write(c("#n=",n,"λ=",theta0[1],"β=",theta0[2]),file=arquivo,append=TRUE, ncolumns=12)
    names_pars<-c("λ","β")
    # Cálculo estatísticas e tabela .tex
    SIM.COMP(coef=output,par=theta0,names.pars=names_pars,conf=c(0.90,0.95,0.99),nsign=c(0.01,0.05,0.1),
             arquivo=arquivo)
  }
  rm(theta0, estimate,output)
}

# Encerrando configuração de Paralelismo 
foreach::registerDoSEQ()
parallel::stopCluster(cl)
#### ---------------------------------------------------------------------- ####