require("caschrono")
library(forcats)
library(astsa)
#processus AR(2)
Ar2<-arima.sim(list(ar=c(1.5,-.75)),n=400)
plot(Ar2,col='red')
#on calcule les racie 
D <- 1.5^2-4*0.75
r1 <- (1.5+sqrt(-D)*1i)/(2*0.75) 
abs(r1)
acf(Ar2,10)
pacf(Ar2,10)
#AR(4)
ARt<- arima.sim(list(ar=c(0.3,-0.75,0.5,-0.7)),n=1000)
acf(ARt,10)
pacf(ARt,10)
#ARMA
ARMA22 <- arima.sim(list(ar = c(0.9, -0.5), ma =c(-0.23,0.9)), n=1000)
plot(ARMA22)
acf(ARMA22,10)
pacf(ARMA22,10)   
#auto arima
auto.arima(ARMA22)
res<-auto.arima(ARMA22)$residuals
acf(res,10)
pacf(res,10)
Box.test(res)
#########################
#########################
help("oil")
plot(oil,col="red")
#calcul log-rendements
r.oil<-diff(log(oil))
plot(r.oil,col='green')
acf(r.oil)
pacf(r.oil)
#Modélisation avec un ARMA
auto.arima(r.oil,stationary =T,seasonal = FALSE)
#etude des résidus de la modélisation
Resoil<-auto.arima(r.oil,stationary = F,seasonal = F)$residuals
acf(Resoil)
pacf(Resoil)
Box.test(Resoil)
###########################
###########################
DLalgo <- function(x,p){
  ## on nomme la fonction d'autocovariance empirique
  gamma <- function(h){acf(x,h,plot=F,type="covariance")$acf[h+1]}
  
  ## initiatilisation
  k <- 1
  kappa = gamma(1)/gamma(0)
  sigma2 = gamma(0)*(1-kappa^2)
  phi <- 1:p ; phi[1] <- kappa
  
  ## algorithme
  while (k <= p){
    
    ### calcul annexe de la somme
    S = 0 
    for (j in 1:k){ S <- S + phi[j] * gamma(k-j+1) }
    
    ### kappa 
    kappa <- (gamma(k+1)-S)/sigma2
    
    ### phi
    for (j in 1:k){ phi[j] <- phi[j] - kappa * phi[k-j+1] }
    phi[k+1] <-kappa
    
    ### sigma2
    sigma2 <- sigma2 * (1-kappa^2)
    
    ### on incremente la boucle
    k <- k+1
  }
  ## resultats
  Result = list(phi,sigma2)
  return(Result)
}
DLalgo(Ar2,1)


