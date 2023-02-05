#Manipulation simulation et estimation pour une série temporelle 
#Bruit blanc 
X=rnorm(10000)
plot.ts(X,col="red")
lag.plot(X,4,layout = c(2,2),do.lines = F)
lag.plot(X,4,c(2,2))
#simulation d'une série temporelle
n=1000
t=(1:n)/n
m=5*t+1
s=2*cos(2*pi*t*n/8)
X=m+s+rnorm(n,0,1)
X=ts(X,start = c(1997,3),frequency = 4)
plot.ts(X)
acf(x,24)

res=decompose(X,type = "additive")
plot(res)
res=stl(X,s.window = 8,t.degree = 1)
plot(res)

tendance=res$time.series[,"trend"]
saison=res$time.series[,"seasonal"]
eps=res$time.series[,"remainder"]

#pente10

n=1000
t=(1:n)/n
m=1+10*t
s=cos(n*2*pi*t/8)
X=m+s+rnorm(n,0,1)
X=ts(X,start=c(1997,3),frequency=4)

res=decompose(X,type = "additive")
plot(res)
res=stl(X,s.window = 8,t.degree = 1)
plot(res)

tendance=res$time.series[,"trend"]
saison=res$time.series[,"seasonal"]
eps=res$time.series[,"remainder"]

#filtrage d'une série temporelle ;filtre des différences

n=128
t=(1:n)/n
m=5*t+1
#s=2*cos(2*pi*t*n/8)+3*sin(2*pi*t/12)
s=2*cos(2*pi*t*n/8)
X=m+s+rnorm(n,0,1)
X=ts(X,start=c(1997,3),frequency=4)
plot.ts(X)
acf(X,24)

library(TSA)
per=periodogram(X)
freq=per$freq
spec=per$spec
dd=data.frame(spec,1/freq)
order=dd[order(-dd$spec),]

lag.plot(X,set.lags = c(1,2,4,6,8,12),layout=c(2,3),do.lines = F)
lag.plot(X,set.lags = c(1,2,4,6,8,12),layout=c(2,3),do.lines = T)


Y=diff(X,lag=1)
plot.ts(Y)
per=periodogram(Y)
freq=per$freq
spec=per$spec
dd=data.frame(spec,1/freq)
order=dd[order(-dd$spec),]

Z=diff(X,lag=8)
plot.ts(Z,col="red")
per=periodogram(Z)
freq=per$freq
spec=per$spec
dd=data.frame(spec,1/freq)
order=dd[order(-dd$spec),]
head(order,2)
acf(na.omit(Z))


n=1000
t=(1:n)/n
m=1+2*t+10*t^2
#s=2*cos(2*pi*t*n/8)+3*sin(2*pi*t/12)
s=2*cos(2*pi*t*n/8)
X=m+s+rnorm(n,0,1)
X=ts(X,start=c(1997,3),frequency=4)
plot.ts(X,col="green")
acf(X)


Z=diff(X,lag=1,differences = 2)
plot.ts(Z,col="green")
per=periodogram(Z)
freq=per$freq
spec=per$spec
dd=data.frame(spec,1/freq)
order=dd[order(-dd$spec),]
acf(na.omit(Z))

Z=diff(X,lag=8)
Y=diff(Z,lag=1,differences = 2)

#filtres de moyenne mobile 

n=1000
t=(1:n)/n
m=5*t+1
#s=2*cos(2*pi*t*n/8)+3*sin(2*pi*t/12)
s=2*cos(2*pi*t*n/8)
X=m+s+rnorm(n,0,1)
X=ts(X,start=c(1997,3),frequency=4)
plot.ts(X,col="orange")

X1=filter(X,filter = rep(1,8)/8)
plot.ts(X,col="orange")

YY=as.numeric(na.omit(X1))
XX=1:length(YY)
lm(YY~XX)

X1=filter(X,filter = rep(1,24))/24
plot.ts(X1,col="orange")


YY=as.numeric(na.omit(X1))
XX=1:length(YY)
lm(YY~XX)

X1=filter(X,sides = 2,method="convolution",filter = rep(1,8)/8)
plot.ts(X1,col="orange")
##########################################################################
##########################################################################
##########################################################################
#bruit blanc
eps=rnorm(1000)
r=acf(eps)
lag.plot(eps,2)
yn=eps[1:99]+eps[2:100]
ryn=acf(yn)
rho=ryn$acf[2]
lag.plot(yn,4)
#bruit blanc uniforme
epsu=runif(1000,-sqrt(3),sqrt(3))
r=acf(epsu)
lag.plot(epsu,2)
#variance histogramme et qqplot
var(epsu)
hist(epsu)
qqnorm(epsu)
qqline(epsu)

#test blancheur 
Box.test(eps)
Box.test(eps,lag=2)
Box.test(eps,type="Ljung-Box")
Box.test(yn,type="Ljung-Box")


pval_epsn_1=rep(0,10)
pval_yn_1=rep(0,10)
pval_epsn_2=rep(0,10)
pval_yn_2=rep(0,10)
for (k in 1:50)
{
  pval_epsn_1[k]=Box.test(eps, lag=k)$p.value
  pval_epsn_2[k]=Box.test(eps, lag=k, type="Ljung-Box")$p.value
  pval_yn_1[k]=Box.test(yn, lag=k)$p.value
  pval_yn_2[k]=Box.test(yn, lag=k, type="Ljung-Box")$p.value
}

# Exercice 2
T=1000
#bruit gaussien
eps=rnorm(T+1)
X=eps[2:(T+1)]-eps[1:T]/3
plot.ts(X)
lag.plot(X,lag=2)
r=acf(X)
tau=pacf(X)
# bruit uniforme
epsu=runif(T+1,-sqrt(3),sqrt(3))
Xu=epsu[2:(T+1)]-epsu[1:T]/3
plot.ts(Xu)
lag.plot(Xu,lag=2)
ru=acf(Xu)
tauu=pacf(Xu)

# Exercice 3
T=1000
T0=500
phi=c(0.9,-0.8)
p=length(phi)
#bruit gaussien
eps=rnorm(T+T0)
n=length(eps)
# valeurs initiales
init=rep(10,p)
x=init
for (i in (p+1):n)
{
  aux=eps[i]+sum(phi*x[(i-1):(i-p)])
  x=c(x,aux)
}
X=x[(T0+1):n]
plot.ts(X)
lag.plot(X,lag=2)
r=acf(X)
tau=pacf(X)

# bruit uniforme
epsu=runif(T+T0,-sqrt(3),sqrt(3))
xu=init
for (i in (p+1):n)
{
  aux=eps[i]+sum(phi*xu[(i-1):(i-p)])
  xu=c(xu,aux)
}
Xu=xu[(T0+1):n]
plot.ts(Xu)
lag.plot(Xu,lag=2)
ru=acf(Xu)
tauu=pacf(Xu)

# Exercice 4
set.seed(219)
X1=arima.sim(n=200,list(ma=c(-0.3,0.6)),sd=1)
ARMAacf(ma=c(-0.3,0.6),lag.max=20)
ARMAacf(ar=c(0.9),lag.max=20,pacf=TRUE)
ARMAtoMA(ar=c(0.9),ma=c(-0.3,0.6),lag.max=20)
# Xt=epst-1/3*eps_{t-1}
X2=arima.sim(n=1000,list(ma=c(-1/3)),sd=1)
plot.ts(X2)
lag.plot(X2,lag=2)
r2=acf(X2)
r2
ARMAacf(ma=c(-1/3),lag.max=20)
tau2=pacf(X2)
tau2
ARMAacf(ma=c(-1/3),lag.max=20,pacf=TRUE)
#Xt=0.9X_{t-1}-0.8X_{t-2}+epst
X3=arima.sim(n=1000,list(ar=c(0.9,-0.8)),sd=1)
plot.ts(X3)
lag.plot(X3,lag=2)
r3=acf(X3)
r3
ARMAacf(ar=c(0.9,-0.8),lag.max=20)
tau3=pacf(X3)
tau3
ARMAacf(ar=c(0.9,-0.8),lag.max=20,pacf=TRUE)
# Xt-0.9X_{t-1}=epst-0.3eps_{t-1}+0.6eps_{t-2}
X4=arima.sim(n=1000,list(ma=c(-0.3,0.6),ar=(0.9)),sd=1)
plot.ts(X4)
lag.plot(X4,lag=2)
r4=acf(X4)
r4
ARMAacf(ar=c(0.9),ma=c(-0.3,0.6),lag.max=20)
tau4=pacf(X4)
tau4
r4
ARMAacf(ar=c(0.9),ma=c(-0.3,0.6),lag.max=20)
tau4=pacf(X4)
####################################################################
####################################################################
# Validation d'un modèle ARMA


rm(list=ls())


library(datasets)
library(forecast)

# Exo 1
# Question 1-2
X=AirPassengers
summary(X)
plot.ts(X)
monthplot(X)

# Question 3
# 3a)
N=length(X)

#time=matrix(1:N,ncol=1,nrow=N)

t=time(X)
model=lm(X~t)

a=model$coefficient[2]
b=model$coefficient[1]

plot(as.numeric(t),as.numeric(X),type='l')
lines(as.numeric(t),as.numeric(t)*a+b,col="red")

# 3b)
Z=as.ts(X-b-a*t)
mean(Z)
plot(Z)

# 3c)
acf(Z)
pacf(Z)
model0=auto.arima(Z)

# > model0
# Series: Z 
# ARIMA(2,1,1)(0,1,0)[12] 

# Coefficients:
# ar1     ar2      ma1
# 0.5960  0.2143  -0.9819
# s.e.  0.0888  0.0880   0.0292

# sigma^2 estimated as 132.3:  log likelihood=-504.92
# AIC=1017.85   AICc=1018.17   BIC=1029.35

eps0=model0$residuals
Box.test(eps0,lag=20)

# > Box.test(eps0,lag=20)

# Box-Pierce test

# data:  eps0
# X-squared = 20.149, df = 20, p-value = 0.4487


# Question 4
# 4a)

X_ds=diff(X,lag=12,diff=1)
X_dt=diff(X_ds,lag=1,diff=2)
Y=X_dt
plot.ts(Y)
mean(Y)

# 3b)
acf(Y)
pacf(Y)
model1=auto.arima(Y)

# > model1
# Series: Y 
# ARIMA(5,0,0) with zero mean 

# Coefficients:
# ar1      ar2      ar3      ar4      ar5
# -1.0906  -0.8005  -0.6538  -0.5638  -0.2573
# s.e.   0.0845   0.1184   0.1240   0.1178   0.0858

# sigma^2 estimated as 167.8:  log likelihood=-515.72
# AIC=1043.44   AICc=1044.12   BIC=1060.64

eps1=model1$residuals
Box.test(eps1,lag=20)

# > Box.test(eps1,lag=20)

# Box-Pierce test

# data:  eps1
# X-squared = 22.119, df = 20, p-value = 0.3341

# Question 4
# decompose does not work. it is better to do it with the moving average filters, commande filter

X_add<-decompose(X,type="additive")
plot(X_add)
eps_add=X_add$random
model2=auto.arima(eps_add)
eps2=model2$residuals
Box.test(eps2,lag=20)

# > model2
# Series: eps_add 
# ARIMA(2,0,2)(1,0,0)[12] with zero mean 

# Coefficients:
# ar1      ar2     ma1     ma2    sar1
# 0.1187  -0.3332  0.3382  0.6494  0.8110
# s.e.  0.2058   0.1717  0.1789  0.1164  0.0584

# sigma^2 estimated as 87.96:  log likelihood=-487.11
# AIC=986.22   AICc=986.89   BIC=1003.51

# > Box.test(eps2,lag=20)

# Box-Pierce test

# data:  eps2
# X-squared = 23.004, df = 20, p-value = 0.2886

acf(na.omit(X_add$random))



X_mult<-decompose(X,type="multiplicative")
plot(X_mult)
eps_mult=X_mult$random
model3=auto.arima(eps_mult)
eps3=model3$residuals
Box.test(eps3,lag=20)

# > model3
# Series: eps_mult 
# ARIMA(3,0,1)(1,0,0)[12] with non-zero mean 

# Coefficients:
# ar1      ar2      ar3      ma1    sar1    mean
# 1.0963  -0.2262  -0.2105  -0.8597  0.1786  0.9988
# s.e.  0.1108   0.1330   0.0948   0.0835  0.0998  0.0012

# sigma^2 estimated as 0.0007765:  log likelihood=287.67
# AIC=-561.35   AICc=-560.44   BIC=-541.17

# > Box.test(eps3,lag=20)

# Box-Pierce test

# data:  eps3
# X-squared = 12.441, df = 20, p-value = 0.9001

acf(na.omit(X_mult$random))
# non-stationnaire

# Question 5.

# with filters:
# desaisoning with the moving average
X_ds=filter(X, rep(1/12,12), method = "convolution")
# detrending with differencing 
Y=diff(X_ds[6:(N-6)], lag=1,difference=2)

acf(na.omit(Y))
pacf(na.omit(Y))

model4=auto.arima(Y)
# > model4
# Series: Y 
# ARIMA(2,0,1) with zero mean 

# Coefficients:
# ar1     ar2      ma1
# 0.5960  0.2143  -0.9819
# s.e.  0.0888  0.0880   0.0292

# sigma^2 estimated as 0.9191:  log likelihood=-179.4
# AIC=366.8   AICc=367.12   BIC=378.3
eps4=model4$residuals
Box.test(eps4,lag=20)

# > Box.test(eps4,lag=20)

# Box-Pierce test

# data:  eps4
# X-squared = 18.159, df = 20, p-value = 0.5769
acf(eps4)


# Question 7. 
library(forecast)
model<-auto.arima(X)
plot(forecast(model,h=20))


library(forecast)
library(seasonal)

# Exo 2

temp=read.table("monthly_csv.csv",sep=',',header=TRUE)
n=length(temp[,1])

data=temp[temp$Source=='GISTEMP',2:3];
n=length(data[,1])

Y=data[order(data$Date),]
Yseries=data.frame(Y,row.names=NULL)
X=ts(Yseries$Mean[(30*12+1):n],start=c(1910,1),end=c(2012,12),frequency=12)

XX=ts(Yseries$Mean[30*12+1:n],start=c(1910,1),end=c(1950,12),frequency=12)

plot.ts(X)

monthplot(X)
boxplot(X~cycle(X),xlab="month")

lag.plot(X,4)
lag.plot(X,12)


# decompose
res1=decompose(X,type="additive")
plot(res1)

tendance1=res1$trend
saison1=res1$seasonal
eps1=res1$random

monthplot(eps1)
boxplot(eps1~cycle(eps1),xlab="month")

lag.plot(eps1,12)
lag.plot(saison1,12)
lag.plot(tendance1,12)

# auto.arima
model1=auto.arima(eps1)

# > model1
# Series: eps1 
# ARIMA(3,0,0)(1,0,0)[12] with non-zero mean 

# Coefficients:
# ar1     ar2      ar3     sar1    mean
# 0.1886  0.0044  -0.1659  -0.0001  0.0000
# s.e.  0.0282  0.0288   0.0283   0.0289  0.0027

# sigma^2 estimated as 0.008378:  log likelihood=1192.32
# AIC=-2372.63   AICc=-2372.56   BIC=-2341.97

res1=model1$residuals
Box.test(res1,lag=20)
# > Box.test(res1,lag=20)

# Box-Pierce test

# data:  res1
# X-squared = 103.75, df = 20, p-value = 2.669e-13


# STL

res2 = stl(X,s.window=12,s.degree=0)
plot(res2)
tendance2=res2$time.series[,"trend"]
saison2=res2$time.series[,"seasonal"]
eps2=res2$time.series[,"remainder"]

model2=auto.arima(eps2)

# > model2
# Series: eps2 
# ARIMA(1,0,2)(2,0,0)[12] with non-zero mean 

# Coefficients:
# ar1     ma1     ma2     sar1     sar2     mean
# -0.6676  0.8252  0.1550  -0.2956  -0.1685  -0.0001
# s.e.   0.2903  0.2865  0.0364   0.0282   0.0282   0.0018

# sigma^2 estimated as 0.006045:  log likelihood=1405.53
# AIC=-2797.06   AICc=-2796.97   BIC=-2761.22

res2=model2$residuals
Box.test(res2,lag=20)

# > Box.test(res2,lag=20)

# Box-Pierce test

# data:  res2
# X-squared = 70.857, df = 20, p-value = 1.319e-07

# X11 does not work
library(seasonal)
res3 = seas(X)


model0=auto.arima(X)
# > model0
# Series: X 
# ARIMA(1,1,2) with drift 

# Coefficients:
# ar1      ma1     ma2  drift
# 0.8464  -1.3713  0.3836  8e-04
# s.e.  0.0289   0.0446  0.0418  3e-04

# sigma^2 estimated as 0.01155:  log likelihood=1003.67
# AIC=-1997.35   AICc=-1997.3   BIC=-1971.75

res0=model0$residuals
Box.test(res0,lag=20)

# > Box.test(res0,lag=20)

# Box-Pierce test

# data:  res0
# X-squared = 19.991, df = 20, p-value = 0.4585



library(forecast)
model=auto.arima(X)
plot(forecast(model,h=24))
prevision=forecast(model,h=24)
NN=dim(Y)[1]
X_true=Y[(NN-47):(NN-24),]
plot(X_true$Mean,type="l",ylim=c(0.5,0.9))
lines(as.numeric(prevision$mean),col="blue")

# Exo 3. 

path="~/Documents/Work/Teaching/Poitiers/Time_Series/data"
setwd(path)

X=read.table("gnpus.csv",sep=',')
n=length(X[,1])

X <- ts(X, frequency=4, start=c(1947,2))
plot(X)

Box.test(X,lag=20)

acf(X)
pacf(X)

Y=diff(X,lag=4,diff=1)


model=auto.arima(X)

model1=arima(X,order=c(1,0,0),seasonal=list(order=c(0,1,0),period=4))

model2=arima(X,order=c(1,0,0))

eps1=model1$residuals
Box.test(eps1)

eps2=model2$residuals
Box.test(eps2)

model3=ar(X)
Box.test(model3$resid)

###################################
#Prévision

# Exercice 1
# Question 1
Y=EuStockMarkets
plot.ts(Y)
X<-EuStockMarkets[,"CAC"]
plot.ts(X)
# Question 2
x=window(X,end=c(1997,260))
plot.ts(x)
# Questions 3 et 4
xsmooth=HoltWinters(x,alpha=NULL,beta=FALSE,gamma=FALSE)
xpred<-predict(xsmooth,n.ahead=1)
plot(xsmooth,xpred,main="prediction")
xsmooth$alpha
# Recherche jusqu'où on peut aller avec erreur inférieure à 100 points
xpred <- predict(xsmooth,n.ahead=length(X)-length(x))
erreur=window(X,start=c(1998,1))-xpred
plot(erreur)
erreur<100 # 5 pas
# Question 5
xsmooth<-HoltWinters(x,gamma=FALSE)
xpred<-predict(xsmooth,n.ahead=1)
plot(xsmooth,xpred,main="prediction")
# Recherche jusqu'où on peut aller avec erreur inférieure à 100 points
xpred <- predict(xsmooth,n.ahead=length(X)-length(x))
erreur=window(X,start=c(1998,1))-xpred
plot(erreur)
erreur<100 # 5 pas

# Exercice 2
# Questions 1 à 3
X=arima.sim(n=105,list(ar=c(1,-1/2,1/3)),sd=1)
x=window(X,end=100)
model=arima(x,order=c(3,0,0))
pred<-predict(model,n.ahead=5)$pred
erreur=X[101:105]-pred

# Question 4
erreur=array(rep(0,50*5),c(50,5))
for (i in 1:50)
{
  X=arima.sim(n=105,list(ar=c(1,-1/2,1/3)),sd=1)
  x=window(X,end=100)
  model=arima(x,order=c(3,0,0))
  pred<-predict(model,n.ahead=5)$pred
  erreur[i,]=X[101:105]-pred
}
biais=colMeans(erreur)
variance=colMeans(erreur^2)
# Question 5
for (i in 1:50)
{
  X=arima.sim(n=505,list(ar=c(1,-1/2,1/3)),sd=1)
  x=window(X,end=500)
  model=arima(x,order=c(3,0,0))
  pred<-predict(model,n.ahead=5)$pred
  erreur[i,]=X[501:505]-pred
}
biais=colMeans(erreur)
variance=colMeans(erreur^2)

# Exercice 3
Y=scan('sanfran.dat',skip=1)
Yts<-ts(Y,start=c(1932,1),frequency=12)
plot.ts(Yts)
acf(Yts)
# saisonnalité de période 12
# avec filtre différence
Z=diff(Yts,lag=12,differences=1)
Zextrait <- window(Z,start=c(1933,1),end=c(1963,12))
par(mfrow=c(1,1))
plot.ts(Zextrait)
par(mfrow=c(2,1))
acf(Zextrait,na.action=na.pass) # suggère d'utiliser un MA(12)
pacf(Zextrait,na.action=na.pass) # suggère d'utiliser un AR(12) ou AR(24)
# modèle AR
model_AR=arima(Zextrait,order=c(12,0,0))
model_AR2=arima(Zextrait,order=c(24,0,0))
acf(model_AR$residuals)
Box.test(model_AR$residuals,lag=20)
acf(model_AR2$residuals)
Box.test(model_AR2$residuals,lag=20)
predAR=predict(model_AR,n.ahead=length(Z)-length(Zextrait))
par(mfrow=c(1,1))
plot.ts(Z,xlim=c(1963,1967))
lines(predAR$pred,col=3)
predAR2=predict(model_AR2,n.ahead=length(Z)-length(Zextrait))
lines(predAR2$pred,col=6)
model_MA=arima(Z,order=c(0,0,12))
acf(model_MA$residuals)
Box.test(model_MA$residuals,lag=20)
erreur1=window(Z,start=c(1964,1))-predAR$pred
erreur2=window(Z,start=c(1964,1))-predAR2$pred
plot(erreur1)
lines(erreur2,col=3)
# modèle MA
model_MA=arima(Zextrait,order=c(0,0,12))
acf(model_MA$residuals)
Box.test(model_MA$residuals,lag=20)
predMA=predict(model_MA,n.ahead=length(Z)-length(Zextrait))
par(mfrow=c(1,1))
plot.ts(Z,xlim=c(1963,1967))
lines(predMA$pred,col=3)
erreur3=window(Z,start=c(1964,1))-predMA$pred
plot(erreur1)
lines(erreur2,col=3)
lines(erreur3,col=6)
# Modèle SARIMA
Ytsextrait=window(Yts,start=c(1933,1),end=c(1963,12))
model_SARIMA=arima(Ytsextrait,order=c(2,0,0),seasonal=list(order=c(2,0,0),period=12))
acf(model_SARIMA$residuals)
Box.test(model_SARIMA$residuals,lag=20)
predSARIMA=predict(model_SARIMA,n.ahead=length(window(Yts,start=c(1933,1)))-length(Ytsextrait))
par(mfrow=c(1,1))
plot.ts(Yts,xlim=c(1963,1967))
lines(predSARIMA$pred,col=3)
erreur4=window(Yts,start=c(1964,1))-predSARIMA$pred
plot(erreur4)