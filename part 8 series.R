library(tidyverse)
library(fpp2)
train<-window(goog,end=900)
test<-window(goog,start=901)

train1<-window(qcement,end=c(2012,4))
test1<-window(qcement,start=c(2013,1))

ses <- ses(train,alpha = .2,h = 100)
autoplot(ses)

ses1 <- ses(train1,alpha = .2,h = 100)
autoplot(ses1)

#ON supprime la tendandne  dans les données training 
train.dif<-diff(train)
autoplot(train.dif,col="red")

train1.dif<-diff(train1)
autoplot(train.dif,col="red")

ses2 <- ses(train.dif,alpha = .2,h = 100)
autoplot(ses2)

ses3 <- ses(train1.dif,alpha = .2,h = 100)
autoplot(ses3)

#on suprimer la tendance dans les données tresing 

test1.dif<-diff(test)
accuracy(ses,test1.dif)
#########################################
#La méthode de holt
#########################################

holt1<-holt(train,h=100)
autoplot(holt1)

holt2<-holt(train1,h=100)
autoplot(holt2)

holt1$model

autoplot(decompose(train1))

hW1<-ets(train1,model="AAA")
autoplot(forecast(hW1))

f1<-forecast(hw1,h=5)

##########################################
##########################################
plot(USAccDeaths,col="red")
monthplot(USAccDeaths)
X<-window(USAccDeaths,start=1973,end=c(1977,12))
Y<-window(USAccDeaths,start=1978)
fitles<-ets(X,model="ANN")
autoplot(fitles)
summary(fitles)

predles<-forecast(fitles,h=12)
plot(predles)
points(Y,type="l",col="green",lwd=2)

#lissage exponentiel double
fitled<-ets(X,model = "AAA")
summary(fitled)
predled<-forecast(fitled,h=12)
plot(predled)
points(X,type="l",col="green",lwd=2)

#méthode de holt-winters

fitHW<-ets(X,model = "AAA")
predHW<-forecast(fitHW,h=12)
plot(predHW)
points(Y,type = "l",col="green",lwd=2)

#choix d'un modèle 

fit<-ets(X)
summary(fit)

predfit<-forecast(fit,h=12)
plot(predfit)
points(Y,type="l",col="green",lwd=2)

fitAIC <- ets(X,ic="aic")
summary(fitAIC)

predfitAIC <- forecast(fitAIC,h=12)
plot(predfitAIC)
points(Y,type='l',col='darkgreen',lwd=2)





















