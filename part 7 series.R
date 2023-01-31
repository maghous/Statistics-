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

