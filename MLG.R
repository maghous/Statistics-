library(tidyverse)
library(corrplot)
library(leaps)
dat<-read.csv("bodyfat.csv")
attach(dat)
names(dat)
summary(dat)
coorelatio<-cor(dat)
corrplot(coorelatio,method="circle")
corrplot(coorelatio,method="pie")
corrplot(coorelatio,method="color")
corrplot(coorelatio,method="number")

modele <- lm(BodyFat~.,data=dat)
summary(modele)
coef=modele$coefficients
mod_AIC <- step(modele)

model_vide <-lm(BodyFat ~1,data=dat)
mod_sel_forward <- step(model_vide,direction ="forward",scope=list(upper=modele))
mod_sel_stepwise <- step(model_vide,direction="both",, scope=list(upper=modele))


mod_BIC <- step(modele,k=log(nrow(dat)),trace=FALSE)
summary(mod_BIC)
Mod_sel_forward_BIC <- step(model_vide, direction="forward", scope=list(upper=modele), k=log(nrow(dat_body)), trace=FALSE)
summary(Mod_sel_forward_BIC)
Mod_sel_stepwise_BIC = step(model_vide, direction="both", scope=list(upper=modele), k=log(nrow(dat_body)), trace=FALSE)
summary(Mod_sel_stepwise_BIC)

print(sort(mod_AIC$coefficients))
print(sort(mod_sel_forward$coefficients))
print(sort(mod_sel_stepwise$coefficients))

print(sort(mod_BIC$coefficients))
print(sort(Mod_sel_forward_BIC$coefficients))
print(sort(Mod_sel_stepwise_BIC$coefficients))

print(mod_BIC$coefficients)
new_dat<-data.frame(Age=28,Weight=86,Height=183,Neck=39.8, Chest=96.2, Abdomen=100.4, Hip=101.5, 
                    Thigh=63.1, Knee=42.8, Ankle=23, Biceps=28.3, Forearm=26.8, Wrist=17.9)
predict(mod_BIC,newdata = new_dat)


#prediction du taux d'azote dans une rivière 
dat1 <- read.csv("Azote.csv")
attach(dat1)
names(dat1)
summary(dat1)
coorelatio1<-cor(dat1)
corrplot(coorelatio1,method="circle")
corrplot(coorelatio1,method="pie")
corrplot(coorelatio1,method="color")
corrplot(coorelatio1,method="number")

modele<-lm(Azote~.,data=dat1)
summary(modele)
modele$coefficients
mod_AIC<-step(modele)
new_modele <-lm(Azote~ComInd + Foret,data=dat1)
new_modele$coefficients

noms<-names(dat1)[1:4]
L = leaps(x=dat1[, 1:4], y=dat1[, 5], names=noms, method="Cp")
print(L)
noms[L$which[L$Cp == min(L$Cp)]]


L = leaps(x=dat1[, 1:4], y=dat1[, 5], names=noms, method="r2")
print(L)
noms[L$which[L$r2 == min(L$r2)]]

L = leaps(x=dat1[, 1:4], y=dat1[, 5], names=noms, method="adjr2")
print(L)
noms[L$which[L$adjr2 == min(L$adjr2)]]

temp_train <-read.csv("Temperatures_train.csv",stringsAsFactors = T)
attach(temp_train)
temp_test <- read.csv("Temperatures_test.csv")
attach(temp_test)

modele_complet <- lm(Vienne~.,data=temp_train[,-1])
summary(modele_complet)
prediction_modele_complet = predict(modele_complet, newdata =temp_test)
emq = mean((prediction_modele_complet - temp_test$Vienne)^2)
print(emq)

modele_vide <- lm(Vienne~1,data=temp_train[,-1])
modele_selec_AIC = step(modele_vide, scope=list(upper=modele_complet), direction="forward")

prediction_modele_selec_AIC <- predict(modele_selec_AIC, newdata =temp_test[,-1])
emq = mean((prediction_modele_selec_AIC - temp_test$Vienne)^2)
print(paste("Erreur moyenne quadratique",round(emq,5)))
summary(modele_selec_AIC)


modele_selec_BIC = step(modele_vide, scope=list(upper=modele_complet), direction="forward", k=log(nrow(temp_test[,-1])))
prediction_modele_selec_BIC <-predict(modele_selec_BIC,, newdata =temp_test[,-1])
      emq=mean((prediction_modele_selec_BIC - temp_test$Vienne)^2)
print(paste("Erreur moyenne quadratique:",round(emq,5)))      
