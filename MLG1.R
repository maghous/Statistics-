#regL simple
X = c (1.1 , 1.6 , 1.9 , 1.4 , 1.7 , 1.2 , 1.6 , 1.8 , 2.3)
Y = c (3 , 2.96 , 5.28 , 3.72 , 3.76 , 2.91 , 4.34 , 4.28 , 5.71)
plot(X,Y,lwd=2,col="red")

print(paste("Moyenne de X",round(mean(X),4)))
print(paste("Moyenne de Y",round(mean(Y),4)))
print(paste("Cov non corrigée  de X et Y",round(cov(X,Y)*8/9,4)))
print(paste("Var non corrigée  de X ",round(var(X)*8/9,4)))


a=cov(X,Y)/var(X)
b=mean(Y) - a *mean(X)
print(paste("Estimation de a",round(a,4)))
print(paste("Estimation de b",round(b,4)))

plot(X,Y,lwd=2,col="red")
abline(b,a)

modele <- lm(Y~X)
modele$coefficients
confint(modele,level = 0.99)

residu <- Y - (a*X +b)
round(residu,3)
round(modele$residuals,3)

sigma<-sqrt(sum(residu^2)/7)
print(paste("Estimation de sigma:",round(sigma,4)))
summary(modele)

r2<- 1 - mean(residu^2)/(var(Y)*8/9)
print(paste('calcul de r2',round(r2,4)))
modele$df.residual
summary(modele)

print(a*1.5 + b)
print(predict(modele,newdata=list(X=1.5)))

prediction <- predict(modele,newdata=list(X=1.5),interval = "prediction",level=0.99)
print(paste("Intervalle de prédiction: [", round(prediction[2],4), ", ", round(prediction[3],4), "]", sep=""))


prediction <- predict(modele,newdata=list(X=1.5),interval = "confidence",level=0.99)
print(paste("Intervalle de prédiction: [", round(prediction[2],4), ", ", round(prediction[3],4), "]", sep=""))


df<-read.csv("Frog.csv",stringsAsFactors = T)
attach(df)
names(df)
summary(df)
pipo <-cor(df[,c(-1,-2)])
library(corrplot)
corrplot(pipo,"shade")
corrplot(pipo,"number")
corrplot(pipo)
plot(df$SVL_mm,df$ED_mm,xlab="Longeur",ylab="Diamètre",col="black")
modole <- lm(ED_mm ~ SVL_mm,data=df)
coef=modole$coefficients
lines(df$SVL_mm,df$SVL_mm*coef[2] + coef[1],col="red")

df$Mass_sqcb <- df$Mass_g^(1/3)
plot(df$Mass_sqcb,df$ED_mm,xlab="mass^(1/3)",ylab="Diamètre")
modole1<-lm(df$ED_mm ~df$Mass_sqcb,data=df)
coeff <- modole1$coefficients
lines(df$Mass_sqcb,df$Mass_sqcb*coeff[2] + coeff[1],col="red")


modele_frog <-lm(df$CD_mm~df$ED_mm,data=df)
prediction <- predict(modele_frog,newdata =list(ED_mm=10), interval="prediction")
print(paste("Estimation du diamètre de cornée",round(prediction[1],4)))
print(paste("Intervalle de prédiction: [", prediction[2], ", ", prediction[3], "]", sep=""))

library(tidyverse)
df1<- read.csv("Fish.csv",stringsAsFactors = T)
plot(df1$Height^3,df1$Weight,xlab = "Longeur",ylab="Poids",lwd=2)
table(df1$Species)

plot(df1$Height^3,df1$Weight,pch=as.numeric(df1$Species), col=as.numeric(df1$Species),
     xlab="Longeur au cube",ylab="Poids")
legend("topright",legend=levels(df1$Species),pch=1:7,col=1:7,lty = c(1, 2),lwd = 1)


df1$type = 0*(df1$Species=="Pike") + 1*(df1$Species=="Perch" | df1$Species=="Whitefish" | df1$Species=="Roach") + 2*(df1$Species=="Bream" | df1$Species=="Parkki" | df1$Species=="Smelt")

par(mfrow=c(2,2), fin=c(6,6))

df1$Height_Cube = df1$Height^3
plot(df1$Height_Cube[df1$type==0], df1$Weight[df1$type==0], main="Groupe 1"
     ,xlab="Longueur au cube", ylab="Poids")
coeff0 = lm(df1$Weight[df1$type==0]~df1$Height_Cube[df1$type==0])$coefficients
lines(df1$Height_Cube[df1$type==0], df1$Height_Cube[df1$type==0]*coeff0[2] + coeff0[1], col="red")

df1$Height_Cube = df1$Height^3
plot(df1$Height_Cube[df1$type==1], df1$Weight[df1$type==1], main="Groupe 2"
     ,xlab="Longueur au cube", ylab="Poids")
coeff1 = lm(df1$Weight[df1$type==1]~df1$Height_Cube[df1$type==1])$coefficients
lines(df1$Height_Cube[df1$type==1], df1$Height_Cube[df1$type==1]*coeff1[2] + coeff1[1], col="red")

df1$Height_Cube = df1$Height^3
plot(df1$Height_Cube[df1$type==2], df1$Weight[df1$type==2], main="Groupe 3"
     ,xlab="Longueur au cube", ylab="Poids")
coeff2 = lm(df1$Weight[df1$type==2]~df1$Height_Cube[df1$type==2])$coefficients
lines(df1$Height_Cube[df1$type==2], df1$Height_Cube[df1$type==2]*coeff2[2] + coeff2[1], col="red")

plot(df1$Height^3, df1$Weight, pch=as.numeric(df1$Species), col=as.numeric(df1$Species)
     ,main="Tous les poissons", xlab="Longueur au cube", ylab="Poids")
legend("topright", legend = levels(Fish$Species), pch=1:7, col=1:7)
abline(coeff0[1], coeff0[2], col="red")
abline(coeff1[1], coeff1[2], col="red")
abline(coeff2[1], coeff2[2], col="red")

df2<-read.csv('Frog.csv',stringsAsFactors = T)
modele_frog <-lm(CD_mm~ED_mm,data=df2)
summary(modele_frog) #on rejette l'hypothèse nul 

x = seq(0,20,0.01)
intC = predict(modele_frog, newdata=list(ED_mm=x), interval="confidence")
intP = predict(modele_frog, newdata=list(ED_mm=x), interval="prediction")
plot(df2$ED_mm, df2$CD_mm, xlab="Diamètre de l'oeil", ylab="Diamètre de cornée")
coeff = modele_frog$coefficients
abline(coeff[1], coeff[2], col="blue")
lines(x, intC[, 2], col="red")
lines(x, intC[, 3], col="red")
lines(x, intP[, 2], col="green")
lines(x, intP[, 3], col="green")
legend(2, 14, legend=c("Régression", "Intervalle de confiance", "Intervalle de prédiction")
       ,col=c("blue", "red", "green"), lty=1)


X1 = c(0.6, 0.4, 1.0, 0.2, 0.6, 0.3, 0.6, 0.2, 0.3, 0.7, 0.9)
X2 = c(0.9, 0.1, 0.6, 0.6, 0.7, 0.6, 0.1, 0.5, 1.0, 0.8, 0.6)
X3 = c(0.3, 0.7, 0.6, 0.5, 0.5, 0.4, 0.4, 0.0, 0.1, 1.0, 0.3)
Y = c(-1.30, -0.80, -2.73, -0.65, -1.23, -1.00, -1.56, 0.80, -0.98, -1.03, -2.41)

X=matrix(data=c(X1,X2,X3,rep(1,11)),ncol=4,byrow = F)
coeff<-solve(t(X) %*% X,t(X)%*% Y)
modele<-lm(Y~X1 + X2 + X3)
t(coeff)
modele$coefficients

plot(X1, Y,lwd=2,col="red")
abline(coeff[2]*mean(X2)+ coeff[3]*mean(X3) + coeff[4], coeff[1])

plot(X2, Y,lwd=2,col="red")
abline(coeff[1]*mean(X1)+ coeff[3]*mean(X3) + coeff[4], coeff[2])

plot(X3, Y,lwd=2,col="red")
abline(coeff[1]*mean(X1)+ coeff[2]*mean(X2) + coeff[4], coeff[3])

residu <- Y - X %*% coeff
round(t(residu),4)
round(modele$residuals,4)

sigma <- sqrt(sum(residu)/7)
print(paste("Estimation de sigma",round(sigma,10)))
summary(modele)

r2 <- 1 - mean(residu^2)/var(Y)*(10/11)
print(paste("coefficient de corrélation",round(r2,5)))

print(paste("Prédiction:", 0.4*coeff[1]+0.9*coeff[2]+0.2*coeff[3]+coeff[4]))
print(predict(modele, newdata=list(X1=0.4, X2=0.9, X3=0.2)))

IP = predict(modele, newdata=list(X1=0.4, X2=0.9, X3=0.2), interval="prediction", level=0.99)
IC = predict(modele, newdata=list(X1=0.4, X2=0.9, X3=0.2), interval="confidence", level=0.99)
print(paste("Intervalle de prédiction de niveau 99%: [", IP[2], ", ", IP[3], "]", sep=""))
print(paste("Intervalle de confiance de niveau 99%: [", IC[2], ", ", IC[3], "]", sep=""))


Femur = read.csv("Femur.csv", stringsAsFactors=T)
plot(Femur$Longueur, Femur$Circonference, xlab="Longueur du fémur", ylab="Circonférence du fémur",lwd=2)
modele_lin = lm(Circonference~Longueur, dat=Femur)
abline(modele_lin$coefficients, col="red")

Femur$Longueur_carré = Femur$Longueur^2
plot(Femur$Longueur, Femur$Circonference, xlab="Longueur du fémur", ylab="Circonférence du fémur",lwd=2)
modele_quad = lm(Circonference~Longueur_carré + Longueur, dat=Femur)
x = seq(min(Femur$Longueur), max(Femur$Longueur), 1)
lines(x, modele_quad$coefficients[1] + modele_quad$coefficients[2]*x^2 + modele_quad$coefficients[3]*x, col="red")


plot(Femur$Longueur, Femur$Circonference, xlab="Longueur du fémur", ylab="Circonférence du fémur",lwd=2)
modele_quad = lm(Circonference~poly(Longueur, 2, raw=TRUE), dat=Femur)
x = seq(min(Femur$Longueur), max(Femur$Longueur), 1)
lines(x, modele_quad$coefficients[1] + modele_quad$coefficients[2]*x + modele_quad$coefficients[3]*x^2, col="red")


Temp = read.csv("Températures.csv")
plot(Vienne~Indre, dat=Temp, xlab="Températures de l'Indre", ylab="Température de la Vienne",lwd=2)
plot(Vienne~Alpes_Maritimes, dat=Temp, xlab="Températures des Alpes Maritimes", ylab="Température de la Vienne",lwd=2)


summary(lm(Vienne~Indre, dat=Temp))
summary(lm(Vienne~Alpes_Maritimes, dat=Temp))

modele_multi = lm(Vienne~Indre + Charente + Indre_Et_Loire + Maine_Et_Loire + Haute_Vienne + Deux_Sevres, dat=Temp)
summary(modele_multi)

dat_train = Temp[1:974,]
dat_test = Temp[975:1461,]

modele1 = lm(Vienne~Indre, dat=dat_train)
score1 = mean((predict(modele1, newdata = dat_test) - dat_test$Vienne)^2)
print(paste("Erreur quadratique moyenne:", score1))

modele2 = lm(Vienne~Indre + Charente + Indre_Et_Loire + Maine_Et_Loire + Deux_Sevres, dat=dat_train)
score2 = mean((predict(modele2, newdata = dat_test) - dat_test$Vienne)^2)
print(paste("Erreur quadratique moyenne:", score2))
