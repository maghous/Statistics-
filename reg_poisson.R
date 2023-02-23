library(MASS)
the <- c(1,1,2)

N <- 50
X1 <- rnorm(50,-1,1)
X2 <- rexp(50,5)

lam <- c()
for (i in 1:N){
  lam <- c(lam,exp(the[1]+the[2]*X1[i]+the[3]*X2[i]))
}

Y <- rpois(N,lam)

datapoi <- data.frame(X1,X2,Y)

modpoi <- glm(Y~X1+X2,family = poisson(link = log))

print(summary(modpoi))

# Verification de la matrice de covariance :
X <- cbind(rep(1,N),X1,X2)
dlam <- diag(lam,N,N)
W <- t(X)%*%dlam%*%X
library(matlib)
Sigm <- inv(W)
elam <- exp(X%*%coefficients(modpoi))
elam2 <- diag(rep(0,50),N,N)
elam0 <- c()
for (i in 1:N){
  elam2[i,i] <- elam[i,1]
  elam0 <- c(elam0,elam[i,1])
}
Wchap <- t(X)%*%elam2%*%X
Sigchap <- inv(Wchap)
print('Covariance du modele :')
print(vcov(modpoi))
print('Covariance si on utilise les vrais lambdas :')
print(Sigm)
print('Covariance par calcul direct avec estimation :')
print(Sigchap)

# Calcul de la deviance
# on regarde directement la deviance entre le modele et le modele nul :
mY <- mean(Y)
dmodzero <- 0
dmodsat <- 0
rd2 <- c()
rd <- c()
r <- c()
for (i in 1:N){
  dmodzero <- dmodzero+2*(Y[i]*log(elam0[i]/mY)-(elam0[i]-mY))
  if (Y[i]!=0){
    rd2 <- c(rd2,2*(Y[i]*log(elam0[i]/Y[i])-(elam0[i]-Y[i])))
  } else{
    rd2 <- c(rd2,-2*elam0[i])
  }
  rd <- c(rd,sqrt(abs(rd2[i]))*sign(Y[i]-elam0[i]))
  r <- c(r,(Y[i]-elam0[i])/elam0[i])
  dmodsat <- dmodsat+rd2[i]
}
dmodzero <- -dmodzero
dmodsat <- -dmodsat
print(dmodzero)
print(modpoi$deviance-modpoi$null.deviance)
print(dmodsat)
print(modpoi$deviance)
print(rd)
print(r)
print(modpoi$residuals)

# Predictions
pr <- predict(modpoi,type="response",se.fit=TRUE)
# ceci contient en fait elam0 (l'estimateur de lambda_i pour chaque point) et son ecart type
plot(c(1:N),Y,"p",col="blue",pch=1,ylim=c(0,3),xlab="i",ylab="valeur/prediction")
par(new=T)
plot(c(1:N),pr$fit,"p",col="red",pch=2,ylim=c(0,3),xlab="i",ylab="valeur/prediction")
# La valeur la plus probable avec ces lambda_i (chapeaux) est donnee par...
ypp <- ceiling(pr$fit)-1
par(new=T)
plot(c(1:N),ypp,"p",col="black",pch=7,ylim=c(0,3),xlab="i",ylab="valeur/prediction")

# Application aux affaires extraconjugales
library(AER)
data("Affairs")
View(Affairs)
# Dans ce jeu de donnees, le nombre d'affaires est code de facon genante : 
# 7 correspond a entre 4 et 10 fois, et 12 d'hebdomadaire a quotidien
# on va donc modifier le jeu de donnees pour que ces quantites soient plus "claires", mais en faisant
# cela on modifie les choses de facon arbitraire sans lien avec les donnees...
Naff <- length(Affairs$affairs)
for (i in 1:Naff){
  if(Affairs$affairs[i]==7){
    u <- floor(runif(1,4,10))
    Affairs$affairs[i] <- u
  }
  if(Affairs$affairs[i]==12){
    u <- 11+rgeom(1,0.2)
    Affairs$affairs[i] <- u
  }
}

fm <- glm(affairs ~ age + yearsmarried + religiousness + occupation + rating,data=Affairs,family=poisson(link = log))
summary(fm)
library("pscl")
fm_zip <- zeroinfl(affairs ~ age + yearsmarried + religiousness + occupation + rating | age + 
                     yearsmarried + religiousness + occupation + rating, data = Affairs)
summary(fm_zip)

vuong(fm_zip,fm)

# regressions multinomiales

library(nnet)

library(car)
attach(Womenlf)
Womenlf$partic <- factor(Womenlf$partic,
                         levels=c("not.work","parttime","fulltime"))

womenlf.model <- multinom(partic~hincome+children+region, data=Womenlf)
summary(womenlf.model)

Anova(womenlf.model)

# On enleve la region qui est moins significative
womenlf.model1 <- multinom(partic~hincome+children, data=Womenlf)
summary(womenlf.model1,Wald=TRUE)
womenlf.model3 <- multinom(partic~hincome*children, data=Womenlf)

# Plots
library(effects)
plot(allEffects(womenlf.model1), ask=FALSE)
plot(effect("hincome*children",womenlf.model3))
