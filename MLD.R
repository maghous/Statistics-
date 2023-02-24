library(tidyverse)
library(nnet)
#Mortalité dans copulation 
dat<-read.csv("Spyder.csv",stringsAsFactors = T)
attach(dat)
view(dat)
names(dat)
modele = glm(Male_survival ~ Female_chelicera_length + Female_body_weight
             + Male_chelicera_length + Male_body_weight, data=dat, family="binomial")
summary(modele)
dat$chelicera_length_ratio <- dat$Male_chelicera_length /dat$Female_chelicera_length

plot(dat$chelicera_length_ratio,dat$Male_survival=="Yes",col="orange")

coeff=modele$coefficients
plot(dat$chelicera_length_ratio, dat$Male_survival=="Yes", pch=19, 
     xlab="Ratio des longuers des chélicères", ylab="Survie du mâle", 
     main="Probabilité de survie", 
     cex.lab=1.5, col="red", cex=1.5, cex.axis=1.5)
x = seq(0, 2, 0.01)
lines(x, 1/(1+exp(-coeff[2]-coeff[1]*x)), lwd=3)

modele1 = glm(Male_survival ~ chelicera_length_ratio, data=dat, family="binomial")
summary(modele1)
plot(dat$chelicera_length_ratio,Male_survival=="Yes",col="orange")


coeff = modele1$coefficients
plot(dat$chelicera_length_ratio, dat$Male_survival=="Yes", pch=19, 
     xlab="Ratio des longuers des chélicères", ylab="Survie du mâle", 
     main="Probabilité de survie du mâle en fonction de la taille des chélicères", 
     cex.lab=1.5, col="red", cex=1.5, cex.axis=1.5)
x = seq(0, 2, 0.01)
lines(x, 1/(1+exp(-coeff[1]-coeff[2]*x)), lwd=3)

#Nombre de grains 

dat=read.csv("Pollen.csv")
attach(dat)
view(dat)
names(dat)
plot(dat$Cum.Dist..cm.,dat$GUS,col="orange",xlab = "Cum.Dist.cm.",ylab="GUS")
dat1=dat[dat$Tripped == 1,]
plot(dat1$Cum.Dist..cm.,dat1$GUS,col=2,xlab = "Cum.Dist.cm.",ylab="GUS")

modele2 <- glm(GUS~Cum.Dist..cm.,data=dat1,family = "poisson")
summary(modele2)
coef<-modele2$coefficients

x=seq(0,1500,10)
plot(dat1$Cum.Dist..cm.,dat1$GUS,xlab = "Cum.Dist.cm.",ylab="GUS",col="green")
lines(x,exp(x*coef[2]+coef[1]),lwd=3,col=4)

modele3 = glm(GUS~Cum.Dist..cm., data=dat1, family="poisson"(link="sqrt"))
coeff3 = modele3$coefficients
print(coeff3)
summary(modele3)


x=seq(0,1500,10)
plot(dat1$Cum.Dist..cm.,dat1$GUS,xlab = "Cum.Dist.cm.",ylab="GUS",col="green")
lines(x,exp(x*coeff3[2]+coeff3[1]),lwd=3,col=4)

print(paste("Prediction:", predict(modele3, newdata=list(Cum.Dist..cm.=100), type="response")[1]))
print((100*coeff3[2]+coeff3[1])**2)

modele4<-glm(GUS~.,data=dat1)
modele5<-step(modele4)

print(paste("Prediction:", 
            predict(modele5, newdata=list(Cum.Dist..cm.=100, All.flowers.order=10), type="response")[1]))

#Type de fondation 
dat<-read.csv("Fondation.csv",stringsAsFactors = T)
attach(dat)
names(dat)
modl=multinom(Fondation~.,data=dat)
summary(modl)
print(coef(modl))


dat$N =  0.5*(dat$Fondation=="Parpaing") + 1*(dat$Fondation=="Brique")
plot(dat$Prix_de_vente,dat$N,col="orange",xlab ="prix de vente",ylab="N",ylim = c(0,1))


color = rep("red", 1427)
color[dat$N==0.5] = "blue"
color[dat$N==1] = "Forestgreen"
plot(dat$Prix_de_vente, dat$N, xlab="Prix de vente", ylab="Type de fondation", 
       pch=19, col=color, cex.axis=1.5, cex.lab=1.5, 
       main="Probabilité du type de fondation en fonction du prix de vente")
  
  M = coef(modl)
  x = seq(50000, 700000, 1000)
  lines(x, exp(M[1,1]+M[1,2]*x)/(1+exp(M[1,1]+M[1,2]*x)+exp(M[2,1]+M[2,2]*x)), col="red", lwd=3)
  lines(x, exp(M[2,1]+M[2,2]*x)/(1+exp(M[1,1]+M[1,2]*x)+exp(M[2,1]+M[2,2]*x)), col="blue", lwd=3)
  lines(x, 1/(1+exp(M[1,1]+M[1,2]*x)+exp(M[2,1]+M[2,2]*x)), col="Forestgreen", lwd=3)
  
  legend(500000, 0.9, pch=c(19, 19, 19), legend=c("Brique", "Parpaing", "Béton"), 
         col=c("red", "blue", "Forestgreen"), cex=1.5)

