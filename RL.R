X = c(2.3, 1.4, 1.6, 0.7, -0.4, 0.9, 0.2, 0.7)
Y = c(1, 1, 1, 1, 0, 0, 0, 0)
modele <- glm(Y~X,family = binomial)
summary(modele)
coeff<-modele$coefficients
print(coeff)

print(predict(modele,newdata=list(X=0.8),type="response"))
print(1/(1+exp(-(coeff[1]+coeff[2]*0.8))))

x=seq(-1,3,0.01)
plot(x, 1/(1+exp(-(coeff[1]+coeff[2]*x))), type="l",col=4)
points(X, Y)
abline(v=-coeff[1]/coeff[2],col="red")

plot(x, 1/(1+exp(-(coeff[1]+coeff[2]*x))), type="l", xlab="x", ylab="y", lwd=2, cex.lab=2
     , main="Comparaison de l'estimation")
points(X,Y,col="red",pch=20,cex=2)
abline(v=-coeff[1]/coeff[2],col=5,lty=2,lwd=2)
abline(h=0,lty=2,lwd=0.5)
abline(h=1,lty=2,lwd=0.5)

#Visualisation de rég

data<-read.csv("Visu.csv")
V<-ifelse(data$Y,"red","green")
plot(data$X1,data$X2,col=V,xlab="X1",ylab="X2",pch=19,cex=1.2)

modele1 <- glm(Y~X1+X2,data=data,family = binomial)
coeff<-modele1$coefficients
plot(data$X1,data$X2,col=V,xlab = "X1",ylab='X2',pch=19,cex=1.2)
abline(-coeff[1]/coeff[3],-coeff[2]/coeff[3],lwd=3,lty=2)


plot(data$X1, data$X2, col=V, xlab="X1", ylab="X2", pch=19, cex=1.5, main="Visualisation de la régression logistique")
abline(-coeff[1]/coeff[3], -coeff[2]/coeff[3], lwd=3, lty=2)
polygon(c(-1,2,2,-1), c((-log(9)-coeff[1]+coeff[2])/coeff[3],(-log(9)-coeff[1]-2*coeff[2])/coeff[3]
                        ,(log(9)-coeff[1]-2*coeff[2])/coeff[3]
                        ,(log(9)-coeff[1]+coeff[2])/coeff[3]), col=adjustcolor("grey30",alpha.f=0.5)  , border=NA)

dat<-read.csv("acouphenes.csv",stringsAsFactors = TRUE)
attach(dat)
view(dat)
names(dat)
unique(dat$cÃ.tÃ.)
unique(dat$durÃ.e)
unique(dat$intensitÃ.)
unique(dat$type)
table(dat$hauteur,dat$groupe)
chisq.test(table(dat$hauteur,dat$groupe))
chisq.test(table(dat$cÃ.tÃ.,dat$groupe))
chisq.test(table(dat$durÃ.e,dat$groupe))
chisq.test(table(dat$type,dat$groupe))
#on ne rejette pas l'hypothèse nulle

modele2 <- glm(groupe~.,data=dat,family=binomial())
summary(modele2)

model_selc <- step(modele2)


dat = read.csv("2ou8.csv", stringsAsFactors = TRUE)
image(matrix(as.numeric(dat[31,2:170]), nrow=13)[,13:1], col=gray(12:1/12))

vec = sqrt(diag(cov(dat[, 2:170])))>0.2
image(matrix(as.numeric(1-vec), nrow=13)[,13:1], col=gray(12:1/12), xaxt="n", yaxt="n")
grid(nx = 13)

dat_extract = dat[, c(TRUE, vec)]

modele = glm(label~., data=dat_extract, family = binomial)
prop_success = 1 - mean(abs(as.numeric(predict(modele, type = "response")>0.5) - (as.numeric(dat$label)-1)))
print(paste(round(100 * prop_success, 2), "%", sep=""))

dat_to_predict = read.csv("image.csv")
par(mfrow=c(1,2), pin=c(3,3))
image(matrix(as.numeric(dat_to_predict[1,]), nrow=13)[,13:1], col=gray(12:1/12))
image(matrix(as.numeric(dat_to_predict[2,]), nrow=13)[,13:1], col=gray(12:1/12))
print(predict(modele, newdata=dat_to_predict, type="response"))


#calcul numérique 
X = c(2.3, 1.4, 1.6, 0.7, -0.4, 0.9, 0.2, 0.7)
Y = c(1, 1, 1, 1, 0, 0, 0, 0)

fun = function(param){
  logit = 1/(1+exp(-param[1]*X-param[2]))
  return(- sum(Y*log(logit) + (1-Y)*log(1-logit)))
}

print(optim(c(0,0), fun)$par)
print(glm(Y~X, family = binomial)$coefficients)



