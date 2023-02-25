#régression simple
a <- 1
b <- 10
X <- runif(30,min=0,max=10)
Y <- a*X+b+rnorm(30,0,5)

modele <-lm(Y~X)
achap <- modele$coefficients[1]
bchap <- modele$coefficients[2]

summary(modele)

plot(X,Y,col="red")
abline(b,a,col="green",lwd=4)
abline(achap,bchap,col="gray",lwd=2)

J <- 3
fac <- c("un","deux","trois")
dec <- c(4,3,7)
nb <- c(16,24,31)

n<-sum(nb)

X<- c()
Y <- c()
for(j in 1:3){
  X <- c(X,rep(fac[j],nb[j]))
  Y <- c(Y,rnorm(nb[j],dec[j],1))
}
df<-data.frame(X,Y)
ano<-lm(Y~X)
summary(ano)

anova(ano)

modele1<-aov(Y~X)
summary(modele1,2)


#Analyse de la variance a deux facteurs 

N <- 20
I <- 2
J <- 3
fac1 <- c("fac1 - un","fac1 - deux")
fac2 <- c("fac2 - un","fac2 - deux","fac2 - trois")


mu <- 0
a <- c(1,-1)
b <- c(1,0,-1)
c <- rbind(c(0,1,-1),c(0,-1,1))

X1<-c()
X2<-c()
Y<-c()
for(i in 1:2){
  for(j in 1:3){
    X1 <- c(X1,rep(fac1[i],20))
    X2 <- c(X2,rep(fac2[j],20))
    Y <- c(Y,rnorm(20,mu+a[i]+b[j]+c[i,j]))
  }
}

modele2<-lm(Y~X1+X2)
summary(modele2)    

anova(modele2)
modele3<-aov(Y~X1*X2)
summary(modele3)

a <- 1
b <- 10
X <- runif(30,min=0,max=10)
Y <- a*X+b+rnorm(30,0,5)
rl<-data.frame(X,Y)
modele4 <- lm(Y~X)
summary(modele4)

res<-modele4$residuals
print(res)
quantile(unique(res))


a <- 1
b <- 10
X <- runif(30,min=0,max=10)
Y <- c()
for (i in 1:30){
  Y <- c(Y,a+b*X[i]+X[i]^2+rnorm(1,0,5))
}

modele5<-lm(Y~X)
summary(modele5)



a <- 1
b <- 10
X <- runif(30,min=0,max=10)
X <- sort(X)
Y <- a*X+b+rnorm(30,0,5)
Y[4] <- Y[4]+50
Y[26] <- Y[26]-50
rl4 <- lm(Y~X)
achap <- rl4$coefficients[1]
bchap <- rl4$coefficients[2]
Summary(rl4)


plot(X,Y,pch=4,col="blue")
abline(b,a,col="red")
abline(achap,bchap,col="green",lwd=2)

plot(rl4,3)
plot(rl4,4)
plot(rl4,5)
plot(rl4,6)


#modele lineare general

n <- 50
X0 <- rep(1,50)
X1 <- runif(50,min=0,max=10)
X2 <- rnorm(50,5,4)
theta <- c(1,2,-5)
Y <- theta[1]*X0+theta[2]*X1+theta[3]*X2+rnorm(50,0,1)

ml <- data.frame(Y,X1,X2)

mlg <- lm(Y~.,data=ml)
summary(mlg)

plot(mlg,1)
plot(mlg,2)
plot(mlg,3)
plot(mlg,4)
plot(mlg,5)
plot(mlg,6)

x <- c(-5,-4,-3,-2,-1,0,1,2,3,4,5)
xcarre <- c(25,16,9,4,1,0,1,4,9,16,25)
z <- c(1.5,1.9,1,1.7,1.1,1.7,1.6,1.2,1.1,1.3,1.4)
u <- c(1.2,0.8,1.2,0.8,1.2,0.8,1.2,0.8,1.2,0.8,1.2)
y <- c(-3.37 ,-2.11, -2.24 ,1.59 ,3.28 ,3.96, 6.42, 8.57 ,10.71, 14.32 ,15.9)
y.mc <- lm(y~x+xcarre+x+z+u)

anova(y.mc)

plot(y.mc,2)
plot(y.mc,6)

y.m2 <- lm(y~x+xcarre+x+z)
anova(y.m2)

plot(y.m2,3)
plot(y.m2,6)

y.m3 <- lm(y~x+xcarre+x)
anova(y.m3)

plot(y.m3,3)
plot(y.m3,6)

library(leaps)
Z <- matrix(c(x,xcarre,z,u),ncol=4);
colnames(Z) <- c("x","xcarre","z","u");
r <- leaps(Z,y);
r$whi

ZAD <- as.data.frame(Z)
y.lm <- lm(y~.,data=ZAD)
library(MASS)
y.bic <- stepAIC(y.lm,k=log(11))




