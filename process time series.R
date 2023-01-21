x1<-c(1:10)
x2<-c("a", "b", "c", "d", "e","f", "g", "h", "k", "l")
x3<-c(rep(TRUE,5), rep(FALSE, 5))
data<-data.frame(x1,x2,x3)
names(data)<-c("var1","var2","var3")
summary(data)
data$var4<-c(10:19)
data<-data.frame(data,var5=rep("statML",10))
data1<-subset(data,select = -c(var1,var2))
attach(amazon)
date.ts<-ts(date,start=1,frequency=24*12)
plot(sunspots,xlab="t",ylab="Sunspots",col="red")
#bruit blanc 
set.seed(123)
plot(ts(rnorm(1000,sd=3),start=1,end=1000),xlab="t",ylab="Bruit blanc gaussien de variance 9",col="green")
abline(h=0)
set.seed(123)
plot(ts(rnorm(100,sd=3),start=1,end=100),xlab="t",ylab="Bruit blanc gaussien de variance 9",col="green")
abline(h=0)
plot(uspop,xlab="t",ylab="Uspop",col="red")
plot(AirPassengers,xlab="t",ylab="Airpass",col="red")
plot(log(AirPassengers),xlab="t",ylab="Airpass",col="red")
plot(lynx,xlab="t",ylab="lynx",col="red")
#Création des bases tendancielle et saisonnière 
t=1:144
for (i in 1:12) {
  su=rep(0,times=12)
  su[i]=1
  s=rep(su,times=12)
  assign(paste("s",i,sep=""),s)
}
#REG LIN
x=AirPassengers
y=log(x)
reg=lm(y~t+s1+s2+s3+s4+s5+s6+s7+s8+s9+s10+s11+s12-1)
summary(reg)

a=mean(reg$coefficients[2:13])
b=reg$coefficients[1]
c=reg$coefficients[2:13]-mean(reg$coefficients[2:13])

#Calcul de la série corrigée des variations saisonnières

y_cvs=y-(c[1]*s1+c[2]*s2+c[3]*s3+c[4]*s4+c[5]*s5+c[6]*s6+c[7]*s7+c[8]*s8+c[9]*s9+c[10]*s10+c[11]*s11+c[12])
x_cvs=exp(y_cvs)
ts.plot(x,x_cvs,xlab="t",ylab="Airpass",col=c(1,2),lwd=c(1,2))
legend("topleft",legend=c("X","X_CVS"),col=c(1,2),lwd=c(1,2))

#décomposition saisonnière à l'aide des moyennes mobile
m2_12=function(x){
  y=(1/12)*filter(x,c(0.5,rep(1,times=11),0.5))
  return(y)
}
m3=function(x){
  y=(1/3)*filter(x,rep(1,times=3))
  return(y)
}

m13h=function(x){
  y=(1/16796)*filter(x,c(-325,-468,0,1100,2475,3600,4032,3600,2475,1100,0,-468,-325))
  return(y)
}
m5=function(x){
  y=(1/5)*filter(x,rep(1,times=5))
  return(y)}


t1=m2_12(y)
sig1=y-t1
s1=m3(m3(sig1))
shat1=s1-m2_12(s1)
ycvs1=y-shat1
xcvs1=exp(ycvs1)
ts.plot(x,xcvs1,col=c(1,2),lwd=c(1,2))
legend("topleft",legend=c("X","X_CVS"),col=c(1,2),lwd=c(1,2))


#Décomposition saisonnière à l'aide de la fonction decompose

decomp.x=decompose(x,type="multiplicative")
decomp.x$figure

plot(decomp.x)
#################################
#################################
#LISSAGE EXPONENTIEL
library(forecast)

#lissage exponentiel simple

les=ets(y,model = 'ANN')
les.pred=predict(les,12)
plot(les.pred)

#lissage exponentiel double

les=ets(x,model = 'ANN')
les.pred=predict(les,12)
plot(les.pred)


#méthode de Holt-winters

hw=ets(x,model="MMM")
hw.pred=predict(hw,12)
plot(hw.pred)


#Blancheur 

library(caschrono)
bb.sim=ts(rnorm(1000,sd=3),start=1,end=100)
acf(bb.sim,lag.max=20)

#périodogramme

library(TSA)
lynx.periodogram=periodogram(x,ylab="Periodogramme")
#fréquence maximal de la périodogramme
lynx.periodogram$freq[which.max(as.vector(lynx.periodogram$spec))]*114

#Processus AR,MA & ARMA

set.seed(123)
ar.sim1=arima.sim(n=1000,list(ar=0.6),sd=3)
plot(ar.sim1,xlab="t",ylab="X",main="AR(1)",col="red")
abline(h=0,lty=2)

acf(ar.sim1,lagmax=20)


set.seed(123)
ar.sim2=arima.sim(n=1000,list(ar=-0.9),sd=3)
plot(ar.sim2,xlab="t",ylab="X",main="AR(1)",col="red")
abline(h=0,lty=2,col="green")
acf(ar.sim2)


#processus MA

set.seed(123)
ma.sim=arima.sim(n=1000,list(ma=-0.7),sd=3)
plot(ma.sim,xlab="t",ylab="X",main="MA(1)",col="red")
abline(h=0,lty=2)
acf(ma.sim,lag.max = 20)

#Processus ARMA

arma.sim=arima.sim(n=100,list(ar=1/3,ma=-1/4),sd=1)
plot(arma.sim,xlab="t",ylab="X",main="ARMA(1,1)",col="red")
abline(h=0,lty=2)

acf(arma.sim,lag.max=20)









