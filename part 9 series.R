library(datasets)
X=as.numeric(Nile)
Lissage<-function(a){
  T=length(X)
  L=rep(NA,T)
  L[1]=x[1]
  for (t in 2:T) {L[t]=a*X[t]+(1-a)*L[t-1]}
  return(L)  
}
plot(X,type="b",cex=.5)
lines(Lissage(.2),col="red")

a1<-ts(data=rnorm(1000),start =0,frequency=1/20)
plot(a1,col="red")

a2<-ts(matrix(rt(204,df=3)),start =c(2000,2),frequency=12)
print(a2)
plot(a2,col="red")


a3<-ts(matrix(rnorm(300),100,3),start = c(2000,1),frequency =12) 
class(a3)
plot(a3,col="red")
plot(a3,plot.type = "single",lty=1:3)


plot(co2,col="red")
lines(filter(co2,rep(1/12,12),side=1),col="green")

r<-decompose(co2)
plot(r)

recons<-r$trend + r$seasonal
par(mfrow=c(1,2))
plot(co2)
plot(recons,col="red")

plot(AirPassengers,col="blue")
#autocorrélationn
acf(AirPassengers)
points(x=1,y=0.76,col="orange")
points(x=1/12,y=0.848,col=3)
#réprésentation de la série 
time<-ts(AirPassengers,frequency =12)
plot(time,col="Green")
boxplot(time)
#affichons le lag plot de la série temporelle 
lag.plot(time,lags=9,do.lines=FALSE,diag.col = "red")
#affichons l'histogramme et le qqplot de la séeire
par(mfrow=c(1,2))
hist(time,breaks =15,col="blue")
qqnorm(time)
qqline(time,col="red")
#Décomposition de la série 
wi<-decompose(time)
par(mfrow=c(3,1))
plot(wi$trend,type="l",col="red",main="tendance")
plot(wi$seasonal,type="l",col="green",main="saisonnière")
plot(wi$random,type="l",col="blue",main="bruit")
#acf/pacf
par(mfrow=c(1,2))
acf(wi$random,na.action = na.pass,main="acf")
pacf(wi$random,na.action = na.pass,main="pacf")

residu<- ts(AirPassengers, start = c(1990,8), end = c(2006,2), frequency = 12)
y <- window(residu, start=c(1990,8),end=c(2005,8), frequency=12)
new_serie=window(y, start=c(1990,8),end=c(2005,2),frequency=12)
plot(new_serie)








