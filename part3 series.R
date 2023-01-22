data('AirPassengers')
is.ts(AirPassengers)
time(AirPassengers)
#This will plot the time series
ts.plot(AirPassengers, xlab="Year", ylab="Number of Passengers", main="Monthly totals of international airline passengers, 1949-1960",col="red")
# This will fit in a line
abline(reg=lm(AirPassengers~time(AirPassengers)),col="green")

acf(AirPassengers)
pacf(AirPassengers)
#MODEL
AR <- arima(AirPassengers, order = c(1,0,0))
print(AR)
#PLOT
ts.plot(AirPassengers)
AR_fit <- AirPassengers - residuals(AR)
points(AR_fit, type = "l", col = 2, lty = 2)
#MODEL
AR1 <- arima(AirPassengers, order = c(3,2,1))
print(AR1)
#PLOT
ts.plot(AirPassengers)
AR_fit <- AirPassengers - residuals(AR1)
points(AR_fit, type = "l", col = 2, lty = 2)
#ar model
pred_ar<-predict(AR)
pred_ar$pred[1]
#plot
ts.plot(AirPassengers, xlim = c(1949, 1961))
AR_forecast <- predict(AR, n.ahead = 10)$pred
AR_forecast_se <- predict(AR, n.ahead = 10)$se
points(AR_forecast, type = "l", col = 2)
points(AR_forecast - 2*AR_forecast_se, type = "l", col = 2, lty = 2)
points(AR_forecast + 2*AR_forecast_se, type = "l", col = 2, lty = 2)
#MA
ma<-arima(AirPassengers,order = c(1,3,1))
ts.plot(AirPassengers)
ma_fit<-AirPassengers - resid(ma)
points(ma_fit,type="l",col=2,lty=3)
###########################################
###########################################
library(tsm)
set.seed(123)
x<-arima.sim(model = list(ar=0.6),n=1000)
plot.ts(x)
acf(x)
pacf(x)
box.red<-Box.test(x, lag = 1, type = "Ljung-Box")
Q.stat <- rep(0, 10)
Q.prob <- rep(0, 10)
Q.stat[1] <- Box.test(x, lag = 1, type = "Ljung-Box")$statistic
Q.prob[1] <- Box.test(x, lag = 1, type = "Ljung-Box")$p.value
for (i in 1:10) {
  Q.stat[i] <- Box.test(x, lag = i, type = "Ljung-Box")$statistic
  Q.prob[i] <- Box.test(x, lag = i, type = "Ljung-Box")$p.value
}
op <- par(mfrow = c(1, 2))  # create plot area of (1 X 2)
plot(Q.stat, ylab = "", main = "Q-Statistic")
plot(Q.prob, ylab = "", ylim = c(0, 1), main = "Probability values")
###############
###############
arma10<-arima(x,order=c(3,2,1),include.mean = F)
plot(arma10$residuals,col="red")
acf(arma10$residuals,max.lag=19)
pacf(arma10$residuals,max.lag=19)

y<-arima.sim(model=list(ma=0.5),n=1000)
plot.ts(y,col="red")
abline(h=0)
acf(y,max.lag=19)
pacf(y,max.lag=19)

arma01<-arima(y,order=c(0,0,1))
plot(arma01$residuals,col="red")
abline(h=0)
acf(arma01$residuals,max.lag=19)
pacf(arma01$residuals,max.lag=19)

##############
#Ar process
x<-arima.sim(model=list(ar=0,4),n=1000)
acf(x)
pacf(x)
#ma
x <- arima.sim(model = list(ma = 0.5), n = 1000)  # MA(1) process
acf(x)
pacf(x)
##########
x <- arima.sim(model = list(ar = 0.4, ma = 0.5), n = 200)  ## ARMA(1,1)
ac(x, max.lag = 20)
acf(x)
pacf(x)
library(strucchange)
plot.ts(gdp)
#######
r=0.95
w=2*pi/15
phi1=2*r*cos(w)
phi2=-r^2
xt=arima.sim(1000,model=list(ar=c(phi1,phi2)))
ts.plot(as.ts(xt))
acf(xt,lag=50)
acf(xt,lag=50,type="partial")
#########
r1=0.9
r2=0.5
phi1=r1+r2
phi2=-r1*r2
xt=arima.sim(500,model=list(ar=c(phi1,phi2)))
acf(xt,lag=50)
pacf(xt)
#####################################
set.seed(250)
timeseries=arima.sim(list(order = c(1,1,2), ma=c(0.32,0.47), ar=0.8), n = 50)+20
plot(timeseries)
acf(timeseries)
pacf(timeseries)
#######################################
set.seed(123)
w=rnorm(1000)
x = filter(w, filter=c(.7), method="recursive")[-(1:50)]
data=log(AirPassengers)
data=ts(data,frequency=12,start=c(1949,1))
plot(AirPassengers)
plot(data, main="log du nbre mensuel de passagers", xlab="ann´ee",
     ylab="")

dataMOVA=filter(data,filter=rep(1/12,12), sides=2)
dataMOVA=filter(data,filter=rep(1/12, 12))
dataMOVA=filter(data,filter=c(1/24,rep(1/12,11),1/24))
plot(data, main="log du nbre mensuel de passagers (en millier)",
     xlab="ann´ee",
     ylab="")
lines(dataMOVA,col="red")
y=as.vector(data)
x=as.vector(time(data))
reg=lm(y~poly(x,3))
lines(predict(reg)~x)
#Pr´evision Holt-Winters
data.hw<-HoltWinters(data)
predict(data.hw, n.ahead=12)
plot(data, main="log du nbre mensuel de passagers (en millier)",
     xlab="ann´ee",
     ylab="", xlim=c(1949,1962),ylim=c(4.6, 6.5))
lines(predict(data.hw, n.ahead=12), col=2)

#Stationnarisation
y=diff(data,lag=12,differences=1)
x=diff(y,lag=1,differences=1)
plot(y)
plot(x)
mean(x)
adf.test(data, alternative=c("stationary"),12)
adf.test(y, alternative=c("stationary"),12)
adf.test(x, alternative=c("stationary"),12)

acf(x)
pacf(x)
#mod`ele ARIMA
z<-arima(data, order=c(3,1,1), seasonal=list(order=c(1,1,1), period=12))
z<-arima(data, order=c(3,1,1), seasonal=list(order=c(0,1,1), period=12))
z<-arima(data, order=c(2,1,1), seasonal=list(order=c(1,1,1), period=12))
z<-arima(data, order=c(2,1,1), seasonal=list(order=c(0,1,1), period=12))
z<-arima(data, order=c(1,1,1), seasonal=list(order=c(1,1,1), period=12))
z<-arima(data, order=c(1,1,1), seasonal=list(order=c(0,1,1), period=12))
z<-arima(data, order=c(0,1,1), seasonal=list(order=c(0,1,1), period=12))
#Etude des r´esidus
tsdiag(z, gof.lag=20)
plot(z$residuals)
shapiro.test(z$residuals)
hist(z$residuals,main="Histogramme
des r\'esidus",
     freq=F)
curve(dnorm(x,mean(z$residuals),sqrt(var(z$residuals))),
      add=TRUE)
#Pr´evision ARIMA
prev<-predict(z, n.ahead=12)
plot(data, main="log du nbre mensuel de passagers (en millier)", xlab="ann´ee",
     ylab="", xlim=c(1949,1962),ylim=c(4.6, 6.5))
lines(prev$pred,col="blue")
lines(predict(data.hw, n.ahead=12), col=2)
tl <- prev$pred - 1.96 * prev$se
tu <- prev$pred + 1.96 * prev$se
ts.plot(data, tl, tu , lty=c(1,2,2),
        main="log du nbre mensuel de passagers (en millier) et
pr´evision sur 1 an", xlab="ann´ee")
lines(prev$pred,col="blue")
ts.plot(AirPassengers, exp(tl), exp(tu), lty=c(1,2,2),
        main="nombre mensuel de passagers (en millier)
et pr´evision sur 1 an", xlab="ann´ee")
lines(exp(prev$pred), col="blue")


