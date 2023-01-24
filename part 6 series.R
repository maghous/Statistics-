###################################################
#Mean,Median and mode 
x <- c(12,7,3,4.2,18,2,54,-21,8,-5)
result.mean <- mean(x)
result.mean1 <-  mean(x,trim = 0.3)
X <- c(12,7,3,4.2,18,2,54,-21,8,-5,NA)
mean(na.omit((X)))
rslt<-mean(X,na.rm = T)
####################################################
#Linear regression 
x <- c(151, 174, 138, 186, 128, 136, 179, 163, 152, 131)
y <- c(63, 81, 56, 91, 47, 57, 76, 72, 62, 48)
model<-lm(y~x)
print(model)
print(summary(model))
plot(y,x,,col="red",main = "regression lineair")
lines(model$coefficients[2],model$coefficients[1])
####################################################
#Multiple regression 
data <- mtcars[,c("mpg","disp","hp","wt")]
attach(data)
model1<-lm(mpg~disp + hp + wt,data=data)
print(summary(model1))
#####################################################
#regression model 
data1 <- mtcars[,c("am","cyl","hp","wt")]
model2<-glm(am~disp + hp + wt,data=data1,family=binomial)
print(summary(model2))
#####################################################
#Normal Distribution
x <- seq(-1, 1, by = .01)
y <- dnorm(x, mean = 0, sd = 0.5)
plot(x,y,main="normal distribution",col="red")
y1<-pnorm(x,mean=0,sd=0.5)
plot(x,y1,main="proba",col="red")
y2<-qnorm(x,mean=0,sd=0.5)
plot(x,y2)
y=rnorm(1000)
hist(y,breaks =20,col="red")
######################################################
#Binomial Distribution 
x<-seq(0,50,by=1)
y<-dbinom(x,50,0.5)
plot(x,y,col='red',type="l")
######################################################
#Poisson regression 
data<-warpbreaks
attach(data)
model3<-glm(breaks~wool + tension,data=data,family = poisson)
print(summary(model3))
######################################################
#Time series
data <- c(799,1174.8,865.1,1334.6,635.4,918.5,685.5,998.6,784.2,985,882.8,1071)
data.timeseries <- ts(data,start = c(2012,1),frequency = 12)
plot(data.timeseries,col="red")
#multiple time series 
data1<- 
  c(655,1306.9,1323.4,1172.2,562.2,824,822.4,1265.5,799.6,1105.6,1106.7,1337.8)
combined.data <-  matrix(c(data,data1),nrow = 12)
data.timeseries1 <- ts(combined.data,start = c(2012,1),frequency = 12)
plot(data.timeseries1, main = "Multiple Time Series",col='red')
