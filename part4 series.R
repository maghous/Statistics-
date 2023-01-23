
```

## Analyse des données AirPassengers

### 1 
```{r}
library(datasets)
X=AirPassengers
#Summary(AirPassengers)
```
### 2
```{r}
plot.ts(X,col="red",main="ts de X")
monthplot(X)
boxplot(X~cycle(X),xlab="Month")
```
### 3 
```{r}
lag.plot(X,4,col="red")
lag.plot(X,12,col="red")

```
###4
#### a
```{r}
t=time(X)
linmodel=lm(X~t)
linmodel$coefficients
```
#### b 
```{r}
plot.ts(X)
abline(linmodel,lty=20,col="red")
```
```{r}
toto=linmodel$coefficients[2] * t + linmodel$coefficients[1]
X_sans_tendance=X- toto
plot(X_sans_tendance)
```
```{r}
mean(X_sans_tendance)
```


#### c
```{r}
acf(X_sans_tendance)
pacf(X_sans_tendance)
```
#### d

```{r}
library(forecast)
model=auto.arima(X_sans_tendance)
plot(model$residuals)
```
#### e 
```{r}
eps=model$residuals
Box.test(eps,lag=20)
```
### 5 
#### a 
```{r}
y<-diff(X,lag=T,differences = 5)
y
y1<-diff(X,lag=T,differences =1)

```
```{r}
plot.ts(y,col="red")
abline(h=0,col="green")
```

```{r}
plot.ts(y1,col="red")
abline(h=0,col="green")
```
### b

```{r}
acf(y,main="auto-corrélation")
pacf(y,main="auto-corrélatio partielle")
```
```{r}
#model=auto.arima(y)
model$coef
model$arma
```
#### d
```{r}
eps=model$residuals
Box.test(eps,lag=20)
```



### 6 
```{r}
new_X<-decompose(X,type="additive")
new_X1<-decompose(X,type="multiplicative")
plot(new_X$figure,type="l")
lines(new_X1$figure,type="l",col="red")
```
```{r}
plot(new_X$seasonal,col="red")
abline(h=0)
plot(new_X1$seasonal,col="red")
abline(h=0)
```

### b 

```{r}
eps=new_X$random
acf(na.omit(eps))
```
### c
```{r}
x_ds=filter(X,rep(1/12,12),method="convolution")
Y=diff(x_ds[6:(144-6)],lag=1,differences = 2)
acf(Y)
pacf(Y)
MOYENNE=mean(Y)
tt=time(Y)
model1=lm(Y~tt)
plot.ts(Y)
abline(model1,lty=20,col="red")

```

## Exercice données de la temérature global
### prépation des données et analyse visuelle 
#### 1
```{r}
attach(monthly_csv)
data=as.data.frame(monthly_csv[monthly_csv$Source == "GISTEMP",])
data=data[order(data$Date),]
#yser=data.frame(y,row.names = NULL)
X=ts(data$Mean,start = 1910,end = 2012,frequency = 12)
#X=ts(yser$Mean[(30*12 + 1):n],start = 1910,end = 2012,frequency = 12)
plot(X,col="red")
```
#### 2
```{r}
acf(X)
pacf(X)
lag.plot(X,20)
```
```{r}
monthplot(X,col="red")
boxplot(X~cycle(X))
```
```{r}
lag.plot(X,4)
```

## determination de la tendance et de la saisonalité
### 1
```{r}
toto=decompose(X,type="additive")
wi=filter(X, 1 , method ="convolution",filter=(1/12) *rep(1,12))

a=rep(0,102)
a1=rep(0,102)
a2=rep(0,102)
a3=rep(0,102)
a4=rep(0,102)
a5=rep(0,102)
a6=rep(0,102)
a7=rep(0,102)
a8=rep(0,102)
a9=rep(0,102)
a10=rep(0,102)
a11=rep(0,102)
for (i in 1:102) {
  a[i]=wi[12*i+1]
  a1[i]=wi[12*i+2]
  a2[i]=wi[12*i+3]
  a3[i]=wi[12*i+4]
  a4[i]=wi[12*i+5]
  a5[i]=wi[12*i+6]
  a6[i]=wi[12*i+7] 
  a7[i]=wi[12*i+8]
  a8[i]=wi[12*i+9]
  a9[i]=wi[12*i+10]
  a10[i]=wi[12*i+11]
  a11[i]=wi[12*i+12]
}
mt=c(mean(na.omit(a)),mean(na.omit(a1)),mean(na.omit(a2)),mean(na.omit(a3)),mean(na.omit(a4)),mean(na.omit(a5)),mean(na.omit(a6)),mean(na.omit(a7)),mean(na.omit(a8)),mean(na.omit(a9)),mean(na.omit(a10)),mean(na.omit(a11)))


```

```{r}
wi=filter(X,(1/12) *rep(1,12) , method ="convolution")
XPRIME=X-wi
```

### 3
```{r}
a=rep(0,102)
a1=rep(0,102)
a2=rep(0,102)
a3=rep(0,102)
a4=rep(0,102)
a5=rep(0,102)
a6=rep(0,102)
a7=rep(0,102)
a8=rep(0,102)
a9=rep(0,102)
a10=rep(0,102)
a11=rep(0,102)
for (i in 1:102) {
  a[i]=XPRIME[12*i+1]
  a1[i]=XPRIME[12*i+2]
  a2[i]=XPRIME[12*i+3]
  a3[i]=XPRIME[12*i+4]
  a4[i]=XPRIME[12*i+5]
  a5[i]=XPRIME[12*i+6]
  a6[i]=XPRIME[12*i+7] 
  a7[i]=XPRIME[12*i+8]
  a8[i]=XPRIME[12*i+9]
  a9[i]=XPRIME[12*i+10]
  a10[i]=XPRIME[12*i+11]
  a11[i]=XPRIME[12*i+12]
}
st=c(mean(na.omit(a)),mean(na.omit(a1)),mean(na.omit(a2)),mean(na.omit(a3)),mean(na.omit(a4)),mean(na.omit(a5)),mean(na.omit(a6)),mean(na.omit(a7)),mean(na.omit(a8)),mean(na.omit(a9)),mean(na.omit(a10)),mean(na.omit(a11)))

```
```{r}
st1=rep(st[1],102)
st2=rep(st[2],102)
st3=rep(st[3],102)
st4=rep(st[4],102)
st5=rep(st[5],102)
st6=rep(st[6],102)
st7=rep(st[7],102)
st8=rep(st[8],102)
st9=rep(st[9],102)
st10=rep(st[10],102)
st11=rep(st[11],102)
st12=rep(st[12],102)


stchap = matrix(data = c(st1,st2,st3,st4,st5,st6,st7,st8,st9,st10,st11,st12), ncol = 12)
as.vector(stchap)
```



```{r}
epss=XPRIME  - as.vector(stchap)
plot(epss,col="red")
```




### 1
```{r}
res1=decompose(X,type="additive")
tendance=res1$trend
saison=res1$seasonal
eps1=res1$random
plot(res1)
```


```{r}
res2=stl(X,s.window=12,s.degree = 0)
tendance=res2$time.series[,"trend"]
saison=res2$time.series[,"seasonal"]
eps2=res2$time.series[,"remainder"]
plot(eps2)
```
### 2

```{r}
monthplot(epss)
boxplot(epss)
```

### 3
```{r}
acf(saison)
acf(tendance)
acf(eps)
acf(X)
```

## Ajustement de'un modèle ARIMA pour les résidus 

### 1
```{r}
arma=auto.arima(eps1)
arma1=auto.arima(eps2)
```

### 2
```{r}
shapiro.test(arma$residuals) 
shapiro.test(arma1$residuals) 

```
### 3 
```{r}
library(seasonal)
res3=seas(eps)
summary(res3)
view(res3)
```





# Exercice 3 Analyse des données de PNB AM2RICAIN 
###
```{r}
attach(gnpus)
X=ts(gnpus)
plot(X)
```
### 2 

```{r}
Box.test(X,lag=20)
```

### 3 

```{r}
acf(X,2)
acf(X,4)
acf(X,6)
acf(X,12)
pacf(X)

```

### 4
```{r}
modelar=ar(X)
modelar$ar #c donc c'est un ar(3)
modelar
```

### 5 


```{r}
plot(modelar$resid,col="red")
pacf(na.omit(modelar$resid))
shapiro.test(modelar$resid)
```


```{r}
auto.arima(X)
```






















