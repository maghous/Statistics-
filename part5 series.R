

#Exercice 1 Test de blancheur

```{r cars}
epsn=rnorm(100)
r=acf(epsn)
#acf(epsn)
```

## part2
```{r}
yn=epsn[1:99] + epsn[2:100]
ryn=acf(yn)
rho=ryn$acf[2]
lag.plot(yn,2)
```
## part3
```{r}
epsu=runif(100,-sqrt(3),sqrt(3))
var=sd(epsu)**2
hist(epsu,breaks = 10,col="blue")
```

## part 4
```{r}
Box.test(epsn)
Box.test(yn)
```
```{r}
a1=Box.test(epsn,lag=1)
a2=Box.test(epsn,lag=2)
a3=Box.test(epsn,lag=3)
a4=Box.test(epsn,lag=4)
a5=Box.test(epsn,lag=5)

print(a1$p.value)
print(a2$p.value)
print(a3$p.value)
print(a4$p.value)
print(a5$p.value)
```
```{r}
a11=Box.test(epsn,lag=1,type='Ljung-Box')
a12=Box.test(epsn,lag=2,type='Ljung-Box')
a13=Box.test(epsn,lag=3,type='Ljung-Box')
a14=Box.test(epsn,lag=4,type='Ljung-Box')
a15=Box.test(epsn,lag=5,type='Ljung-Box')

print(a11$p.value)
print(a12$p.value)
print(a13$p.value)
print(a14$p.value)
print(a15$p.value)
```

```{r}
b1=Box.test(yn,lag=1)
b2=Box.test(yn,lag=2)
b3=Box.test(yn,lag=3)
b4=Box.test(yn,lag=4)
b5=Box.test(yn,lag=5)

print(b1$p.value)
print(b2$p.value)
print(b3$p.value)
print(b4$p.value)
print(b5$p.value)
```

```{r}
b11=Box.test(yn,lag=1,type='Ljung-Box')
b12=Box.test(yn,lag=2,type='Ljung-Box')
b13=Box.test(yn,lag=3,type='Ljung-Box')
b14=Box.test(yn,lag=4,type='Ljung-Box')
b15=Box.test(yn,lag=5,type='Ljung-Box')

print(b11$p.value)
print(b12$p.value)
print(b13$p.value)
print(b14$p.value)
print(b15$p.value)
```


#Exercice 2 simulation d'un processus MA(q)
```{r}
t=1:1000
epsilon=rnorm(1000,0,1)
X<-function(z){
  p={}
  for(i in length(z)){
    p[i+1]=p[i] + z[i] -1/3 *z[i-1]
  }
  p
}
```
#Exercice 4 simulation automatique 


```{r}
set.seed(219)
X1=arima.sim(n=200,list(ma=c(-.3,.6)),sd=1)
ARMAacf(ma=c(-.3,.6),lag.max=20)
ARMAacf(ar=c(.9),lag.max=20,pacf = T)
ARMAtoMA(ar=c(.9),ma=c(-.3,.6),lag.max = 20)
```










