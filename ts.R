
```{r pressure, echo=FALSE}
x=c(4,3,7,1.7,3,8,4,5,12,9,12)
is.ts(x)
y=as.ts(x)
y
time(y)
is.ts(y)
####
t=tsp(y)
t
plot.ts(y,col="red")
```
```{r}
plot.ts(y,col="red",type="b")
```
```{r}
z=ts(y,freq=4)
time(z)
plot.ts(z,col="red")
```
```{r}
frequency(z)
length(z)
m=tapply(z, cycle(z), mean)
m
```

```{r}
set.seed(123)
x=rnorm(100)
plot.ts(x,col="red")
```
```{r}
shapiro.test(x)
acf(x)
lag.plot(x)
```
#exercice 3 

```{r}
n=160
i=1:n
t=i/n
ep=rnorm(n)
X=1 + 5*t + 2*cos((2*pi*n*t)/8)+ep
plot.ts(X,col="red",main="plot(X)")
```

```{r}
#plot fonction d'autocorrélation
acf(X)
```

```{r}
Z=ts(X,frequency=12)
cc1=decompose(ts(X,frequency = 2),type = "additive")
plot(cc1)
```
```{r}
cc2=stl(ts(X,frequency = 2),s.window = 8)
plot(cc2)
```
```{r}
plot(cc1$trend)
```
```{r}
plot(cc2$time.series[,"trend"])
```
```{r}
plot(cc1$seasonal)

```

```{r}
plot(cc2$time.serie[,"seasonal"])
```
```{r}
hist(X,breaks = 20)
```


#filtrage d'une série temporelle :filtre des différences
```{r}
fil=diff(X,lag=1,differences =1)
plot(fil)
lag.plot(fil)
plot.ts(fil)
```
```{r}
acf(fil)
length(fil) #longeur de cette série 
```

```{r}
fil1=diff(X,lag=8,differences =1)
lag.plot(fil1)
plot.ts(fil1)
acf(fil1)
length(fil1)
#quand on augmente l'ordre d de différenciation la langeur de la série baisse
```
```{r}
Y= 1 + 2*t + 10 *t^2 + 2*cos((2*pi*n*t)/8)+ep
fil2=diff(Y,lag=1,differences =1)
acf(fil2)
lag.plot(fil2)
plot.ts(fil2)
```

```{r}
fil3=diff(Y,lag=8,differences =1)
acf(fil3)
lag.plot(fil3)
plot.ts(fil3)
```
#filtres de  moyenne mobile
```{r}
y=filter(X,sides = 1, method = "convolution",filter = rep(1, 8)/8)
y1=filter(X, method = "recursive",filter = rep(1, 8)/8)
acf(na.omit(y1))
acf(na.omit(y))
```












