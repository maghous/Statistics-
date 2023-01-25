#Exercice 1:

x=c(4,3,7,1.7,3,8.4,5,12,9,12)
is.ts(x)
y=as.ts(x)
y
time(y)
is.ts(y)

t=tsp(y)
plot.ts(y)
plot.ts(y, type="b")
z=ts(y,freq=4)
z
time(z)
z=ts(x,freq=4,1991+1/4, 1993)

time(z)
plot.ts(z)
frequency(z)
length(z)
m=tapply(z,cycle(z),mean)


#Exercice 2:
x<-rnorm(100)
plot.ts(x)
shapiro.test(x)
plot(acf(x))

#Exercice3:

n=160
tendance<-function(t){
  1+5*t
}
saisonnier<-function(t){
  2*cos((2*pi*n*t)/4)
}

epsilon<-rnorm(n)

Serie<-function(t){
  tendance(t)+saisonnier(t)+epsilon
}
x<-Serie((1:n)/n)
y<-ts(x,freq=4,1997+3/4)
plot.ts(y)

cc1=decompose(y, type="additive")
plot(cc1)
cc2=stl(y,s.window=4)
plot(cc2)
plot(cc1$trend)
plot(cc2$time.series[,"trend"])
plot(cc1$seasonal)
plot(cc2$time.series[,"seasonal"])

#Exercice 4:Donn�es de la temperature globale
#1)
X=read.table("monthly_csv.csv", header=T,sep=",", dec=".")
Y<- X[X$Source=='GISTEMP',]
Y<-Y[order(Y$Date),]
H<-ts(Y$Mean,freq=12)
#2)
plot.ts(H)
#3)
res1=decompose(H, type="additive")
tendance1=na.omit(res1$trend)
saison1=na.omit(res1$seasonal)
eps1=na.omit(res1$random)

res2=stl(H,s.window=12,s.degree=0)
tendance2=na.omit(res2$time.series[,"trend"])
saison2=na.omit(res2$time.series[,"seasonal"])
eps2=res2$time.series[,"remainder"]

J1=tendance1 + saison1 + eps1
J2=tendance2 + saison2 + eps2
#4)
monthplot(J1)
boxplot(J1~cycle(J1), xlab="month")

monthplot(J2)
boxplot(J2~cycle(J2), xlab="month")

#5)
lag.plot(rev(tendance1),4)
lag.plot(rev(saison1),4)
lag.plot(rev(eps1),4)
lag.plot(rev(J1),4)



#Exercice 1:Test de blancheur
#Simulation:
epsn=rnorm(100)
#Afficher le corr�logramme
r=acf(epsn)
# La s�rie epsn est-elle un bruit blanc?
# Oui il s'agit d'un bruit blanc

yn=epsn[1:99]+epsn[2:100]
#Quelle est la distribution th�otique de yn?
hist(yn)
shapiro.test(yn)
#On trouve que yn suit une loi normale, car on a une p-valeur sup�rieur � 0.05
qqnorm(yn)
#Afficher le corr�logramme de yn
ryn=acf(yn)
#Calcul du coefficient de corr�lation empirique pour h=1:
rho=ryn$acf[2]
#On trouve une valeur similaire � la valeur th�orique 0.5!

#Tracer le logplot:
lag.plot(yn,2)

#Deuxi�me partie de l'exercice:
epsu=runif(100,-sqrt(3),sqrt(3))
variance_th�o=((2*sqrt(3))**2)/12
hist(epsu)
qqplot(seq(-sqrt(3),sqrt(3),length=100),epsu)

#Le test de Blancheur de Box-Pierce:
Box.test(epsn)


#Exercice 2:
tendance=1+5*((1:100)/100)
saisonnier=2*cos(2*pi*((1:100))/5)
epsilon=rnorm(100)
X=ts(tendance + saisonnier + epsilon, frequency = 5)
#1) Tracer la s�rie:
plot(X)
#2)Tracer la s�rie filtr�e:
filt=rep(1/5,5)
Y=filter(X,filter=filt,sides=2)
plot(Y)

#3) Tracer la s�rie filtrer Y2:
X_t5=diff(X,lag=5)
Y2=X-X_t5
plot(Y2)
#4) Enlever la tendance de X:
Z=X-Y

#5)Estimer la saisonnalit�:
s=tapply(Z,cycle(Z),mean,na.rm=T)
S=s-mean(s)
plot(S)

R�sidu=X-Y-rep(S,100/5)
plot(R�sidu)

#Comparer avec les r�sultats donn�es par decompose(X,type='add') et stl(..):
decompose(X,type='add')
#On remarque que decompose donne le m�me r�sultat qu'on a trouv� pr�c�dement
stl(X,s.window=5)
#Avec stl on trouve un r�sultat �volu�.

#Exercice 3: Simulation d'un processus MA(q)
epsilon1=rnorm(1001)
X1=ts(epsilon1[2:1001]-(1/3)*epsilon1[1:1000],frequency = 200)
plot(X1)
lag.plot(X1,2)
acf(X1)
##################################################################
##################################################################
#1)Charger les donn�es et afficher les statistiques descriptives
library(datasets)
library(aTSA)
X<-AirPassengers
summary(X)

#2)Repr�senter graphiquement la s�rie X:
plot.ts(X)
monthplot(X)
#On remarque la s�rie est non stationnaire.
#On remarque la pr�sence de l'effet de saisonnalit�.
#Pour tester la stationnarit� on effectue le test de dickey-fuller:
adf.test(X)
#On obtient des p-valeur sup�rieure � 0.05, donc on maintient l'hypoth�se nulle.

#3) Estimation param�trique de la tendance:
#(a)Estimer les param�tres de la tendance lin�aire par la m�thode des moindres carr�s:
t=time(X)
model=lm(X~t)
summary(model)

#(b)Tracer la s�rie X et la droite de la tendance estim�e:
plot.ts(X)
abline(-62055.90728,31.88521)
#Supprimer la tendance et tracer la s�rie obtenue
B=X-(-62055.90728)-31.88521*t
plot.ts(B)
acf(B)
#On �limine la saisonalit�:
G=decompose(X,type="additive")
Eps=B-G$seasonal
mean(G$random,na.rm=T)

D=stl(X,s.window="periodic")
residus=D$time.series[,3]
mean(residus)

#V�rification
tendance=D$time.series[,2]
saison=D$time.series[,1]
Y=X-tendance-saison
mean(Y)




#Pour cette partie on trouve que la moyenne est nulle.

#(c)Calculer et tracer la fonction d'auto-corr�lation de la s�rie des r�sidus:
A=acf(Eps)
plot(A)

#########################################################################
#########################################################################
#Exo 1: Estimation de la s�rie et lissage exponentiel pour l'indice CAC
#1-Charger les donn�es EuStockMarkets:
library(datasets)
library(forecast)
data<-EuStockMarkets
plot.ts(data[,1],main='Graphe de DAX')
plot.ts(data[,2],main='Graphe de SMI')
plot.ts(data[,3],main='Graphe de CAC')
plot.ts(data[,4],main='Graphe de FTSE')
CAC40<-data[,3]
#2-S�lectionner une sous-s�rie des valeurs de 1991 � 1997:
data2<-window(data,start=1991,end=1998)

#3-Faire un lissage exponentiel simple:
xsmooth=HoltWinters(data2[,3],alpha=0.1, beta=FALSE,gamma=FALSE)

#4-Faire varier alpha:
#Pour alpha=0.1
xpred<-predict(xsmooth,n.ahead=1)
plot(xsmooth,xpred,main="prediction pour alpha=0.1")

#Pour alpha=0.3
xsmooth=HoltWinters(data2[,3],alpha=0.3, beta=FALSE,gamma=FALSE)
xpred<-predict(xsmooth,n.ahead=1)
plot(xsmooth,xpred,main="prediction pour alpha=0.3")

#Pour alpha=0.7
xsmooth=HoltWinters(data2[,3],alpha=0.7, beta=FALSE,gamma=FALSE)
xpred<-predict(xsmooth,n.ahead=1)
plot(xsmooth,xpred,main="prediction pour alpha=0.7")

#Pour alpha=0.9
xsmooth=HoltWinters(data2[,3],alpha=0.9, beta=FALSE,gamma=FALSE)
xpred<-predict(xsmooth,n.ahead=1)
plot(xsmooth,xpred,main="prediction pour alpha=0.9")

#Pour alpha=Null
xsmooth=HoltWinters(data2[,3],alpha=NULL, beta=FALSE,gamma=FALSE)
xsmooth$alpha

#On remarque que plus on augmente alpha les deux graphes se coincident.

#5-Utiliser la m�thode de lissage Holt-Winters non-saisonni�re avec gamma=FALSE:
xsmooth<-HoltWinters(data2[,3],gamma=FALSE)
xpred<-predict(xsmooth,n.ahead=10)
plot(xsmooth,xpred,main="prediction")

#6-Pour la comparaison, on compare les valeurs predict "xpred" et la commande suivante:
CAC40[time(CAC40)>=1998][1:10]
w=0
z=window(CAC40,start=c(1998,1))
for (i in 1:169){
  if (abs(z[i]-predict(xsmooth,n.ahead=169)[i])<100){
    w=w+1
  }
  else(break)
}
#On peut aller jusqu'� 5 valeurs.


#Exercice 2:
#1-Simuler 50 r�alisations de longueur 105:
X<-ts(array(rep(0,105*50),c(105,50)))

for (i in 1:50){
  X[,i]=arima.sim(n=105,model=list(ar=c(1,-0.5,1/3)))
}

#2-Extraire les 100 premi�res valeurs et estimer les param�tres d'un AR(3):

Y<-array(rep(0,50*4),c(4,50))
for (i in 1:50){
  for (j in 1:4){
    Y[j,i]<-arima(X[,i][1:100],order=c(3,0,0))$coef[j]
  }
}

#3-Pour chaque simulation, pr�dire les cinq valeurs suivantes:

Z<-array(rep(0,50*5),c(5,50))
for (i in 1:50){
  for (j in 1:5){
    Z[j,i]<-predict(arima(X[,i][1:100],order=c(3,0,0)),n.ahead=5)$pred[j]
  }
}

#Donnez une estimation de l'erreur moyenne (biais) et de la variance de l'erreur
# de pr�vision � 1,2,3,4 et 5 pas:

#Pour 1 pas:
#Biais
mean(Z[1,]-X[101,])
#On trouve que le biais �gale � 0.138

#Variance
var(Z[1,]-X[101,])
# On trouve que la variance �gale � 1.110

#A 2 pas:
#Biais
mean(Z[2,]-X[102,])
#On trouve que le biais �gale 0.216

#Variance
var(Z[2,]-X[102,])
# On trouve que la variance �gale 2.486

#A 3 pas:
#Biais
mean(Z[3,]-X[103,])
# On trouve que le biais �gale 0.274

#Variance
var(Z[3,]-X[103,])
#On trouve que la variance �gale 2.446

#A 4 pas:
#Biais
mean(Z[4,]-X[104,])
#On trouve le biais �gale 0.067

#Variance
var(Z[4,]-X[104,])
#On trouve la variance �gale 2.836

#A 5 pas
#Biais
mean(Z[5,]-X[105,])
#On trouve le biais �gale -0.033

#Variance
var(Z[5,]-X[105,])
#On trouve la variance �gale 2.886

#5) On recommence en rallongeant la dur�e d'observation:

#1-Simuler 50 r�alisations de longueur 205:
X1<-ts(array(rep(0,205*50),c(205,50)))

for (i in 1:50){
  X1[,i]=arima.sim(n=205,model=list(ar=c(1,-0.5,1/3)))
}

#2-Extraire les 200 premi�res valeurs et estimer les param�tres d'un AR(3):

Y1<-array(rep(0,50*4),c(4,50))
for (i in 1:50){
  for (j in 1:4){
    Y1[j,i]<-arima(X1[,i][1:200],order=c(3,0,0))$coef[j]
  }
}

#3-Pour chaque simulation, pr�dire les cinq valeurs suivantes:

Z1<-array(rep(0,50*5),c(5,50))
for (i in 1:50){
  for (j in 1:5){
    Z1[j,i]<-predict(arima(X1[,i][1:200],order=c(3,0,0)),n.ahead=5)$pred[j]
  }
}

#Pour 1 pas:
#Biais
mean(Z1[1,]-X1[201,])
#On trouve que le biais �gale � 0.238

#Variance
var(Z1[1,]-X1[201,])
# On trouve que la variance �gale � 1.105

#A 2 pas:
#Biais
mean(Z1[2,]-X1[202,])
#On trouve que le biais �gale 0.360

#Variance
var(Z1[2,]-X1[202,])
# On trouve que la variance �gale 1.852

#A 3 pas:
#Biais
mean(Z1[3,]-X1[203,])
# On trouve que le biais �gale 0.448

#Variance
var(Z1[3,]-X1[203,])
#On trouve que la variance �gale 2.308

#A 4 pas:
#Biais
mean(Z1[4,]-X1[204,])
#On trouve le biais �gale 0.346

#Variance
var(Z1[4,]-X1[204,])
#On trouve la variance �gale 2.618

#A 5 pas
#Biais
mean(Z1[5,]-X1[205,])
#On trouve le biais �gale 0.213

#Variance
var(Z1[5,]-X1[205,])
#On trouve la variance �gale 2.897
#Exercice 3:
D<-scan(file="sanfran.dat.txt",skip=1)
D<-matrix(D,nrow=70,ncol=6)
D<-data.frame(D)
colnames(D)=c("MONTHLY PRECIPITATION", "MM", "SOUTHWESTERN", "MOUNTAIN", "REGION", "1932-1966" )

Yts <- ts(D$`1932-1966`,start=c(1932,1),end=c(1966,12),frequency=12)
data2=window(Yts,start =c(1933,1),end=c(1963,12))


