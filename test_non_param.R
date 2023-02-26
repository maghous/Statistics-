#Kolmogorov smirnov
X=c(1.6,10.3,3.5,13.5,18.4,7.7,24.3,10.7,8.4,4.9,7.9,12.0,16.2,6.8,14.7)
n=length(X)

#avec la commande ks.test
T=ks.test(X,"pexp",0.1)
T$statistic
T$p.value

#calcul direct de la statistique 
F=1-exp(-sort(X)/10)
Femp=(1:n)/n
Femp2=0:(n-1)/n
D=max(abs(F-Femp),abs(F-Femp2))
K=sqrt(n)*D
#le quatile d'ordre 0.05 est 0.3376

#test de normalité 

n=100
X=rnorm(n,4,sqrt(3))

qqnorm(X,lwd=2)
qqline(X,lty=2,col="yellow")


## tests d'hypothese d'adequation a une loi normal specifiee
T1=ks.test(X,"pnorm",4,sqrt(3))
T1$statistic
T1$p.value
T2=ks.test(X,"pnorm",4,1)
T2$statistic
T2$p.value
T3=ks.test(X,"pnorm",1,sqrt(5))
T3$statistic
T3$p.value


## les tests cdv AD, lilliefors et shapiro-wilk
library(nortest)
T_AD=ad.test(X)
T_AD$statistic
T_AD$p.value
T_cvm=cvm.test(X)
T_cvm$statistic
T_cvm$p.value
T_lillie=lillie.test(X)
T_lillie$statistic
T_lillie$p.value
T_shapiro=shapiro.test(X)
T_shapiro$statistic
T_shapiro$p.value
#les 4 tests acceptent l'hypthèse de normalité 
T_ks=ks.test(X,"pnorm")
T_ks$p.value
#on rejette l'hypothèse de normalité 

n=1000

X=rnorm(1000,4,sqrt(3))

qqnorm(X,lwd=1)
qqline(X,lty=2,col="red")
# on doit observer une augmentation de la p-valuer pour T1 et diminution pour T2 et T3 
# par rapport au cas de n=100

T1=ks.test(X,"pnorm",4,sqrt(3))
T1$statistic
T1$p.value
T2=ks.test(X,"pnorm",4,1)
T2$statistic
T2$p.value
T3=ks.test(X,"pnorm",1,sqrt(5))
T3$statistic
T3$p.value

T_AD=ad.test(X)
T_AD$statistic
T_AD$p.value
T_CVM=cvm.test(X)
T_CVM$statistic
T_CVM$p.value
T_lillie=lillie.test(X)
T_lillie$statistic
T_lillie$p.value
T_shapiro=shapiro.test(X)
T_shapiro$statistic
T_shapiro$p.value
T_KS=ks.test(X,"pnorm")
T_KS$statistic
T_KS$p.value


set.seed(123)
X<-rlnorm(20,0,0.4)
qqnorm(X,lwd=2)
qqline(X,col="red",lty=2)
T=shapiro.test(X)
T$statistic
T$p.value
#le test accepte la normalité des donnes log-normales (non gaussienne)

X<-rbinom(100,5,0.6)
qqnorm(X,lwd=2)
qqline(X,col="red",lty=2)
T=shapiro.test(X)
T$statistic
T$p.value
#on accepte pas la normalité 

n=10000
Y=rt(n,200)

qqnorm(Y,lwd=2)
qqline(Y,col=3,lty=2)

hist(Y,breaks = 30,col="blue")

T1=cvm.test(Y)
T1$statistic
T1$p.value

T2=lillie.test(Y)
T2$statistic
T2$p.value

T3<-ad.test(Y)
T3$statistic
T3$p.value

#les trois tests acceptent l'hypothese de normalite 
T4<-ks.test(Y)
T4$statistic
T4$p.value
#ce test accepte l'hypothese H_0: Y_i~N(0,1)

T5<-shapiro.test(Y)
T5$statistic
T5$p.value
#le test de shapiro ne fonctionne pas pour n>5000


mu=3;sigma=2
n=1000
X=rnorm(n,mu,sigma)
Z=rep(n,0)
for (i in 1:(n-1)) {
  Z[i]=(sum(X[1:i])-i*X[i+1])/sqrt(i*(i+1))
  
}
T=ks.test(Z,"pnorm",0,sigma)
T$statistic
T$p.value

#le nombre de simulation
s=100

simulationU <- function(k,S,n) {
  U=rep(S,0)
  mu=3
  sigma=2
  for (s in 1:S)
  {
    X=rnorm(n,mu,sigma)
    Z=rep(n,0)
    for (i in 1:(n-1))
      Z[i]=(sum(X[1:i])-i*X[i+1])/sqrt(i*(i+1))
    Y=sqrt(k)*Z[k+1]/sqrt(sum((Z[1:k])^2))
    U[s]=pt(Y,k)
  }
  return(U)
}  
#on prend par exemple k=2
UU=simulationU(2,1000,100)	
hist(UU,col = "red")
t2<-ks.test(UU,"punif",0,1)
t2$statistic
t2$p.value

# Donc les variables aleatoires Y suivent la loi de Student(k) pour chaque k donné
# Les v.a. U suivent la loi uniforme.
# Le test de la normalite avec les parametres non-specifiés est basé sur les v.a. U:


n=100 ;mu=3;sigma=2
X=rnorm(n,mu,sigma)
Z=rep(n-1,0)
for (i in 1:(n-1)){
  Z[i]=(sum(X[1:i])-i*X[i+1])/sqrt(i*(i+1))
}

Y=rep(n-2,0)
U=rep(n-2,0)
for (k in 1:(n-2))	
{
  Y[k]=sqrt(k)*Z[k+1]/sqrt(sum((Z[1:k])^2))
  U[k]=pt(Y[k],k)
}

t2<-ks.test(U,'punif',0,1)
t2$statistic
t2$p.value

#le test de la normalité avec la moyenne non-specifiee et variance données

n=100;mu=3;sigma=2
X=rnorm(n,mu,sigma)
Z=rep(n-1,0)
for (i in 1:(n-1)){
  Z[i]=(sum(X[1:i])-i*X[i+1])/sqrt(i*(i+1))
}
t4<-ks.test(Z,'pnorm',0,sigma)
t4$statistic
t4$p.value


#comparaison des échantillons indépendats 
library(tidyverse)
view(mtcars)
t<-wilcox.test(mpg~am,data=mtcars,alternative="greater")
t$statistic
t$p.value

t<-kruskal.test(mpg~am,data=mtcars)
t$statistic
t$p.value

mpg0=mtcars$mpg[which(mtcars$am==0)]
mpg1=mtcars$mpg[which(mtcars$am==1)]

t1<-wilcox.test(mpg0,mpg1,data=mtcars,alternative = "greater")
t1$statistic
t1$p.value

t2<-t.test(mpg0,mpg1,data=mtcars,alternative = "greater")
t2$statistic
t2$p.value


#comparaison des échantillons appariés 

X=c(45,36,47,40,45,35,36,50,50,40,40,30,45,30,45,40,50,40,50,40,55,30,40,40,38,35,40,35,38,50,45,30,38)
Y=c(48,40,53,40,46,30,40,60,60,40,40,35,50,40,50,45,50,40,45,35,50,35,45,35,35,40,45,37,35,50,50,33,38)

plot(X,Y,lwd=2,col="red")

t<-wilcox.test(X,Y,paired = TRUE,alternative = "g")
t$statistic
t$p.value

library(exactRankTests)
t<-wilcox.exact(X,Y,paired = TRUE,alternative = "g")
t$statistic
t$p.value

library(datasets)
data(airquality)

temp= 5/9 * (airquality$Temp -32)
hist(temp,col = "red")
qqnorm(temp,lwd=2)
qqline(temp,lty=2,col="yellow")

t1<-lillie.test(temp)
t1$statistic
t1$p.value
t2<-ad.test(temp)
t2$statistic
t2$p.value
t3<-cvm.test(temp)
t3$statistic
t3$p.value

Temp_57=temp[which(airquality$Month==5 |airquality$Month==6|airquality$Month==7)]
Temp_89=temp[which(airquality$Month==8 | airquality$Month==9)]

R_57=rank(Temp_57)
n_57=length(R_57)
rho_57=12/(n_57*(n_57^2-1))*sum(R_57*(1:n_57)) -3*(n_57+1)/(n_57-1)

p<-cor.test(R_57,1:n_57,method="spearman",alternative="l",exact=F)
p$statistic
p$p.value
cor.test(R_57,1:n_57,method="kendall",alternative="l",exact=F)

R_89=rank(Temp_89)
n_89=length(R_89)
rho_89=12/(n_89*(n_89^2-1))*sum(R_89*(1:n_89)) -3*(n_89+1)/(n_89-1)

cor.test(R_89,1:n_89,method="spearm",alternative="g")
cor.test(R_89,1:n_89,method="kendall",alternative="g")

wilcox.test(Temp_57,Temp_89)

dt_57=Temp_57[2:n_57]-Temp_57[1:(n_57-1)]
dt_89=Temp_89[2:n_89]-Temp_89[1:(n_89-1)]

cor.test(dt_57,1:(n_57-1),method="spearman")
cor.test(dt_89,1:(n_89-1),method="spearman")
wilcox.test(dt_57,dt_89)

kruskal.test(Temp~Month, data = airquality)

n=500
X=exp(rnorm(n,0,1))
Y=rgamma(n,sqrt(exp(1))*0.46,rate=0.46)
mean(X)
mean(Y)
median(X)
median(Y)
var(X)
var(Y)
plot(X,Y,lwd=2)

t<-t.test(X,Y)
t$p.value
t$statistic

t1<-wilcox.test(X,Y)
t1$statistic
t1$p.value

N=25
x_i=(1:(5*N))/N
X_th=plnorm(x_i,0,1)
Y_th=pgamma(x_i,sqrt(exp(1))*0.46,rate=0.46)

plot(x_i,X_th,col="red",type="l")
lines(x_i,Y_th,col="blue")

n=1000
S=500
Pvalues=matrix(0,nrow=S,ncol=2)
Means=matrix(0,nrow=S,ncol=2)
Medians=matrix(0,nrow=S,ncol=2)

for (i in 1:S)
{
  X=exp(rnorm(n,0,1))
  Y=rgamma(n,sqrt(exp(1))*0.46,rate=0.46)
  Means[i,]=c(mean(X),mean(Y))
  Medians[i,]=c(median(X),median(Y))
  a_t=t.test(X,Y)
  a_w=wilcox.test(X,Y)
  Pvalues[i,1]=a_t$p.value
  Pvalues[i,2]=a_w$p.value
}

sum(Pvalues[,1]<0.05)/S
sum(Pvalues[,2]<0.05)/S


N=100
x_i=(1:(10*N))/N
X_th=plnorm(x_i,0,2)
Y_th=pgamma(x_i,exp(2)*0.03,rate=0.03)
plot(x_i,Y_th,col="blue",type="l",ylim=c(0,1))
lines(x_i,X_th,col="red")

qgamma(1/2,exp(2)*0.003,rate=0.003)
qlnorm(1/2,0,2)

n=1000
X=exp(rnorm(n,0,2))

Y=rgamma(n,0.03*(exp(2)),rate=0.03)
mean(X)
mean(Y)
median(Y)
var(X)
var(Y)


S=500
Pvalues=matrix(0,nrow=S,ncol=2)
Means=matrix(0,nrow=S,ncol=2)
Medians=matrix(0,nrow=S,ncol=2)
n=500
for (i in 1:S)
{
  X=exp(rnorm(n,0,2))
  Y=rgamma(n,0.03*(exp(2)),rate=0.03)
  Means[i,]=c(mean(X),mean(Y))
  Medians[i,]=c(median(X),median(Y))
  a_t=t.test(X,Y)
  a_w=wilcox.test(X,Y)
  Pvalues[i,1]=a_t$p.value
  Pvalues[i,2]=a_w$p.value
}

sum(Pvalues[,1]<0.05)/S
sum(Pvalues[,2]<0.05)/S

library(nCDunnett)
library(DescTools)

Weight=c(728,955,823,1161,972,974,748,937,904,869,548,682,763,617,632,450,405,529,759,397)

Type=rep(c("E1","E2","E3","E4"),each=5)
X=data.frame(Type,Weight)

n=length(X$Weight[X$Type=='E1'])
alpha=0.05
k=4


m1=mean(X$Weight[X$Type=='E1'])
m2=mean(X$Weight[X$Type=='E2'])
m3=mean(X$Weight[X$Type=='E3'])
m4=mean(X$Weight[X$Type=='E4'])

SumVar=4*(var(X$Weight[X$Type=='E1'])+var(X$Weight[X$Type=='E2'])
          +var(X$Weight[X$Type=='E3'])+var(X$Weight[X$Type=='E4']))
S2=SumVar/(4*5-4)


#Student 
a<-t.test(X$Weight[X$Type=='E1'],X$Weight[X$Type=='E2'],var.equal=TRUE,alternative='two.sided')
a$p.value
a1<-t.test(X$Weight[X$Type=='E1'],X$Weight[X$Type=='E3'],var.equal=TRUE,alternative='two.sided')
a1$p.value
a2<-t.test(X$Weight[X$Type=='E1'],X$Weight[X$Type=='E4'],var.equal=TRUE,alternative='two.sided')
a2$p.value

#Dunett
#stat globale

Dunnett_Stat=sqrt(n/2)*max(abs(m2-m1),abs(m3-m1),abs(m4-m1))/sqrt(S2)
# les statistiques individuelles 
Dunnett_Stats=sqrt(n/2)/sqrt(S2)*c(abs(m2-m1),abs(m3-m1),abs(m4-m1))

alpha = 0.05
quantile = qNCDun(1-alpha, nu= k*(n-1), rho=(rep(0.5,times=k-1)), delta=rep(0,times=k-1), two.sided=TRUE)

# p-valeurs des tests
pvalue=1-pNCDun(Dunnett_Stats, nu= k*(n-1), rho=(rep(0.5,times=k-1)), delta=rep(0,times=k-1))

# decision
Test_Dunnett=Dunnett_Stats>quantile

DunnettTest(Weight ~ Type, data = X)

#Tukey
Tukey_Stat=sqrt(n)*(max(m1,m2,m3,m4)-min(m1,m2,m3,m4))/sqrt(S2)
Tukey_Stats=sqrt(n)/sqrt(S2)*c(m1-m2,m1-m3,m1-m4,m2-m3,m2-m4,m3-m4)

quantile=qtukey(1-alpha,k,k*(n-1))
Test_Tukey=Tukey_Stats>quantile
pvalue=1-ptukey(Tukey_Stats,k,k*(n-1))

model=lm(Weight ~ Type, data = X)
ANOVA=aov(model)
TUKEY_test=TukeyHSD(x=ANOVA,conf.level=0.95)

library(mvtnorm)
set.seed(1234)
n=200
p=100
alpha=0.05

mu1=rep(0,p)
mu2=rep(0,p)

X=rmvnorm(n,mean=mu1, sigma=diag(p))
Y=rmvnorm(n,mean=mu2, sigma=diag(p))

Pvalue=rep(0,p)
for (k in 1:p)
{
  T=t.test(X[,k],Y[,k],alternative="two.sided")	
  Pvalue[k]=T$p.value
}
#print("Rejected hypotheses of equality of means:")
#print(which(Pvalue<alpha))
#print("Accepted hypotheses of equality of means:")
#print(which(Pvalue>=alpha))
#print("Upper bound on FWER:")
print("Combien de fois H_0: mu_1^k=mu_2^k est accepté")
sum (Pvalue>alpha)

Pvalue_Bonf=p.adjust(Pvalue, method = "bonferroni")
#print("Accepted hypotheses:")
#print(which(Pvalue_Bonf>=alpha))
#print("Upper bound on FWER:")
sum (Pvalue_Bonf>alpha)

Pvalue_Holm=p.adjust(Pvalue, method = "holm")
#print("Accepted hypotheses:")
#print(which(Pvalue_Holm>=alpha))
sum (Pvalue_Holm>alpha)

#print("Accepted hypotheses:")
#print(which(Pvalue_Holm>=alpha))
sum (Pvalue_Holm>alpha)


print("Accepted hypotheses:")
print(which(Pvalue_FDR>=alpha))
sum (Pvalue_FDR>alpha)

# Type I errors
set.seed(1234)
S=200
p=100
alpha=0.05
FWER=rep(0,3)
FWER_fdr=0
n=200
for (i in 1:S)
{
  mu1=rep(0,p)
  mu2=rep(0,p)
  X=rmvnorm(n,mean=mu1, sigma=diag(p))
  Y=rmvnorm(n,mean=mu2, sigma=diag(p))
  
  Pvalue=rep(0,p)
  for (k in 1:p)
  {
    T=t.test(X[,k],Y[,k])	
    Pvalue[k]=T$	p.value
  }
  
  N_T=sum(Pvalue<alpha)
  FWER[1]=FWER[1]+N_T
  
  Pvalue_Bonf=p.adjust(Pvalue, method = "bonferroni")
  N_Bonf=sum(Pvalue_Bonf<alpha)
  FWER[2]=FWER[2]+N_Bonf
  
  Pvalue_Holm=p.adjust(Pvalue, method = "holm")
  N_Holm=sum(Pvalue_Holm<alpha)
  FWER[3]=FWER[3]+N_Holm
  
  Pvalue_FDR=p.adjust(Pvalue, method = "fdr")
  N_fdr=sum(Pvalue_FDR<alpha)
  FWER_fdr=FWER_fdr+N_fdr
}

FWER/S
FWER_fdr/S

# Type II errors
set.seed(1234)
S=200
alpha=0.05
Err2=rep(0,3)
Err2_fdr=0
p=100
n=200
for (i in 1:S)
{
  mu1=rep(0,p)
  mu2=0.75*rep(1,p)
  X=rmvnorm(n,mean=mu1, sigma=diag(p))
  Y=rmvnorm(n,mean=mu2, sigma=diag(p))
  
  Pvalue=rep(0,p)
  for (j in 1:p)
  {
    T=t.test(X[,j],Y[,j])	
    Pvalue[j]=T$p.value
  }
  
  N_T=sum(Pvalue>alpha)
  Err2[1]=Err2[1]+N_T
  
  Pvalue_Bonf=p.adjust(Pvalue, method = "bonferroni")
  N_Bonf=sum(Pvalue_Bonf>alpha)
  Err2[2]=Err2[2]+N_Bonf
  
  Pvalue_Holm=p.adjust(Pvalue, method = "holm")
  N_Holm=sum(Pvalue_Holm>alpha)
  Err2[3]=Err2[3]+N_Holm
  
  Pvalue_FDR=p.adjust(Pvalue, method = "fdr")
  N_FDR=sum(Pvalue_FDR>alpha)
  Err2_fdr=Err2_fdr+N_FDR
}


Err2/S

Err2_fdr/S





