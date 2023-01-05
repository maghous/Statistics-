######Exercice 1
data<-mtcars
v1<-mtcars$mpg
v2<-mtcars$am
library(exactRankTests) # on utilise cette library pour avoir une valuer de p-velue exacte avec la fonction wilcox.test 
test1<-wilcox.test(mpg~am,data=mtcars,alternative="less")
test1<-wilcox.test(mpg~am,data=mtcars,alternative="two.sided")
test2<-kruskal.test(mpg~am,data=mtcars)
###################################
############Exercice 2#############
avant<-c(45,36,47,40,45,35,36,50,50,40,40,30,45,30,45,40,50,40,50,40,55,30,40,40,38,35,40,35,38,50,45,30,38)
apres<-c(48,40,53,40,46,30,40,60,60,40,40,35,50,40,50,45,50,40,45,35,50,35,45,35,35,40,45,37,35,50,50,33,38)
test3<-wilcox.test(avant,apres,paired=T)
wilcox.exact(avant,apres,paired = T) # pour calculer la p-value exacte donc on rejette H0 
####################################
#########exercice 3
#question1
library(datasets)
library(airquality)
X<-(airquality$Temp -32)*(5/9)
qqnorm(X)
qqline(X)
#on va tester la normalité des données 
shapiro.test(X) #p-value inf à 0.05 donc on rejette h0 c'est à dire les données ne sont pas gaussiennes 
#question2
toto<-airquality[airquality$Month <=7 & airquality$Month >=5,] # les données pour mai--juillet 
pipo<-airquality[airquality$Month <=9 & airquality$Month >=8,] # les données pour aout--septembre 
temp1<-toto$Temp
temp2<-pipo$Temp
#on va visualiser les données 
hist(temp1)
hist(temp2)
#on va tester l'homogénéité pour la premiere variable 
R1<-rank(temp1)
y<-1:length(temp1)
cor.test(R1,y,method = "spearman") # on rejette H0
cor.test(R1,y,method = "kendall") # on rejette H0
#on va tester l'homogénéité pour la deuxieme variable 
R2<-rank(temp2)
y<-1:length(temp2)
cor.test(R2,y,method = "spearman") # on rejette H0
cor.test(R2,y,method = "kendall") # on rejette H0
#Donc les données ne sont pas homgenes 
#on va creer des accroissement de temparture pour les deux périodes 
#first period
Xp<-diff(toto$Temp)
Yp<-diff(pipo$Temp)
#on effectue un test de comparaison 
ks.test(Xp,Yp,alternative='two.sided') #on ne rejette pas h0
ks.test(Xp,Yp,alternative='less') #on ne rejette pas h0
#on effectute un test de kruskal walis
kruskal.test(Temp ~ Month, data = airquality) #on rejette h0
kruskal.test(Month ~ Temp, data = airquality) #on rejette h0
#######################################
##################exercice 4
n=500
X=exp(rnorm(n,0,1))
Y=rgamma(n,exp(0.5)*0.46,rate=0.46)
#on va calculer la moyenne de ces 2 échantillons 
mx<-mean(X);my<-mean(Y)
#on va calculer la mediane de ces 2 échantillons 
medx<-median(X) ;medy<-median(Y)
#La médiane théorique d'une v.a. est le quantile théorique d'ordre 0.5
medxth<-quantile(X,0.5) ; medyth<-quantile(Y,0.5)  #donc lles deux medianes sont égaux
# on calcule variance empirique 
Vx<-var(X) ;Vy<-var(Y)
var.test(X, Y)
shapiro.test(X)
shapiro.test(Y)
#on  trace les fonctions de rép 
#pour la loi lognormal
x_plnorm <- seq(0, 10, by = 0.01) 
y_plnorm <- plnorm(x_plnorm) 
plot(y_plnorm,ylab = "F(x)")
curve(pnorm(x, 5, 1.5), 0.5, 9.5, ylab = "FX(x)")
#pour la loi gamma 
x_pgamma <- seq(0, 20, by = 0.01)   
y_pgamma <- pgamma(x_pgamma,exp(0.5)*0.46,rate=0.46) 
plot(y_pgamma)
#on applique le test de student 
t.test(X,Y) #donc il y a une probabilité de 82.08% que les deux échantillons soient d'une même population
#On applique le test de wilcoxon de comparaison
wilcox.test(X, Y, paired = TRUE) #on remarque que Les deux échantillons x et y ne sont pas significativement différents.
#on répete 500 fois les étapes 1-4
n=500
v<-rep(0,n) #pour le test de student 
vv<-rep(0,n) #pour le test de wilcoxon de comparaison
a<-rep(0,n) #pour le test de variance 
for (i in 1:n){
  X=exp(rnorm(n,0,1))
  Y=rgamma(n,exp(0.5)*0.46,rate=0.46)
  a[i]<-var.test(X,Y)$p.value
  v[i]<-t.test(X,Y)$p.value
  vv[i]<-wilcox.test(X,Y,paired = T)$p.value
  a
  v
  vv

  }

nba<-length(a[a<0.05])
nbv<-length(v[v<0.05])
nbvv<-length(vv[vv<0.05])
#Le risque (de première espèce) est le pourcentage de chances de rejeter H0,
err1<-nba/n
err2<-nbv/n
err3<-nbvv/n
#je dois faire des conclusions 
n=25
X=exp(rnorm(n,0,1))
Y=rgamma(n,exp(0.5)*0.46,rate=0.46)
#pour la loi lognormal
x_plnorm <- seq(0, 25, by = 1) 
y_plnorm <- plnorm(x_plnorm) 
plot(y_plnorm,ylab = "F(x)")
#pour la loi gamma 
x_pgamma <- seq(0, 25, by = 1)   
y_pgamma <- pgamma(x_pgamma,exp(0.5)*0.46,rate=0.46) 
plot(y_pgamma)
st1<-t.test(X,Y)
st1$p.value 
wc1<-wilcox.test(X,Y)
wc1$p.value
################
n=100
X=exp(rnorm(n,0,1))
Y=rgamma(n,exp(0.5)*0.46,rate=0.46)
#pour la loi lognormal
x_plnorm <- seq(0, n, by = 1) 
y_plnorm <- plnorm(x_plnorm) 
plot(y_plnorm,ylab = "F(x)")
#pour la loi gamma 
x_pgamma <- seq(0, n, by = 1)   
y_pgamma <- pgamma(x_pgamma,exp(0.5)*0.46,rate=0.46) 
plot(y_pgamma)
st2<-t.test(X,Y)
st2$p.value 
wc2<-wilcox.test(X,Y)
wc2$p.value
#######################
################
n=500
X=exp(rnorm(n,0,1))
Y=rgamma(n,exp(0.5)*0.46,rate=0.46)
#pour la loi lognormal
x_plnorm <- seq(0, n, by = 1) 
y_plnorm <- plnorm(x_plnorm) 
plot(y_plnorm,ylab = "F(x)")
#pour la loi gamma 
x_pgamma <- seq(0, n, by = 1)   
y_pgamma <- pgamma(x_pgamma,exp(0.5)*0.46,rate=0.46) 
plot(y_pgamma)
st2<-t.test(X,Y)
st2$p.value 
wc2<-wilcox.test(X,Y)
wc2$p.value


