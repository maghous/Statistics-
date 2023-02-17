set.seed(123)
n=50
X=rnorm(n)
Y=rbinom(n,size=1,prob=
           exp(2*X-1)/(1+exp(2*X-1)))
B=data.frame(Y,X)
reg=glm(Y~X,family=binomial,data=B)
S=predict(reg,type="response")

plot(0:1,0:1,xlab="positive rate",ylab="True positve",cex=0.5)
for (s in seq(0,1,.01)) {
  ps=(S>s)*1
  fp=sum((ps==1)*(Y==0))/sum(Y==0)
  tp=sum((ps==1)*(Y==1))/sum(Y==1)
  points(fp,tp,cex=.5,col="red")
}

#courbe roc
fp=tp=rep(NA,101)
plot(0:1,0:1,xlab="positive rate",ylab="True positve",cex=0.5)
for (s in seq(0,1,.01)) {
  ps=(S>s)*1
  fp[1+s*100]=sum((ps==1)*(Y==0))/sum(Y==0)
  tp[1+s*100]=sum((ps==1)*(Y==1))/sum(Y==1)
  
}
lines(c(fp),c(tp),type = "s",col="green")


library(ROCR)
pred=prediction(ps,Y)
pref=performance(pred,"tpr","fpr")
plot(pref)









