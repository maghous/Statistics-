data<-read.csv("winequality-red.csv")
attach(data)
names(data)
str(data)
summary(data)
data$quality
#si quality > 5 -> 1 sinon 0
data$quality<-ifelse(data$quality>5,1,0)
data$quality<-as.factor(data$quality)
#on prépare le training test
index<-sample(1:nrow(data),size = 0.8*nrow(data))
train<-data[index,]
test<-data[-index,]
library(randomForest)
rf<-randomForest(quality~.,data=train)
prd<-predict(rf,test,type="prob")
lr<-glm(quality~.,data=train,family = "binomial")
pred1<-predict(lr,test,type="response")
library(pROC)
ROC_rf<-roc(test$quality,prd[,1])
ROC_lr<-roc(test$quality,pred1)
rf_auc<-auc(ROC_rf)
rl_auc<-auc(ROC_lr)
plot(ROC_lr,col="green",ylim=c(0,1))
lines(ROC_rf,col="orange")
paste("Accuracy % of random forest: ", mean(test$quality == round(prd[,2], digits = 0)))
paste("Accuracy % of logistic regression: ", mean(test$quality == round(pred1, digits = 0)))
paste("Area under curve of random forest: ", round(rf_auc,4))
paste("Area under curve of logistic regression: ",round(rl_auc,4))
