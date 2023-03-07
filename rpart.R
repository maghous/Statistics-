library(ggplot2)
library(readr)
library(caret)
library(randomForest)
library(caTools)
library(rpart.plot)

df<-read.csv("mushrooms.csv")
names(df)
summary(df)

z<-cbind.data.frame(Var=names(df), Total_Class=sapply(df,function(x){as.numeric(length(levels(x)))}))
print(z)

df$veil.type<-NULL
set.seed(123) 
sample = sample.split(df$class, SplitRatio = .7)
x_train = subset(df, sample == TRUE)
x_test = subset(df, sample == FALSE)

y_train<-x_train$class
y_test <- x_test$class

x_train$class<-NULL
x_test$class<-NULL

cv.10.folds<-createMultiFolds(y_train,k=10,times=2)

ctrl.1<-trainControl(method="repeatedcv",number=10,repeats=2,index=cv.10.folds)

rf.1.cv<-train(x=x_train,y=y_train,method="rf",trControl=ctrl.1,tuneLength=3)

plot(varImp(rf.1.cv),main="Random Forest - Variable Importance Plot")

y_predicted<-predict(rf.1.cv,x_test)

df1<-data.frame(Orig=y_test,Pred=y_predicted)

confusionMatrix(table(df1$Orig,df1$Pred))


mod3 <-train(x=x_train,y=y_train,method="rpart",trControl=ctrl.1,tuneLength=5)
plot(varImp(mod3),main="RPART - Variable Importance Plot")

rpart.plot(mod3$finalModel)

