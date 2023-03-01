library(ggplot2)
library(randomForest)
library(e1071)
library(ROCR)
library(caTools)
library(readr)
library(caret)
library(corrplot)
library(dplyr)

df<-read.csv("data.csv")
attach(df)
view(df)
names(df)
summary(df)
dim(df)
sum(is.na(df))
df$diagnosis <-as.factor(df$diagnosis)

table(df$diagnosis)

dev.new(width = 550, height = 330, unit = "px")
corrplot(cor(df[,3:32]))


set.seed(123)

df$sp <- sample.split(df$diagnosis,SplitRatio=0.75)

train_df <- subset(df, df$sp==TRUE)

test_df <- subset(df, df$sp==FALSE)

lr <- glm(diagnosis ~ ., data = train_df[,2:32], family=binomial(link='logit'))
predicted_lr <- predict(lr, test_df, type='response')
predicted_lr <- ifelse(predicted_lr > 0.5,1,0)
table(test_df$diagnosis, predicted_lr)

misclassification_error<-mean(predicted_lr != test_df$diagnosis)
lracc<-(1-misclassification_error)*100
print(paste('Logistic Regression Accuracy(in %)',lracc))


rf <- randomForest(as.factor(diagnosis) ~ ., data=train_df[,2:32], nsize=20, ntree=300)

predicted_rf <- predict(rf, test_df)
print("Confusion Matrix is as follows:")
table(test_df$diagnosis,predicted_rf)
misclassification_error<-mean(predicted_rf != test_df$diagnosis)
rfacc<-(1-misclassification_error)*100
print(paste('Random Forest Accuracy(in %)',rfacc))


predict_lr_ROC <- predict(lr, test_df, type="response")
pred_lr <- prediction(predict_lr_ROC, test_df$diagnosis)
final_lr <- performance(pred_lr, "tpr", "fpr")

predict_rf_ROC <- predict(rf, test_df, type="prob")
pred_rf <- prediction(predict_rf_ROC[,2], test_df$diagnosis)
final_rf <- performance(pred_rf, "tpr", "fpr")



print('Area under the Receiver Operating Characteristic (ROC) curve: ')
auc_lr <- performance(pred_lr,"auc")
auc_lr <- round(as.numeric(auc_lr@y.values),3)

auc_rf <- performance(pred_rf,"auc")
auc_rf <- round(as.numeric(auc_rf@y.values),3)





print(paste('AUC of Logistic Regression:',auc_lr))
print(paste('AUC of Random Forest:',auc_rf))


plot(final_lr, main = "ROC curves for the models", col='red')
plot(final_rf, add=TRUE, col='green')

legend('bottom', c("Logistic Regression", "Random Forest"), fill = c('red','green'), bty='n')


X<-c('Logistic Regression','Random Forest')
Y<-c(lracc,rfacc) 
barplot(Y, names.arg = X, xlab = "Classifiers", ylab = "Accuracy", col = "blue",
        main = "Accuracy of different Classifiers", border = "red")


