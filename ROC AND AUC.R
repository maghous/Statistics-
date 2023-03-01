library(tidyverse)
library(caret)
library(ROCR)
head(iris)

df=iris
attach(df)
table(Species)
names(df)
df$Species_binary<-rep(0,150)
df <- df %>% mutate(Species_binary =ifelse(Species == "setosa",1,0))

df$Species_binary <- as.factor(df$Species_binary)
head(df)

set.seed(123)
set.seed(1) 
iris_model <- train(Species_binary ~Sepal.Length, method = "glm", data = df, 
                    family = binomial(link = "logit"),
                    trControl = trainControl(method = 'cv', number = 5))

#prediction probleme
p<-predict(iris_model,df,type="prob")
print(p)
# Create predictions with threshold 0.5
predicted_05 <-as.factor(ifelse(p[,2]>0.5,1,0))
# Create confusion matrix with threshold 0.5
confm_05 <- confusionMatrix(predicted_05, df$Species_binary, positive = "1")
confm_05

plot_logistic <- as.data.frame(confm_05$table)

plot_logistic$Prediction <- factor(plot_logistic$Prediction, 
                                   levels=rev(levels(plot_logistic$Prediction)))
plot_conf_05 <- ggplot(plot_logistic, aes(Prediction, Reference,  fill= Freq)) +
  geom_tile() + geom_text(aes(label=Freq)) + theme(legend.position = "None") +
  ggtitle("Confusion Matrix with Threshold 0.5") +
  scale_fill_gradient(low="white",high = "grey") + theme_light()

plot_conf_05
#true positive rate
TTP<-plot_logistic$Freq[4]/(plot_logistic$Freq[4] + plot_logistic$Freq[3])
print(TTP)
#false positive rate
FPT<-plot_logistic$Freq[2]/(plot_logistic$Freq[2] + plot_logistic$Freq[1])
print(FPT)


predicted_95 <-as.factor(ifelse(p[,2]> 0.95,1,0))

confm_95 <- confusionMatrix(predicted_95, df$Species_binary, positive = "1")

plot_logistic_95 <- as.data.frame(confm_95$table)
plot_logistic_95$Prediction <- factor(plot_logistic_95$Prediction, 
                                      levels=rev(levels(plot_logistic_95$Prediction)))
plot_conf_95 <- ggplot(plot_logistic_95, aes(Prediction, Reference, fill= Freq)) +
  geom_tile() + geom_text(aes(label=Freq)) + theme(legend.position = "None") +
  ggtitle("Confusion Matrix with Threshold 0.95")

plot_conf_95

#true positive rate
TTP1<-plot_logistic_95$Freq[4]/(plot_logistic_95$Freq[4] + plot_logistic_95$Freq[3])
print(TTP1)
#false positive rate
FPT1<-plot_logistic_95$Freq[2]/(plot_logistic_95$Freq[2] + plot_logistic_95$Freq[1])
print(FPT1)



predicted_01 <-as.factor(ifelse(p[,2]> 0.01,1,0))

confm_01 <- confusionMatrix(predicted_01, df$Species_binary, positive = "1")

plot_logistic_01 <- as.data.frame(confm_01$table)
plot_logistic_01$Prediction <- factor(plot_logistic_01$Prediction, 
                                      levels=rev(levels(plot_logistic_95$Prediction)))
plot_conf_01 <- ggplot(plot_logistic_01, aes(Prediction, Reference, fill= Freq)) +
  geom_tile() + geom_text(aes(label=Freq)) + theme(legend.position = "None") +
  ggtitle("Confusion Matrix with Threshold 0.01")

plot_conf_01

#true positive rate
TTP2<-plot_logistic_01$Freq[4]/(plot_logistic_01$Freq[4] + plot_logistic_01$Freq[3])
print(TTP2)
#false positive rate
FPT2<-plot_logistic_01$Freq[2]/(plot_logistic_01$Freq[2] + plot_logistic_01$Freq[1])
print(FPT2)


#LABELS
df_roc <- data.frame(p[,2], df$Species_binary)

df_roc <- df_roc %>%
  rename(predictions = p...2.)

df_roc <- df_roc %>%
  rename(labels = df.Species_binary )


# Create predictions 
pred <- prediction(df_roc$predictions, df_roc$labels)
# Calculte tpr and fpr
perf <- performance(pred,"tpr","fpr")

plot(perf,colorize=TRUE)

#calculer AUC

auc.tmp <- performance(pred,"auc")
auc <- as.numeric(auc.tmp@y.values)
print(paste("AUC IS",auc))


