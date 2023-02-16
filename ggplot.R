library(psych)
library(ggthemes)
library(ggplot2)
library(relaimpo)
df <- read.csv("insurance (1).csv")
str(df)
summary(df)
describeBy(df$charges,df$region)
ggplot(data=df,aes(region,charges))+geom_boxplot(fill=c(10:13))+theme_gray()+ggtitle("Boxplot of medical charges per region")


describeBy(df$charges,df$smoker)
ggplot(data=df,aes(smoker,charges))+geom_boxplot(fill=c(10:11))+theme_gray()+ggtitle("Boxplot of medical charges per smoker")

describeBy(df$charges,df$sex)
ggplot(data=df,aes(sex,charges))+geom_boxplot(fill=c(10:11))+theme_gray()+ggtitle("Boxplot of medical charges per sex")

describeBy(df$charges,df$children)
ggplot(data=df,aes(as.factor(children),charges))+geom_boxplot(fill=c(2:7))+theme_gray()+ggtitle('Boxplot of medical charges per children')


df$bmi30<-ifelse(df$bmi>=30,"yes","no")
describeBy(df$charges,df$bmi30)


ggplot(data=df,aes(bmi30,charges))+geom_boxplot(fill=c(2:3))+theme_stata() +ggtitle("Boxplot")


pairs.panels(df[c("age","bmi","children","charges")])

#reg lin 
model<-lm(charges~.,data=df)
summary(model)

plot(df$age,df$charges,col="orange")
