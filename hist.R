library(tidyverse)
df<-read.csv("Data_Cortex_Nuclear.csv")
attach(df)
str(df)
names(df)
summary(df)
table(df$Treatment)
table(df$Behavior)
table(df$class)
#class
ggplot(df,aes(class))+geom_bar(aes(fill=class))
+theme_light()+ggtitle("class")
#behavior
ggplot(df,aes(Behavior))+geom_bar(aes(fill=Behavior))
+theme_light()+ggtitle("class")
#treatment
ggplot(df,aes(Treatment))+geom_bar(aes(fill=Treatment))
+theme_light()+ggtitle("class")
#On supprime les valeurs manquantes 
df<- df %>%
  group_by(class) %>%
  mutate_each(funs(replace(.,which(is.na(.)),mean(.,na.rm = T))))%>%
  as.data.frame()

protine<-names(df[2:78])
classes<-as.vector(unique(as.character(df$class)))

#histogramme
ggplot(df, aes(eval(parse(text = protine[1])))) +
  geom_histogram(aes(fill=..count..), color = "black", alpha = 0.9,bins = 30) +
  scale_fill_gradient("Count", low="orange", high=4) +ggtitle(protine[1])
  +labs(x="expression",y="count")
  +theme_light()



ggplot(df, aes(eval(parse(text = protine[10])))) +
  geom_histogram(aes(fill=..count..), color = "black", alpha = 0.9,bins = 30) +
  scale_fill_gradient("Count", low="orange", high=4) +ggtitle(protine[1])
+labs(x="expression",y="count")
+theme_light()


ggplot(df, aes(eval(parse(text = protine[20])))) +
  geom_histogram(aes(fill=..count..), color = "black", alpha = 0.9,bins = 30) +
  scale_fill_gradient("Count", low="orange", high=4) +ggtitle(protine[1])
+labs(x="expression",y="count")
+theme_light()


ggplot(df, aes(eval(parse(text = protine[70])))) +
  geom_histogram(aes(fill=..count..), color = "black", alpha = 0.9,bins = 30) +
  scale_fill_gradient("Count", low="orange", high=4) +ggtitle(protine[1])
+labs(x="expression",y="count")
+theme_light()






















