library(ggplot2)
library(tseries)
library(forecast)
data<-read.csv("AirPassengers.csv",stringsAsFactors = F)
#converting dataframe to TimeSeries
dd<-ts(data[-1],frequency =12,start =c(1949,1),end=c(1960,12))
class(dd)
plot(dd,col="red")
abline(reg=lm(dd~time(dd)))
cycle(dd)
boxplot(dd~cycle(dd),col="red")
trends<-decompose(dd,"multiplicative")
plot(trends,col='red')
adf.test(dd,alternative = "stationary")
model<-auto.arima(dd)
model
predict<-forecast(model,level=c(95),h=10*12)
plot(predict,col="gray")
#test our model
Box.test(model$residuals,lag=5,type="Ljung-Box")
Box.test(model$residuals,lag=15,type="Ljung-Box")
Box.test(model$residuals,lag=25,type="Ljung-Box")
##########################################
##########################################
library(ggplot2)
library(dplyr)
library(reshape)
library(tseries)
library(cowplot)
library(forecast)
data1<-read.csv('TSLA.csv')
data1$Date=as.Date(data1$Date)
str(data1)
#Visualisation
data1  %>%
  select(Date,Volume) %>%
  ggplot() +
  geom_point(aes(x = Date, y = Volume),
             alpha =  0.5) +
  geom_line(aes(x = Date, y = Volume),
            alpha = 0.8) +
  xlab("Date") + ylab("Volume") + 
  labs(col = "Types of Price") + 
  theme_bw() +
  theme(text = element_text(size = 15, face = "bold"))


data1  %>%
  select(-Volume) %>%
  melt(id.vars = c("Date")) %>%
  ggplot() +
  geom_point(aes(x = Date, y = value, col = variable),
             alpha =  0.5) +
  geom_line(aes(x = Date, y = value, col = variable, group = variable),
            alpha = 0.8) +
  xlab("Date") + ylab("Stock Price") + 
  labs(col = "Types of Price") + 
  theme_bw() +
  theme(text = element_text(size = 15, face = "bold"),
        legend.position = c(0.2,0.8))


#process MA

Smoothing = function(x,interval){
  
  ts_x = ts(x) # TimeSeries Strings
  Sm = stats::filter(ts_x, filter = rep(1/interval,interval)) # To avoid packages collision
  
  return(Sm)
}

Open_Week = Smoothing(x = data1$Open,
                      interval = 7)
DATE = data1$Date

ggplot(NULL) +
  geom_point(aes(x = DATE, y = Open_Week)) +
  theme_bw() +
  theme(text = element_text(size = 15, face = "bold"),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        legend.position = c(0.2,0.5))


##################
##################

L_Smoothing = lapply(data1[,-grep("Date",colnames(data1))],
                     function(x, interval)  Smoothing(x, 30))

L_Smoothing = as.data.frame(L_Smoothing)

head(L_Smoothing,n = 30)


###############
L_Smoothing$Date = DATE

L_Smoothing %>%
  na.omit() %>%
  select(-Volume) %>%
  melt(id.vars = c("Date")) %>%
  ggplot() +
  geom_point(aes(x = Date, y = value, col = variable),
             alpha =  0.5) +
  geom_line(aes(x = Date, y = value, col = variable, group = variable),
            alpha = 0.8) +
  xlab("Date") + ylab("Stock Price") + 
  ggtitle("30 days Smoothing (Moving Average)") + 
  labs(col = "Types of Price") + 
  theme_bw() +
  theme(text = element_text(size = 15, face = "bold"),          legend.position = c(0.2,0.8))


################

INTERVAL = c(30,60,90,120,365)

for(i in INTERVAL){
    DF = lapply(data1[,-grep("Date",colnames(data1))],
              function(x, interval)  Smoothing(x, i))
  
  DF = as.data.frame(DF)
  assign(paste0("L_Smoothing_",i),DF)
}


TSLA2 = data.frame(
  
  DATE = data1$Date,
  Open = data1$Open,
  Open_30 = L_Smoothing_30$Open,
  Open_60 = L_Smoothing_60$Open,
  Open_90 = L_Smoothing_90$Open,
  Open_120 = L_Smoothing_120$Open,
  Open_365 = L_Smoothing_365$Open
  
)

TSLA2 %>% 
  melt(id.vars = c("DATE")) %>%
  ggplot() + 
  geom_line(aes(x = DATE, y = value, col = variable, group = variable),size = 1.2) +
  theme_bw() +
  xlab("Date") + ylab("Stock Price") + 
  ggtitle("Smoothing (Moving Average)") + 
  labs(col = "Smoothing Days") + 
  theme_bw() +
  theme(text = element_text(size = 15, face = "bold"),
        legend.position = c(0.2,0.8))



#############

TSLA2 %>% 
  melt(id.vars = c("DATE")) %>%
  ggplot() + 
  geom_line(aes(x = DATE, y = value, col = variable, group = variable),size = 1.2) +
  theme_bw() +
  xlab("Date") + ylab("Stock Price") + 
  ggtitle("Smoothing (Moving Average)") + 
  labs(col = "Smoothing Days") + 
  guides(col = FALSE) +
  theme_bw() +
  theme(text = element_text(size = 15, face = "bold"),
        axis.text.x = element_text(angle = 90)) +
  facet_wrap(~variable)

#Decomposition of timeSeries Data
Decomposition = function(x){
  
  TS_X = ts(x, frequency = 365)
  
  Add = decompose(TS_X,type = "additive")
  Multi = decompose(TS_X, type = "multiplicative")
  Result = list()
  Result$Add = Add
  Result$Multi  = Multi
  
  return(Result)
}
TD = Decomposition(x = data1$Open)
autoplot(TD$Add) + 
  theme_bw() +
  theme(text = element_text(size = 15, face = "bold"))


#waterfall chart

Difference = function(x){
  Diff_Vector = c()
  Diff_Vector[1] = 0
  
  for(k in 2:length(x)){
    
    Diff = x[k] - x[k-1]
    
    Diff_Vector[k] = Diff
    }
  return(Diff_Vector)
  
}

Open_Diff = Difference(x = data1$Open)

WATER = data.frame(
  DATE = data1$Date,
  Open = Open_Diff
)

WATER$Increase = ifelse(WATER$Open >= 0 , "Plus", "Minus")

WATER %>%
  mutate(Increase = factor(Increase, levels = c("Plus","Minus"))) %>%
  ggplot() +
  geom_bar(aes(x = DATE, y = Open, fill = Increase), stat = 'identity') + 
  scale_fill_manual(values = c("red","royalblue")) + 
  theme_bw() +
  theme(text = element_text(size = 15, face = "bold"),
        legend.position = c(0.2,0.8))


WATER[(nrow(WATER)-100):nrow(WATER),] %>%
  mutate(Increase = factor(Increase, levels = c("Plus","Minus"))) %>%
  ggplot() +
  geom_bar(aes(x = DATE, y = Open, fill = Increase), stat = 'identity') + 
  scale_fill_manual(values = c("red","royalblue")) + 
  theme_bw() +
  theme(text = element_text(size = 15, face = "bold"),
        legend.position = c(0.2,0.9))




























