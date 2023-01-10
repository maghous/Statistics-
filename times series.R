#cas n1
stockrate <- c(480, 6813, 27466, 49287,
               7710, 96820, 96114, 236214,
               2088743, 381497, 927251,
               1407615
               , 1972113)
stockrate.timeseries <- ts(stockrate,start = c(2019,1),frequency = 12)
plot(stockrate.timeseries)
plot(stockrate.timeseries,col="red",type="b")
#cas n2
stockrate <- c(450, 613, 466, 205.7,
               571.0, 622.0, 851.4, 621.4,
               875.3, 979.7, 927.5,
               14.45)
stockrate2 <- c(550, 713, 566, 687.2,
                110, 120, 72.4, 814.4,
                423.5, 98.7, 741.4,
                345.3)
combined.stockrate <-  matrix(c(stockrate,stockrate2),nrow = 12)
stockrate.timeseries <- ts(combined.stockrate,start = c(2014,1),frequency = 12)
plot(stockrate.timeseries, main = "Showing Mutiple series")
plot(stockrate.timeseries, main = "Showing Mutiple series",col="red",type="b")
print(stockrate.timeseries)
#cas n3
data("austres")
summary(austres)
plot(austres,col="red")
tsdata <- ts(austres, frequency = 12)
dcdata <- decompose(tsdata, "multiplicative")
plot(dcdata,col="red",type="b")