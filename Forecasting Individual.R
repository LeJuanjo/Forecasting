library(readxl)
library(forecast)

setwd("D:/OneDrive IESEG/OneDrive - IESEG/Documents/Forecasting Assignment") # Specify you own working directory here.
data <- read_excel("DataSets2022.xlsx", sheet="Airpass_BE")
air <- ts(data[,2], frequency = 12, start = 2003, end = c(2021,10))
plot(air)

air_train <- window(air, start=2003, end=c(2017,12))
air_test <- window(air, start=2018, end=c(2020,2))
 
###Explore the data----
#Explore the data using relevant graphs, and discuss the properties of the data.
#Include and discuss a time series plot, a seasonal plot, a seasonal subseries plot
#and a (P)ACF plot.

plot(air_train)


#Seasonal plot
seasonplot(air_train,year.labels=TRUE, year.labels.left=TRUE,
           main="Seasonal plot",
           ylab="Passengers",col=rainbow(20), pch=19)
#Seasonal Subseries plot
monthplot(air_train,ylab="Passengers",xlab="Month", xaxt="n",
          main="Seasonal subseries plot",
          type="l")
axis(1, at=1:12, labels=month.abb, cex=0.8)

#ACF and PAC
Acf(air_train,lag.max=36)

Pacf(air_train)



###Transformations ----
#Discuss whether a transformation and/or any other adjustment of the time
#series would be useful. If so, apply the most appropriate transformation and/or
#adjustments. Also, report the optimal Box-Cox lambda value that could be used
#to transform the time series. Clarify how you will proceed with the transformation
#in the remainder of the exercise.


lambda <- BoxCox.lambda(air_train)
lambda
plot(BoxCox(air_train,lambda))
plot(air_train)



### Forecasting---- 
#Create forecasts using the seasonal naive method. Check the residual diagnostics
#(including the Ljung-Box test) and the forecast accuracy (on the test set).

fitsn <- snaive(air_train,lambda = lambda)
plot(fitsn)
lines(air_test,col="red")
checkresiduals(fitsn)
ressn <- residuals(fitsn)
tsdisplay(ressn)
m <- matrix(nrow = 10, ncol = 4)               #
for (i in 1:10){                               #
  a <- Box.test(ressn,lag=i, fitdf=1, type="Lj") #
  m[i,1] <- i                                  #
  m[i,2] <- a$statistic                        #
  m[i,3] <- a$parameter                        #
  m[i,4] <- a$p.value                          #
}                                              #
m  
#Box.test(ressn, lag=1, fitdf=1, type="Lj")

accuracy(fitsn, air_test)[,c(2,3,5,6)]


### Decoposition----
#Use an STL decomposition to forecast the time series. Use the various underlying
#forecasting methods for the seasonally adjusted data (naive, rwdrift, ets, arima).
#Check the residual diagnostics and the forecast accuracy and select the best
#performing STL decomposition.


fit_stl <- stl(air_train[,1], s.window="periodic", robust= TRUE)
plot(fit_stl)


# Derive the forecast, based on a naive forecast
# for the seasonally adjusted series

fcast_stl1 <- stlf(air_train,method="naive",lambda= lambda)
fcast_stl2 <- stlf(air_train, method="rwdrift",lambda= lambda)
fcast_stl3 <- stlf(air_train, method="ets",lambda= lambda)
fcast_stl4 <- stlf(air_train, method="arima",lambda= lambda)

plot(fcast_stl1, ylab="New orders index")
plot(fcast_stl2, ylab="New orders index")
plot(fcast_stl3, ylab="New orders index")
plot(fcast_stl4, ylab="New orders index")

a_fc1 <- accuracy(fcast_stl1,air_test)[2,c(2,3,5,6)]
a_fc2 <- accuracy(fcast_stl2,air_test)[2,c(2,3,5,6)]
a_fc3 <- accuracy(fcast_stl3,air_test)[2,c(2,3,5,6)]
a_fc4 <- accuracy(fcast_stl4,air_test)[2,c(2,3,5,6)]

acc <- rbind(a_fc1, a_fc2, a_fc3, a_fc4)
rownames(acc) <- c("a_fc1", "a_fc2", "a_fc3", "a_fc4")
acc


checkresiduals(fcast_stl1)
checkresiduals(fcast_stl2)
checkresiduals(fcast_stl3)
checkresiduals(fcast_stl4)


### ETS----
#Generate forecasts using ETS. First select the appropriate models yourself and
#discuss their performance. Compare these models with the results of the automated
#ETS procedure. Check the residual diagnostics and the forecast accuracy for the
#various ETS models you've considered. Present the parameters of the final ETS
#model and show the forecasts in a graph.

fit_ets1 <- ets(air_train, model="AAA",lambda=lambda)
fit_ets2 <- ets(air_train, model="MAM")
fit_ets3 <- ets(air_train, model="AAA",damped=TRUE,biasadj = TRUE,lambda=lambda)
fit_ets4 <- ets(air_train,model = "ZZZ",lambda=lambda)

fcast_ets1 <- forecast(fit_ets1, h=26)
fcast_ets2 <- forecast(fit_ets2, h=26)
fcast_ets3 <- forecast(fit_ets3, h=26)
fcast_ets4 <- forecast(fit_ets4, h=26)

plot(fcast_ets1, ylab="New orders index")
plot(fcast_ets2, ylab="New orders index")
plot(fcast_ets3, ylab="New orders index")
plot(fcast_ets4, ylab="New orders index")

a_fc1 <- accuracy(fcast_ets1,air_test)[2,c(2,3,5,6)]
a_fc2 <- accuracy(fcast_ets2,air_test)[2,c(2,3,5,6)]
a_fc3 <- accuracy(fcast_ets3,air_test)[2,c(2,3,5,6)]
a_fc4 <- accuracy(fcast_ets4,air_test)[2,c(2,3,5,6)]

acc <- rbind(a_fc1, a_fc2, a_fc3, a_fc4)
rownames(acc) <- c("AAA", "MAM", "AAdA", "ZZZ")
acc


checkresiduals(fcast_ets1)
checkresiduals(fcast_ets2)
checkresiduals(fcast_ets3)
checkresiduals(fcast_ets4)

summary(fit_ets4)

###AUTO ARIMA----
#Generate forecasts using the auto.arima procedure. Present the estimated
#model using the backward shift operator. Include the parameter estimates.
#Check the residual diagnostics and the forecast accuracy. Discuss your results,
#and if necessary compare these with other possible ARIMA models (e.g. if small
#changes in the model specification improve the properties of the residuals and/or
#the forecast accuracy).

fit_auto <- auto.arima(air_train,lambda = lambda)
fcast_auto <- forecast(fit_auto)
plot(fcast_auto)
accuracy(fcast_auto,air_test)[2,c(2,3,5,6)]
checkresiduals(fcast_auto)
summary(fit_auto)


###COMPARISON----
#Compare the different models (naive, STL, ETS, ARIMA) in terms of residual
#diagnostics and forecast accuracy. Present the results in a summary table.
#Analyse your results and select your final model.



a_fc1 <- accuracy(fitsn,air_test)[2,c(2,3,5,6)]
a_fc2 <- accuracy(fcast_stl1,air_test)[2,c(2,3,5,6)]
a_fc3 <- accuracy(fcast_ets4,air_test)[2,c(2,3,5,6)]
a_fc4 <- accuracy(fcast_auto,air_test)[2,c(2,3,5,6)]

acc <- rbind(a_fc1, a_fc2, a_fc3, a_fc4)
rownames(acc) <- c("SNAIVE", "STL", "ETS", "ARIMA")
acc


checkresiduals(fitsn)
checkresiduals(fcast_stl4)
checkresiduals(fcast_ets4)
checkresiduals(fcast_auto)

plot(fcast_ets4, ylab="New orders index")
lines (air_test,col="red")

###OUT OF SAMPLE FORECAST----
#Generate out of sample forecasts up to December 2022, based on the complete
#time series (January 2003 - February 2020). Present your results.
air_oos <- window(air, start=2003, end=c(2020,2))
fit_oos <- ets(air_oos)
fcast_oos <- forecast(fit_oos,34)
plot(fcast_oos)
checkresiduals(fit_oos)
accuracy(fcast_oos,air)[,c(2,3,5,6)]



###COVID----
#Now consider the last observations in the time series (March 2020 - October
#2021). They correspond to the COVID pandemic times. What do you learn
#about the impact of the pandemic on air passenger transport between Belgium
#and other EU countries, based on the data and your final forecasts?
  
lines(window(air,start=c(2020,2)),col="red")

      