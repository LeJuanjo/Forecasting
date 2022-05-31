library(readxl)
library(forecast)
library(tseries) 
#https://www.kaggle.com/datasets/vishnucr/retails-sales-beer-wine-and-liquor-stores
setwd("D:/OneDrive IESEG/OneDrive - IESEG/Documents/Forecasting Assignment") # Specify you own working directory here.
data <- read.csv("Retail Sales.csv")
sales <- ts(data[,2], frequency = 12, start = 1992)
plot(sales)


sales_train <- window(sales, start=1992, end=c(2016,12))
sales_test <- window(sales, start=2017, end=c(2020,2))


#time series plot
plot(sales_train)


#Seasonal plot
seasonplot(sales_train,year.labels=TRUE, year.labels.left=TRUE,
           main="Seasonal plot",
           ylab="$ million",col=rainbow(20), pch=19)
#Seasonal Subseries plot
monthplot(sales_train,ylab="$ million",xlab="Month", xaxt="n",
          main="Seasonal subseries plot",
          type="l")
axis(1, at=1:12, labels=month.abb, cex=0.8)

#ACF and PAC
Acf(sales_train,lag.max=36)

Pacf(sales_train)

lambda <- BoxCox.lambda(sales_train)
lambda
plot(BoxCox(sales_train,lambda))
plot(sales_train)


fit_stl <- stl(sales_train, s.window="periodic", robust= TRUE)

plot(fit_stl)


#Holt-Winters' Additive Seasonal Method
#https://docs.oracle.com/cd/E57185_01/CBREG/ch06s02s02s02.html
fit_hw_1 <- hw(sales_train,seasonal="additive",lambda = lambda,h=36)
fit_hw_2 <- hw(sales_train,seasonal="additive",alpha=0.5,lambda = lambda,h=36)
fit_hw_3 <- hw(sales_train,seasonal="multiplicative",h=36)
fit_hw_4 <- hw(sales_train,seasonal="multiplicative",alpha=0.5,h=36)

plot(fit_hw_1, ylab="New orders index")
plot(fit_hw_2, ylab="New orders index")
plot(fit_hw_3, ylab="New orders index")
plot(fit_hw_4, ylab="New orders index")

a_fc1 <- accuracy(fit_hw_1,sales_test)[2,c(2,3,5,6)]
a_fc2 <- accuracy(fit_hw_2,sales_test)[2,c(2,3,5,6)]
a_fc3 <- accuracy(fit_hw_3,sales_test)[2,c(2,3,5,6)]
a_fc4 <- accuracy(fit_hw_4,sales_test)[2,c(2,3,5,6)]

acc <- rbind(a_fc1, a_fc2, a_fc3, a_fc4)
rownames(acc) <- c("HWAd", "HWAd0.5", "HWM", "HWM0.5")
acc

checkresiduals(fit_hw_1)
checkresiduals(fit_hw_2)
checkresiduals(fit_hw_3)
checkresiduals(fit_hw_4)




fit_ets1 <- ets(sales_train, model="AAA",lambda=lambda)
fit_ets2 <- ets(sales_train, model="MAM")
fit_ets3 <- ets(sales_train, model="AAA",damped=TRUE,lambda=lambda)
fit_ets4 <- ets(sales_train,model = "MAA")

fcast_ets1 <- forecast(fit_ets1, h=36)
fcast_ets2 <- forecast(fit_ets2, h=36)
fcast_ets3 <- forecast(fit_ets3, h=36)
fcast_ets4 <- forecast(fit_ets4, h=36)

plot(fcast_ets1, ylab="New orders index")
plot(fcast_ets2, ylab="New orders index")
plot(fcast_ets3, ylab="New orders index")
plot(fcast_ets4, ylab="New orders index")

a_fc1 <- accuracy(fcast_ets1,sales_test)[2,c(2,3,5,6)]
a_fc2 <- accuracy(fcast_ets2,sales_test)[2,c(2,3,5,6)]
a_fc3 <- accuracy(fcast_ets3,sales_test)[2,c(2,3,5,6)]
a_fc4 <- accuracy(fcast_ets4,sales_test)[2,c(2,3,5,6)]

acc <- rbind(a_fc1, a_fc2, a_fc3, a_fc4)
rownames(acc) <- c("AAA", "MAM", "AAdA", "MAA")
acc


checkresiduals(fcast_ets1)
checkresiduals(fcast_ets2)
checkresiduals(fcast_ets3)
checkresiduals(fcast_ets4)
#

#FINAL COMPARISON
a_fc1 <- accuracy(fit_hw_2,sales_test)[2,c(2,3,5,6)]
a_fc2 <- accuracy(fcast_ets2,sales_test)[2,c(2,3,5,6)]


acc <- rbind(a_fc1, a_fc2)
rownames(acc) <- c("HW", "ETS")
acc


plot(fit_hw_2, ylab="New orders index")
lines(sales_test,col="red")
