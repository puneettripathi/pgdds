#Loading necessary libraries
library(dplyr)
library(forecast)
library(tseries)
require(graphics)

##DATA PREPARATION

#Perform Basic data quality check, pending
#?????????????????????????????????????????

#Reading the data from CSV file
Data <- read.csv("Global Superstore.csv")

#Extracing month and year from Order Date for aggregation and sorting
Data$Order.Month <- substr(Data$Order.Date, 4, 5)
Data$Order.Year <- substr(Data$Order.Date, 7, 10)

#Converting the transactional level data to a Monthly Aggregated data for each segment.
Data1 <- group_by(Data, Market, Segment, Order.Month, Order.Year)
Data2 <- summarize(Data1, tot_Sales = sum(Sales),
                          tot_Quantity = sum(Quantity),
                          tot_Profit = sum(Profit))

#Find the 2 most profitable (Higher Profit) and consistently profitable segments (Lower COV).
Data3 <- group_by(Data2, Market, Segment)
Data4 <- summarise(Data3, mean_Profit = mean(tot_Profit),
                          sd_Profit = sd(tot_Profit))
Data4$COV <- (Data4$sd_Profit / Data4$mean_Profit) * 100

#After analysing the data, we found:
#As per Max profit, top 2:
#APAC Consumer 4642.0325 2934.3785 63.21323
#EU Consumer   3930.9939 2454.1398 62.43052

#As per Min COV, top 2:
#EU Consumer   3930.9939 2454.1398 62.43052
#APAC Consumer 4642.0325 2934.3785 63.21323


#Preparing Data for APAC Consumer Sales and Demand (Quantity)
Data_AC <- arrange(filter(Data2, Market == 'APAC', Segment == 'Consumer'), Order.Year, Order.Month)
Data_AC$Month <- seq.int(nrow(Data_AC))
Data_AC_Sales <- select(ungroup(Data_AC), Month, tot_Sales)
Data_AC_Quantity <- select(ungroup(Data_AC), Month, tot_Quantity)

#Preparing Data for EU Consumer Sales and Demand (Quantity)
Data_EC <- arrange(filter(Data2, Market == 'EU', Segment == 'Consumer'), Order.Year, Order.Month)
Data_EC$Month <- seq.int(nrow(Data_EC))
Data_EC_Sales <- select(ungroup(Data_EC), Month, tot_Sales)
Data_EC_Quantity <- select(ungroup(Data_EC), Month, tot_Quantity)


#For Forecasting Sales for APAC Consumer Segment
Data_ts <- Data_AC_Sales
names(Data_ts) <- c("Month", "Value")

#Counting the number of records/months
nrow(Data_ts)

total_timeser <- ts(Data_ts$Value)
plot(total_timeser)
#We can clearly see from the plot that there is a global trend, no seasonality observed.

indata <- Data_ts[1:42,]
timeser <- ts(indata$Value)
plot(timeser)

#Smoothing the series - Moving Average Smoothing
w <- 2
smoothedseries <- stats::filter(timeser, 
                                filter=rep(1/(2*w+1),(2*w+1)), 
                                method='convolution', sides=2)
#Smoothing left end of the time series
diff <- smoothedseries[w+2] - smoothedseries[w+1]
for (i in seq(w,1,-1)) {
  smoothedseries[i] <- smoothedseries[i+1] - diff
}
#Smoothing right end of the time series
n <- length(timeser)
diff <- smoothedseries[n-w] - smoothedseries[n-w-1]
for (i in seq(n-w+1, n)) {
  smoothedseries[i] <- smoothedseries[i-1] + diff
}
#Plot the smoothed time series
timevals_in <- indata$Month
lines(smoothedseries, col="blue", lwd=2)

#Building a model on the smoothed time series using classical decomposition
#First, let's convert the time series to a dataframe
smootheddf <- as.data.frame(cbind(timevals_in, as.vector(smoothedseries)))
colnames(smootheddf) <- c('Month', 'Value')

#Now, let's fit a additive model with trend and seasonality to the data
#Seasonality will be modeled using a sinusoid function
lmfit <- lm(Value ~ sin(0.5*Month) * poly(Month,3) + cos(0.5*Month) * poly(Month,3), data=smootheddf)
global_pred <- predict(lmfit, Month=timevals_in)
#summary(global_pred)
#lines(timevals_in, global_pred, col='red', lwd=2)
lines(global_pred, col='red', lwd=2)

#Now, let's look at the locally predictable series
#We will model it as an ARMA series
local_pred <- timeser-global_pred
plot(local_pred, col='red', type = "l")
acf(local_pred)
acf(local_pred, type="partial")
armafit <- auto.arima(local_pred)

tsdiag(armafit)
armafit
# Series: local_pred 
# ARIMA(0,0,0) with zero mean 
# 
# sigma^2 estimated as 91944190:  log likelihood=-444.67
# AIC=891.33   AICc=891.43   BIC=893.07
# 
#

#We'll check if the residual series is white noise
resi <- local_pred-fitted(armafit)
adf.test(resi,alternative = "stationary")
#Dickey-Fuller = -4.7148, Lag order = 3, p-value = 0.01
#alternative hypothesis: stationary
kpss.test(resi)
#KPSS Level = 0.029285, Truncation lag parameter = 1, p-value = 0.1

#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 months
outdata <- Data_ts[43:48,]
timevals_out <- outdata$Month
global_pred_out <- predict(lmfit,data.frame(Month =timevals_out))
fcast <- global_pred_out

#Now, let's compare our prediction with the actual values, using MAPE
MAPE_class_dec <- forecast::accuracy(fcast,outdata$Value)[5]
MAPE_class_dec
#23.93068

#Let's also plot the predictions along with original values, to
#get a visual feel of the fit
class_dec_pred <- c(ts(global_pred),ts(global_pred_out))
plot(total_timeser, col = "black")
lines(class_dec_pred, col = "red")


#So, that was classical decomposition, now let's do an ARIMA fit
autoarima <- auto.arima(timeser)
autoarima
# ARIMA(0,1,1) 
# 
# Coefficients:
#   ma1
# -0.7559
# s.e.   0.1381
# 
# sigma^2 estimated as 174361555:  log likelihood=-447.11
# AIC=898.23   AICc=898.55   BIC=901.66

#Lower the AIC and BIC value, better the model
#Log likelihood must be higher for better model
tsdiag(autoarima)
plot(autoarima$x, col="black")
lines(fitted(autoarima), col="red")

#Again, let's check if the residual series is white noise
resi_auto_arima <- timeser - fitted(autoarima)
adf.test(resi_auto_arima,alternative = "stationary")
#Dickey-Fuller = -4.2563, Lag order = 3, p-value = 0.01
kpss.test(resi_auto_arima)
#KPSS Level = 0.042734, Truncation lag parameter = 1, p-value = 0.1

#Also, let's evaluate the model using MAPE
fcast_auto_arima <- predict(autoarima, n.ahead = 6)
MAPE_auto_arima <- accuracy(fcast_auto_arima$pred,outdata$Value)[5]
MAPE_auto_arima
#27.68952

#Lastly, let's plot the predictions along with original values, to
#get a visual feel of the fit
auto_arima_pred <- c(fitted(autoarima),ts(fcast_auto_arima$pred))
plot(total_timeser, col = "black")
lines(auto_arima_pred, col = "red")
