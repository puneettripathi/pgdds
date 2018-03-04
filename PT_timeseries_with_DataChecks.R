#Loading necessary libraries
# Keep next line commented, for dev purpose only
# setwd("D:/pgdds/Time Series/TimeSeries/")
# Run these if you get package not found error
# install.packages('forecast')
# install.packages('tseries')

#Loading necessary libraries
library(dplyr)
library(forecast)
library(tseries)
require(graphics)

###### DATA PREPARATION #######

#Reading the data from CSV file
Data <- read.csv("Global Superstore.csv")

# Checking duplicates
which(duplicated(Data) | duplicated(Data[nrow(Data):1, ])[nrow(Data):1])
# integer(0)
# means there are no duplicate records in the data
#Understanding Dimensions
dim(Data)
# 51290 rows    24 columns

#Structure of the dataset
str(Data)
# Row.ID is row identifier and should be removed as no impact on analysis
# same as above with Order.Id, Customer.Id, Product.Id - all are unique identifier
# Customer.Name & Product.Name is again no use for analysis

# Missing values
colnames(Data)[colSums(is.na(Data)) > 0]
# "Postal.Code"
# Only one column Postal.Code has missing values

# Checking for columns with same values
sapply(Data,function(x) length(unique(x)))
# No column with just 1 value
# Row.ID       Order.ID     Order.Date      Ship.Date      Ship.Mode    Customer.ID  Customer.Name 
# 51290          25035           1430           1464              4           1590            795 
# Segment           City          State        Country    Postal.Code         Market         Region 
# 3           3636           1094            147            632              7             13 
# Product.ID       Category   Sub.Category   Product.Name          Sales       Quantity       Discount 
# 10292              3             17           3788          22995             14             27 
# Profit  Shipping.Cost Order.Priority    Order.Month     Order.Year 
# 24575          10037              4             12              4 


#Extracing month and year from Order Date for aggregation and sorting
Data$Order.Month <- substr(Data$Order.Date, 4, 5)
Data$Order.Year <- substr(Data$Order.Date, 7, 10)

#Converting the transactional level data to a Monthly Aggregated data for each segment.
Data1 <- group_by(Data, Market, Segment, Order.Month, Order.Year)
Data2 <- summarize(Data1, tot_Sales = sum(Sales),
                   tot_Quantity = sum(Quantity),
                   tot_Profit = sum(Profit))

####Find the 2 most profitable (Higher Profit) and consistently profitable segments (Lower COV).####
Data3 <- group_by(Data2, Market, Segment)
Data4 <- summarise(Data3, mean_Profit = mean(tot_Profit),
                   sd_Profit = sd(tot_Profit), Profit = sum(tot_Profit))
Data4$COV <- (Data4$sd_Profit / Data4$mean_Profit) * 100

####After analysing the data, we found:####

arrange(Data4,desc(Profit))
#As per Max profit, top 2:
#Market     Segment mean_Profit sd_Profit    Profit       COV
#<fctr>      <fctr>       <dbl>     <dbl>     <dbl>     <dbl>
#  1   APAC    Consumer    4642.033  2934.379 222817.56  63.21323
#2     EU    Consumer    3930.994  2454.140 188687.71  62.43052

arrange(Data4,COV)
##As per Min COV, top 2:
#Market     Segment mean_Profit sd_Profit    Profit       COV
#<fctr>      <fctr>       <dbl>     <dbl>     <dbl>     <dbl>
#  1     EU    Consumer    3930.994 2454.1398 188687.71  62.43052
#2   APAC    Consumer    4642.033 2934.3785 222817.56  63.21323



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


#For Forecasting Revenue (Sales) for APAC Consumer Segment
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
w <- 1
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
#From the plot of smoothed data, we can observe the seasonality (additive)


######## CLASSICAL DECOMPOSITION ########
#Building a model on the smoothed time series using classical decomposition
#First, let's convert the time series to a dataframe
smootheddf <- as.data.frame(cbind(timevals_in, as.vector(smoothedseries)))
colnames(smootheddf) <- c('Month', 'Value')

#Now, let's fit a additive model with trend and seasonality to the data
#Seasonality will be modeled using a sinusoid function
lmfit <- lm(Value ~ sin(0.5*Month) + Month + cos(Month) , data=smootheddf)
global_pred <- predict(lmfit, Month=timevals_in)
summary(global_pred)
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
# sigma^2 estimated as 107883933:  log likelihood=-448.02
# AIC=898.05   AICc=898.15   BIC=899.78
# 
#

#As there is no ARMA model fit for local predictible part,
#we will prove that local pred is stationary.
resi <- local_pred
adf.test(resi,alternative = "stationary")
#Dickey-Fuller = -4.3741, Lag order = 3, p-value = 0.01
#alternative hypothesis: stationary
kpss.test(resi)
#KPSS Level = 0.037546, Truncation lag parameter = 1, p-value = 0.1

#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 months
outdata <- Data_ts[43:48,]
timevals_out <- outdata$Month
global_pred_out <- predict(lmfit,data.frame(Month =timevals_out))
fcast <- global_pred_out

#Now, let's compare our prediction with the actual values, using MAPE
MAPE_class_dec <- forecast::accuracy(fcast,outdata$Value)[5]
MAPE_class_dec
#19.67947

#Let's also plot the predictions along with original values, to
#get a visual feel of the fit
class_dec_pred <- c(ts(global_pred),ts(global_pred_out))
plot(total_timeser, col = "black")
lines(class_dec_pred, col = "red")


######## AUTO ARIMA ########
#So, that was classical decomposition, now let's do an ARIMA fit
autoarima <- auto.arima(timeser)
autoarima
# ARIMA(0,1,1) 
# 
# Coefficients:
#           ma1
#       -0.7559
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
# auto_arima_pred <- c(fitted(autoarima),ts(fcast_auto_arima$pred))
# plot(total_timeser, col = "black")
# lines(auto_arima_pred, col = "purple")
# plot(forecast(auto_arima_pred, h=12))
# 95% confidence interval is very wide, we can't be sure of the prdictions made.

#As the MAPE value is better for classical model decomposition,
#we will forecast the Revenue (Sales) for APAC Consumer using the same.
# Forecasting future months Using Classical Decomposition
forecast.decompose <- predict(lmfit,data.frame(Month =49:54))
# 1        2        3        4        5        6 
# 56747.20 52945.79 50902.72 50949.93 51885.36 52514.01 



####### APAC Demand (Quantity) Modeling #######
#For Forecasting Demand(Quantity) for APAC Consumer Segment
Data_ts <- Data_AC_Quantity
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
w <- 1
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
##From the plot of smoothed data, we can observe the seasonality (additive)


######## CLASSICAL DECOMPOSITION ########
#Building a model on the smoothed time series using classical decomposition
#First, let's convert the time series to a dataframe
smootheddf <- as.data.frame(cbind(timevals_in, as.vector(smoothedseries)))
colnames(smootheddf) <- c('Month', 'Value')

#Now, let's fit a additive model with trend and seasonality to the data
#Seasonality will be modeled using a sinusoid function
lmfit <- lm(Value ~ sin(0.5*Month) + cos(0.5*Month) + poly(Month,2), data=smootheddf)
global_pred <- predict(lmfit, Month=timevals_in)
summary(global_pred)
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
# sigma^2 estimated as 13473:  log likelihood=-259.27
# AIC=520.55   AICc=520.65   BIC=522.28
# 
#

#As there is no ARMA model fit for local predictible part,
#we will prove that local pred is stationary.
#We'll check if the residual series is white noise
resi <- local_pred
adf.test(resi,alternative = "stationary")
# Dickey-Fuller = -5.0179, Lag order = 3, p-value = 0.01
#alternative hypothesis: stationary
kpss.test(resi)
# KPSS Level = 0.028815, Truncation lag parameter = 1, p-value = 0.1

#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 months
outdata <- Data_ts[43:48,]
timevals_out <- outdata$Month
global_pred_out <- predict(lmfit,data.frame(Month =timevals_out))
fcast <- global_pred_out

#Now, let's compare our prediction with the actual values, using MAPE
MAPE_class_dec <- forecast::accuracy(fcast,outdata$Value)[5]
MAPE_class_dec
#22.40313

#Let's also plot the predictions along with original values, to
#get a visual feel of the fit
class_dec_pred <- c(ts(global_pred),ts(global_pred_out))
plot(total_timeser, col = "black")
lines(class_dec_pred, col = "red")


######## AUTO ARIMA ########
#So, that was classical decomposition, now let's do an ARIMA fit
autoarima <- auto.arima(timeser)
autoarima
# ARIMA(0,1,0) 
# 
# sigma^2 estimated as 25366:  log likelihood=-266.07
# AIC=534.14   AICc=534.24   BIC=535.85

#Lower the AIC and BIC value, better the model
#Log likelihood must be higher for better model
tsdiag(autoarima)
plot(autoarima$x, col="black")
lines(fitted(autoarima), col="red")

#Again, let's check if the residual series is white noise
resi_auto_arima <- timeser - fitted(autoarima)
adf.test(resi_auto_arima,alternative = "stationary")
#Dickey-Fuller = -4.3326, Lag order = 3, p-value = 0.01
kpss.test(resi_auto_arima)
#KPSS Level = 0.031535, Truncation lag parameter = 1, p-value = 0.1

#Also, let's evaluate the model using MAPE
fcast_auto_arima <- predict(autoarima, n.ahead = 6)
MAPE_auto_arima <- accuracy(fcast_auto_arima$pred,outdata$Value)[5]
MAPE_auto_arima
#26.24458

#Lastly, let's plot the predictions along with original values, to
#get a visual feel of the fit
# auto_arima_pred <- c(fitted(autoarima),ts(fcast_auto_arima$pred))
# plot(total_timeser, col = "black")
# lines(auto_arima_pred, col = "purple")
# plot(forecast(auto_arima_pred, h=12))
# 95% confidence interval is very wide, we can't be sure of the prdictions made.

#As the MAPE value is better for classical model decomposition,
#we will forecast the Demand (Quantity) for APAC Consumer using the same.
# Forecasting future months
forecast.decompose <- predict(lmfit,data.frame(Month =49:54))
# 1        2        3        4        5        6 
# 664.6094 635.2503 612.2593 603.6888 614.0768 643.3369



########## Starting analysis on EU ###########
#For Forecasting Revenue (Sales) for EU Consumer Segment
Data_ts <- Data_EC_Sales
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
w <- 1
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
##From the plot of smoothed data, we can observe the seasonality (additive)

######## CLASSICAL DECOMPOSITION ########
#Building a model on the smoothed time series using classical decomposition
#First, let's convert the time series to a dataframe
smootheddf <- as.data.frame(cbind(timevals_in, as.vector(smoothedseries)))
colnames(smootheddf) <- c('Month', 'Value')

#Now, let's fit a additive model with trend and seasonality to the data
#Seasonality will be modeled using a sinusoid function
lmfit <- lm(Value ~ sin(0.5*Month) + Month, data=smootheddf)
global_pred <- predict(lmfit, Month=timevals_in)
summary(global_pred)
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
# sigma^2 estimated as 113342644:  log likelihood=-449.06
# AIC=900.12   AICc=900.22   BIC=901.86
# 
#

#As there is no ARMA model fit for local predictible part,
#we will prove that local pred is stationary.
#We'll check if the residual series is white noise
resi <- local_pred
adf.test(resi,alternative = "stationary")
#Dickey-Fuller = -3.9759, Lag order = 3, p-value = 0.02017
#alternative hypothesis: stationary
kpss.test(resi)
#KPSS Level = 0.088036, Truncation lag parameter = 1, p-value = 0.1

#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 months
outdata <- Data_ts[43:48,]
timevals_out <- outdata$Month
global_pred_out <- predict(lmfit,data.frame(Month =timevals_out))
fcast <- global_pred_out

#Now, let's compare our prediction with the actual values, using MAPE
MAPE_class_dec <- forecast::accuracy(fcast,outdata$Value)[5]
MAPE_class_dec
#24.8979

#Let's also plot the predictions along with original values, to
#get a visual feel of the fit
class_dec_pred <- c(ts(global_pred),ts(global_pred_out))
plot(total_timeser, col = "black")
lines(class_dec_pred, col = "red")


######## AUTO ARIMA ########
#So, that was classical decomposition, now let's do an ARIMA fit
autoarima <- auto.arima(timeser)
autoarima
# ARIMA(2,1,0) 
# 
# Coefficients:
#   ar1      ar2
# -0.5796  -0.4906
# s.e.   0.1346   0.1310
# 
# sigma^2 estimated as 168564623:  log likelihood=-445.84
# AIC=897.67   AICc=898.32   BIC=902.81

#Lower the AIC and BIC value, better the model
#Log likelihood must be higher for better model
tsdiag(autoarima)
plot(autoarima$x, col="black")
lines(fitted(autoarima), col="red")

#Again, let's check if the residual series is white noise
resi_auto_arima <- timeser - fitted(autoarima)
adf.test(resi_auto_arima,alternative = "stationary")
#Dickey-Fuller = -4.3522, Lag order = 3, p-value = 0.01
kpss.test(resi_auto_arima)
#KPSS Level = 0.05314, Truncation lag parameter = 1, p-value = 0.1

#Also, let's evaluate the model using MAPE
fcast_auto_arima <- predict(autoarima, n.ahead = 6)
MAPE_auto_arima <- accuracy(fcast_auto_arima$pred,outdata$Value)[5]
MAPE_auto_arima
#28.9226

#Lastly, let's plot the predictions along with original values, to
#get a visual feel of the fit
# auto_arima_pred <- c(fitted(autoarima),ts(fcast_auto_arima$pred))
# plot(total_timeser, col = "black")
# lines(auto_arima_pred, col = "purple")
# plot(forecast(auto_arima_pred, h=12))
# 95% confidence interval is very wide, we can't be sure of the prdictions made.

#As the MAPE value is better for classical model decomposition,
#we will forecast the Revenue (Sales) for EU Consumer using the same.
# Forecasting future months Using Classical Decomposition
forecast.decompose <- predict(lmfit,data.frame(Month =49:54))
# 1        2        3        4        5        6 
# 46730.30 44397.88 41865.95 39875.28 39034.17 39669.43 


####### EU Demand (Quantity) Modeling #######
#For Forecasting Demand(Quantity) for EU Consumer Segment
Data_ts <- Data_EC_Quantity
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
w <- 1
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
##From the plot of smoothed data, we can observe the seasonality (additive)


######## CLASSICAL DECOMPOSITION ########
#Building a model on the smoothed time series using classical decomposition
#First, let's convert the time series to a dataframe
smootheddf <- as.data.frame(cbind(timevals_in, as.vector(smoothedseries)))
colnames(smootheddf) <- c('Month', 'Value')

#Now, let's fit a additive model with trend and seasonality to the data
#Seasonality will be modeled using a sinusoid function
lmfit <- lm(Value ~ sin(0.5*Month) + Month, data=smootheddf)
global_pred <- predict(lmfit, Month=timevals_in)
summary(global_pred)
lines(global_pred, col='red', lwd=2)

#Now, let's look at the locally predictable series
#We will model it as an ARMA series
local_pred <- timeser - global_pred
plot(local_pred, col='red', type = "l")
acf(local_pred)
acf(local_pred, type="partial")
armafit <- auto.arima(local_pred)

tsdiag(armafit)
armafit
# Series: local_pred 
# ARIMA(1,0,2) with non-zero mean 
# 
# Coefficients:
#           ar1     ma1      ma2
#       -0.5238  0.3293  -0.4670
# s.e.   0.2066  0.1923   0.1298
# 
# sigma^2 estimated as 12749:  log likelihood=-256.85
# AIC=521.69   AICc=522.77   BIC=528.64
#

#We'll check if the residual series is white noise
resi <- local_pred - fitted(armafit)
adf.test(resi,alternative = "stationary")
# Dickey-Fuller = -2.5829, Lag order = 3, p-value = 0.3439
#alternative hypothesis: stationary
kpss.test(resi)
#KPSS Level = 0.14343, Truncation lag parameter = 1, p-value = 0.1

#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 months
outdata <- Data_ts[43:48,]
timevals_out <- outdata$Month
global_pred_out <- predict(lmfit,data.frame(Month =timevals_out)) + 
                          predict(armafit, n.ahead=6)$pred
fcast <- global_pred_out 

#Now, let's compare our prediction with the actual values, using MAPE
MAPE_class_dec <- forecast::accuracy(fcast,outdata$Value)[5]
MAPE_class_dec
#27.28549

#Let's also plot the predictions along with original values, to
#get a visual feel of the fit
class_dec_pred <- c(ts(global_pred+fitted(armafit)),ts(global_pred_out))
plot(total_timeser, col = "black")
lines(class_dec_pred, col = "red")


######## AUTO ARIMA ########
#So, that was classical decomposition, now let's do an ARIMA fit
autoarima <- auto.arima(timeser)
autoarima
# ARIMA(2,1,0) 
# 
# Coefficients:
#           ar1      ar2
#       -0.7359  -0.5879
# s.e.   0.1224   0.1185
# 
# sigma^2 estimated as 21185:  log likelihood=-261.9
# AIC=529.8   AICc=530.44   BIC=534.94

tsdiag(autoarima)
plot(autoarima$x, col="black")
lines(fitted(autoarima), col="red")

#Again, let's check if the residual series is white noise
resi_auto_arima <- timeser - fitted(autoarima)
adf.test(resi_auto_arima,alternative = "stationary")
#Dickey-Fuller = -3.5969, Lag order = 3, p-value = 0.04521
kpss.test(resi_auto_arima)
#KPSS Level = 0.047939, Truncation lag parameter = 1, p-value = 0.1

#Also, let's evaluate the model using MAPE
fcast_auto_arima <- predict(autoarima, n.ahead = 6)
MAPE_auto_arima <- accuracy(fcast_auto_arima$pred,outdata$Value)[5]
MAPE_auto_arima
#30.13319

#Lastly, let's plot the predictions along with original values, to
#get a visual feel of the fit
# auto_arima_pred <- c(fitted(autoarima),ts(fcast_auto_arima$pred))
# plot(total_timeser, col = "black")
# lines(auto_arima_pred, col = "purple")
# plot(forecast(auto_arima_pred, h=6))
# 95% confidence interval is very wide, we can't be sure of the prdictions made.

#As the MAPE value is better for classical model decomposition,
#we will forecast the Demand (Quantity) for EU Consumer using the same.
# Forecasting future months
forecast.decompose <- predict(lmfit,data.frame(Month =49:54)) +
                      predict(armafit, n.ahead=12)$pred[7:12]
# 1        2        3        4        5        6 
# 589.2595 562.0990 534.7386 512.4617 503.8577 511.7848  
