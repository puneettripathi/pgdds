
#Perform Basic data quality check, pending
#?????????????????????????????????????????

library('dplyr')

Data <- read.csv("Global Superstore.csv")

Data$Order.Month <- substr(Data$Order.Date, 4, 5)

Data$Order.Year <- substr(Data$Order.Date, 7, 10)

write.csv(Data, file = "Global Superstore1.csv")

Data1 <- group_by(Data, Market, Segment, Order.Month, Order.Year)

Data2 <- summarize(Data1, tot_Sales = sum(Sales),
                          tot_Quantity = sum(Quantity),
                          tot_Profit = sum(Profit))



#Steps are to find the 2 most profitable and consistently profitable segments.
Data3 <- group_by(Data2, Market, Segment)

Data4 <- summarise(Data3, mean_Profit = mean(tot_Profit),
                          sd_Profit = sd(tot_Profit))

Data4$COV <- (Data4$sd_Profit / Data4$mean_Profit) * 100

#As per Max profit, top 2:
#APAC Consumer 4642.0325 2934.3785 0.6321323
#EU Consumer   3930.9939 2454.1398 0.6243052

#As per Min COV, top 2:
#EU Consumer   3930.9939 2454.1398 0.6243052
#APAC Consumer 4642.0325 2934.3785 0.6321323



#Steps for creating time-series for APAC Consumer
Data_AC <- arrange(filter(Data2, Market == 'APAC', Segment == 'Consumer'), Order.Year, Order.Month)
Data_AC$Month <- seq.int(nrow(Data_AC))
TS_AC_Sales <- select(ungroup(Data_AC), Month, tot_Sales)
TS_AC_Quantity <- select(ungroup(Data_AC), Month, tot_Quantity)

#Steps for creating time-series for EU Consumer
Data_EC <- arrange(filter(Data2, Market == 'EU', Segment == 'Consumer'), Order.Year, Order.Month)
Data_EC$Month <- seq.int(nrow(Data_EC))
TS_EC_Sales <- select(ungroup(Data_EC), Month, tot_Sales)
TS_EC_Quantity <- select(ungroup(Data_EC), Month, tot_Quantity)

