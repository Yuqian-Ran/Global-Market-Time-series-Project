library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(forecast)
setwd('/Users/qianer_r/Project/Time series/Data')
global_data <- read.csv('Global Superstore.csv')
str(global_data ) # there is NA values in postal Code
summary(global_data )
sum(duplicated(global_data ))  
# duplicate = 0 means there is no duplicates
pctmiss <- colSums(is.na(global_data))/nrow(global_data)
round(pctmiss, 2)
# proportion of miss value for posttal.code is largest = 0.81
sum(is.na(global_data))
# there are 41296 missing value in postal.code
#Deleting Postal Code as it contains lots of NA and its not important for forcasting
global_data <- subset(global_data, TRUE, -c(Postal.Code))
#Converting Data factors to Data
global_data$Order.Date <- as.Date(global_data$Order.Date, "%d-%m-%Y")
global_data$Ship.Date <- as.Date(global_data$Ship.Date, "%d-%m-%Y")
#Checking Order Dates duration
sum(duplicated(global_data$Order.Date)) #49860
summary(global_data$Order.Date)
#---------------------------------------------------------Data preparation----------------------------------------------------------------------
global_data$Market_Segment <- paste(global_data$Market, global_data$Segment, sep = "_")
global_data$Year_Month <- format(as.Date(global_data$Order.Date), "%Y-%m")

#Step 1: Subsetting data into buckets

invisible(lapply(split(global_data, global_data$Market_Segment), 
                 function(x) {assign(paste(x$Market_Segment[1]), x, pos = .GlobalEnv)}))
global_data

#Step2: Aggregating market buckets ob order date by sales, quantity, profit & CV

data_aggregated <- global_data %>% group_by(Market_Segment) %>%
  summarise(.,sum(Sales), sum(Profit), sum(Quantity), sd(Profit)*100/mean(Profit))
colnames(data_aggregated) <- c("Market_Segment","Total_Sales","Total_Profit","Total Quantity","CV")
data_aggregated[order(data_aggregated$Total_Profit,decreasing= T), ][1:2,]


#Aggregating data by segment & month

data_aggregated_monthly <- global_data %>%
  group_by(Market_Segment, Year_Month) %>%
  summarise(.,sum(Sales), sum(Profit), sum(Quantity), sd(Profit)*100/mean(Profit))
colnames(data_aggregated_monthly) <- c("Market_Segment","Order_Month","Total_Sales",
                                      "Total_Profit","Total Quantity","CV")
data_aggregated_monthly[order(data_aggregated_monthly$Total_Profit,decreasing= T), ]
#Data plots
#Plot Sales, Profit & CV by Segment and Market
sales_profit <- global_data[, c("Profit","Sales","Market","Segment","Quantity")] %>% 
  group_by(Market, Segment) %>% 
  summarise(., sum(Sales),sum(Profit),sd(Profit)*100/mean(Profit))
colnames(sales_profit) = c("Market","Segment","Sales","Profit","CV")
ggplot(sales_profit, aes(Segment, Sales, fill=Market)) + geom_bar(position = "dodge",stat = "identity")
ggplot(sales_profit, aes(Segment, Profit, fill=Market)) + geom_bar(position = "dodge",stat = "identity")
ggplot(sales_profit, aes(Segment, CV, fill=Market)) + geom_bar(position = "dodge",stat = "identity")


#---------------------------------------------------------Model Building----------------------------------------------------------------------
#Subsetting data_aggregated_monthly for top 2 market segments
top2 <- subset(data_aggregated_monthly, Market_Segment == "EU_Consumer" | Market_Segment == "APAC_Consumer")
str(top2)
#Getting Top 2 Market Segments
invisible(lapply(split(top2, top2$Market_Segment), 
                 function(x){assign(paste0("Top_", x$Market_Segment[1]), x, pos = .GlobalEnv)}))
APAC_Consumer_Agg <- Top_APAC_Consumer[,c(2:5)]
EU_Consumer_Agg <- Top_EU_Consumer[,c(2:5)]

#-------------------------------------------------APAC Consumer Sales Forecast--------------------------------------------
#Creating APAC sales timeseries
APAC_Consumer_Sales_TS <- ts(APAC_Consumer_Agg$Total_Sales,frequency=12,start=c(2011,1),end=c(2014,12))
plot(APAC_Consumer_Sales_TS)
#Testing Holt winters for Smoothing
cols <- c("red", "blue", "green", "black")
alphas <- c(0.2, 0.99, 0.56)
labels <- c(paste("alpha =", alphas), "Original")
for (i in seq(1,length(alphas))) {
  smoothedseries <- HoltWinters(APAC_Consumer_Sales_TS, alpha=alphas[i],beta=FALSE, gamma=FALSE)
  lines(fitted(smoothedseries)[,1], col=cols[i], lwd=2)
}
legend("topleft", labels, col=cols, lwd=2)
smoothedseries <- HoltWinters(APAC_Consumer_Sales_TS, alpha=.3,beta=FALSE, gamma=FALSE)
#Smoothing time series using Moving Average method
w <- 4
APAC_Consumer_Sales_Smoothed <- stats::filter(APAC_Consumer_Sales_TS,filter=rep(1/w,w,method='convolution',sides=2)) 
APAC_Consumer_Sales_Smoothed

#Adding NA values to left & right introduced by smoothing
#Smoothing left end of the time series
diff <- APAC_Consumer_Sales_Smoothed[w+2] - APAC_Consumer_Sales_Smoothed[w+1]
for (i in seq(w,1,-1)) {
  APAC_Consumer_Sales_Smoothed[i] <- APAC_Consumer_Sales_Smoothed[i+1] - diff
}
#Smoothing right end of the time series
n <- length(APAC_Consumer_Sales_Smoothed)
diff <- APAC_Consumer_Sales_Smoothed[n-w] - APAC_Consumer_Sales_Smoothed[n-w-1]
for (i in seq(n-w+1, n)) {
  APAC_Consumer_Sales_Smoothed[i] <- APAC_Consumer_Sales_Smoothed[i-1] + diff
}

APAC_Consumer_Sales_Smoothed

#Converting smoothed series to data frame with Order_Month & renaming columns
#Using moving average smoothed time series here
APAC_Consumer_Sales_df <- data.frame(cbind(APAC_Consumer_Agg$Order_Month,APAC_Consumer_Sales_Smoothed))
colnames(APAC_Consumer_Sales_df) <- c("Month","Sales")
APAC_Consumer_Sales_df
class(APAC_Consumer_Sales_df$Sales)
APAC_Consumer_Sales_df$Sales <- as.numeric(as.character((APAC_Consumer_Sales_df$Sales)))
APAC_Consumer_Sales_df

#Plotting smoothed series
plot(APAC_Consumer_Sales_TS)
lines(APAC_Consumer_Sales_Smoothed,col='blue',lwd=2)
lines(fitted(smoothedseries)[,1],col='red',lwd=2)

#Again creating time series from dataframe
APAC_Consumer_Sales_TS <- ts(APAC_Consumer_Sales_df$Sales,frequency=12,start=c(2011,1),end=c(2014,12))
#Creating train & validation sets
ntest <- 6
length(APAC_Consumer_Sales_TS) #48
nTrain <- length(APAC_Consumer_Sales_TS)-ntest
train.ts <- window(APAC_Consumer_Sales_TS,start = c(2011,1),end=c(2011,nTrain)) #1-42 month 2011.1-2014.6
valid.ts <- window(APAC_Consumer_Sales_TS,start=c(2011,nTrain+1), end=c(2011,nTrain+ntest)) #43-48 month 2014.7-2014.12 
test.ts <- ts(APAC_Consumer_Sales_df$Sales,frequency=12,start=c(2015,1),end=c(2015,6))  #49-54 month

#Model 1: Regression model based on trend & seasonality sin(2*pi*trend/12)
train.lm <- tslm(train.ts~trend+I(sin(2*pi*trend/12))+season)
summary(train.lm)
train.lm.forecast <- forecast(train.lm,h=ntest,level=0)
plot(APAC_Consumer_Sales_Smoothed,ylab="Sales",xlab="Time",bty='l',xaxt='n',col="red",title=c("APAC Consumer Sales"))  
lines(train.lm.forecast$fitted,lwd=2,col="black",lty=4)
lines(train.lm.forecast$lower,col="blue")
#Checking MAPE for our linear model
APAC_consumer_accuracy <- accuracy(train.lm.forecast$mean,valid.ts)  
APAC_consumer_accuracy #MAPE=13.54141
#ACF and PACF plots 
acf(train.lm.forecast$residuals,lag.max = 12)
pacf(train.lm.forecast$residuals,lag.max = 12)

#Model 2: Auto ARIMA model
autoarima_ts <- auto.arima(train.ts)
tsdiag(autoarima_ts)
autoarima_forecast <- forecast(autoarima_ts,h=ntest)
autoarima_acc <- accuracy(autoarima_forecast,valid.ts)
plot(autoarima_forecast)
autoarima_acc
#MAPE=5.52011 for Training set & 14.48387 for Validation set for Auto ARIMA

#Forecast for months 49 to 54(Jan 2015 to June 2015) using Regresssion model
train.lm.model <- tslm(APAC_Consumer_Sales_TS~trend+I(sin(2*pi*trend/12))+season)
summary(train.lm.model)
train.lm.total.forecast <- forecast(train.lm.model,h=6,level=0)
train.lm.total.forecast
plot(train.lm.total.forecast,col="black")

#Forecast for months 49 to 54(Jan 2015 to June 2015) using Auto ARIMA model
autoarima_ts <- auto.arima(APAC_Consumer_Sales_TS)
tsdiag(autoarima_ts)
autoarima_forecast <- forecast(autoarima_ts,h=ntest)
plot(autoarima_forecast)

#--------------------------------------------------APAC Consumer Quantity Forecast--------------------------------------------
#Creating APAC Quantity timeseries
APAC_Consumer_Quantity_TS <- ts(APAC_Consumer_Agg$`Total Quantity`,frequency=12,start=c(2011,1),end=c(2014,12))
plot(APAC_Consumer_Quantity_TS)

#Testing Holt winters for Smoothing
plot(APAC_Consumer_Quantity_TS)
cols <- c("red", "blue", "green", "black")
alphas <- c(0.2, 0.99, 0.56)
labels <- c(paste("alpha =", alphas), "Original")
for (i in seq(1,length(alphas))) {
  smoothedseries <- HoltWinters(APAC_Consumer_Quantity_TS, alpha=alphas[i],beta=FALSE, gamma=FALSE)
  lines(fitted(smoothedseries)[,1], col=cols[i], lwd=2)
}
legend("topleft", labels, col=cols, lwd=2)

#Smoothing time series using Moving Average method
w <- 4
APAC_Consumer_Quantity_Smoothed <- stats::filter(APAC_Consumer_Quantity_TS,filter=rep(1/w,w,method='convolution',sides=2)) 
#Adding NA values to left & right introduced by smoothing
#Smoothing left end of the time series
diff <- APAC_Consumer_Quantity_Smoothed[w+2] - APAC_Consumer_Quantity_Smoothed[w+1]
for (i in seq(w,1,-1)) {
  APAC_Consumer_Quantity_Smoothed[i] <- APAC_Consumer_Quantity_Smoothed[i+1] - diff
}
#Smoothing right end of the time series
n <- length(APAC_Consumer_Quantity_Smoothed)
diff <- APAC_Consumer_Quantity_Smoothed[n-w] - APAC_Consumer_Quantity_Smoothed[n-w-1]
for (i in seq(n-w+1, n)) {
  APAC_Consumer_Quantity_Smoothed[i] <- APAC_Consumer_Quantity_Smoothed[i-1] + diff
}

#Converting smoothed series to data frame with Order_Month & renaming columns
#Using moving average smoothed time series here
APAC_Consumer_Quantity_df <- data.frame(cbind(APAC_Consumer_Agg$Order_Month,APAC_Consumer_Quantity_Smoothed))
colnames(APAC_Consumer_Quantity_df) <- c("Month","Quantity")

#Changing Quantity type
APAC_Consumer_Quantity_df$Quantity <- as.numeric(as.character((APAC_Consumer_Quantity_df$Quantity)))

#Plotting smoothed series
plot(APAC_Consumer_Quantity_TS)
lines(APAC_Consumer_Quantity_Smoothed,col='blue',lwd=2)
lines(fitted(smoothedseries)[,1],col='red',lwd=2)

#Again creating time series from dataframe
APAC_Consumer_Quantity_TS <- ts(APAC_Consumer_Quantity_df$Quantity,frequency=12,start=c(2011,1),end=c(2014,12))

#Creating train & validation sets
ntest <- 6
nTrain <- length(APAC_Consumer_Quantity_TS)-ntest
train.ts <- window(APAC_Consumer_Quantity_TS,start = c(2011,1),end=c(2011,nTrain))
valid.ts <- window(APAC_Consumer_Quantity_TS,start=c(2011,nTrain+1), end=c(2011,nTrain+ntest))
test.ts <- ts(APAC_Consumer_Quantity_df$Quantity,frequency=12,start=c(2015,1),end=c(2015,6))

#Model 1: Regression model based on trend & seasonality sin(2*pi*trend/12)
train.lm <- tslm(train.ts~trend+I(sin(2*pi*trend/12))+season)
summary(train.lm)
train.lm.forecast <- forecast(train.lm,h=ntest,level=0)
plot(APAC_Consumer_Quantity_Smoothed,ylab="Quantity",xlab="Time",bty='l',xaxt='n',col="red")  
lines(train.lm.forecast$fitted,lwd=2,col="black",lty=4)
lines(train.lm.forecast$lower,col="blue")
#Checking MAPE for our linear model
APAC_consumer_accuracy <- accuracy(train.lm.forecast$mean,valid.ts)  
APAC_consumer_accuracy #MAPE=9.580049
#ACF and PACF plots 
acf(train.lm.forecast$residuals,lag.max = 12)
pacf(train.lm.forecast$residuals,lag.max = 12)

#Model 2: Auto ARIMA model
autoarima_ts <- auto.arima(train.ts)
tsdiag(autoarima_ts)
autoarima_forecast <- forecast(autoarima_ts,h=ntest)
autoarima_acc <- accuracy(autoarima_forecast,valid.ts)
plot(autoarima_forecast)
autoarima_acc
#MAPE=9.003592 for Training set & 16.622294 for Validation set for Auto ARIMA

#Forecast for months 49 to 54(Jan 2015 to June 2015) using Regresssion model
train.lm.model <- tslm(APAC_Consumer_Quantity_TS~trend+I(sin(2*pi*trend/12))+season)
summary(train.lm.model)
train.lm.total.forecast <- forecast(train.lm.model,h=6,level=0)
train.lm.total.forecast
plot(train.lm.total.forecast,col="black")

#Forecast for months 49 to 54(Jan 2015 to June 2015) using Auto ARIMA model
autoarima_ts <- auto.arima(APAC_Consumer_Quantity_TS)
tsdiag(autoarima_ts)
autoarima_forecast <- forecast(autoarima_ts,h=ntest)
plot(autoarima_forecast)

#--------------------------------------------------EU Consumer Sales Forecast--------------------------------------------
#Creating EU Sales timeseries
EU_Consumer_Sales_TS <- ts(EU_Consumer_Agg$Total_Sales,frequency=12,start=c(2011,1),end=c(2014,12))
plot(EU_Consumer_Sales_TS)

#Testing Holt winters for Smoothing
plot(EU_Consumer_Sales_TS)
cols <- c("red", "blue", "green", "black")
alphas <- c(0.2, 0.99, 0.56)
labels <- c(paste("alpha =", alphas), "Original")
for (i in seq(1,length(alphas))) {
  smoothedseries <- HoltWinters(EU_Consumer_Sales_TS, alpha=alphas[i],beta=FALSE, gamma=FALSE)
  lines(fitted(smoothedseries)[,1], col=cols[i], lwd=2)
}
legend("topleft", labels, col=cols, lwd=2)

#Smoothing time series using Moving Average method
w <- 5
EU_Consumer_Sales_Smoothed <- stats::filter(EU_Consumer_Sales_TS,filter=rep(1/w,w,method='convolution',sides=2)) 
#Adding NA values to left & right introduced by smoothing
#Smoothing left end of the time series
diff <- EU_Consumer_Sales_Smoothed[w+2] - EU_Consumer_Sales_Smoothed[w+1]
for (i in seq(w,1,-1)) {
  EU_Consumer_Sales_Smoothed[i] <- EU_Consumer_Sales_Smoothed[i+1] - diff
}
#Smoothing right end of the time series
n <- length(EU_Consumer_Sales_Smoothed)
diff <- EU_Consumer_Sales_Smoothed[n-w] - EU_Consumer_Sales_Smoothed[n-w-1]
for (i in seq(n-w+1, n)) {
  EU_Consumer_Sales_Smoothed[i] <- EU_Consumer_Sales_Smoothed[i-1] + diff
}

#Converting smoothed series to data frame with Order_Month & renaming columns
#Using moving average smoothed time series here
EU_Consumer_Sales_df <- data.frame(cbind(EU_Consumer_Agg$Order_Month,EU_Consumer_Sales_Smoothed))
colnames(EU_Consumer_Sales_df) <- c("Month","Sales")

#Changing Sales type
EU_Consumer_Sales_df$Sales <- as.numeric(as.character((EU_Consumer_Sales_df$Sales)))

#Plotting smoothed series
plot(EU_Consumer_Sales_TS)
lines(EU_Consumer_Sales_Smoothed,col='blue',lwd=2)
lines(fitted(smoothedseries)[,1],col='red',lwd=2)

#Again creating time series from dataframe
EU_Consumer_Sales_TS <- ts(EU_Consumer_Sales_df$Sales,frequency=12,start=c(2011,1),end=c(2014,12))

#Creating train & validation sets
ntest <- 6
nTrain <- length(EU_Consumer_Sales_TS)-ntest
train.ts <- window(EU_Consumer_Sales_TS,start = c(2011,1),end=c(2011,nTrain))
valid.ts <- window(EU_Consumer_Sales_TS,start=c(2011,nTrain+1), end=c(2011,nTrain+ntest))
test.ts <- ts(EU_Consumer_Sales_df$Sales,frequency=12,start=c(2015,1),end=c(2015,6))

#Model 1: Regression model based on trend & seasonality sin(2*pi*trend/12)
train.lm <- tslm(train.ts~trend+I(sin(2*pi*trend/12))+season)
summary(train.lm)
train.lm.forecast <- forecast(train.lm,h=ntest,level=0)
plot(EU_Consumer_Sales_Smoothed,ylab="Sales",xlab="Time",bty='l',xaxt='n',col="red")  
lines(train.lm.forecast$fitted,lwd=2,col="black",lty=3)
lines(train.lm.forecast$lower,col="blue")
#Checking MAPE for our linear model
EU_consumer_accuracy <- accuracy(train.lm.forecast$mean,valid.ts)  
EU_consumer_accuracy #MAPE=13.80817
#ACF and PACF plots 
acf(train.lm.forecast$residuals,lag.max = 12)
pacf(train.lm.forecast$residuals,lag.max = 12)

#Model 2: Auto ARIMA model
autoarima_ts <- auto.arima(train.ts)
tsdiag(autoarima_ts)
autoarima_forecast <- forecast(autoarima_ts,h=ntest)
autoarima_acc <- accuracy(autoarima_forecast,valid.ts)
plot(autoarima_forecast)
autoarima_acc
#MAPE=5.098584 for Training set & 3.343440 for Validation set for Auto ARIMA

#Forecast for months 49 to 54(Jan 2015 to June 2015) using Auto Regression model
train.lm.model <- tslm(EU_Consumer_Sales_TS~trend+I(sin(2*pi*trend/12))+season)
summary(train.lm.model)
train.lm.total.forecast <- forecast(train.lm.model,h=6,level=0)
train.lm.total.forecast
plot(train.lm.total.forecast,col="black")

#Forecast for months 49 to 54(Jan 2015 to June 2015) using Auto ARIMA model
autoarima_ts <- auto.arima(EU_Consumer_Sales_TS)
tsdiag(autoarima_ts)
autoarima_forecast <- forecast(autoarima_ts,h=ntest)
plot(autoarima_forecast)

#--------------------------------------------------EU Consumer Quantity Forecast--------------------------------------------
#Creating EU Quantity timeseries
EU_Consumer_Quantity_TS <- ts(EU_Consumer_Agg$`Total Quantity`,frequency=12,start=c(2011,1),end=c(2014,12))
plot(EU_Consumer_Quantity_TS)

#Testing Holt winters for Smoothing
plot(EU_Consumer_Quantity_TS)
cols <- c("red", "blue", "green", "black")
alphas <- c(0.2, 0.99, 0.56)
labels <- c(paste("alpha =", alphas), "Original")
for (i in seq(1,length(alphas))) {
  smoothedseries <- HoltWinters(EU_Consumer_Quantity_TS, alpha=alphas[i],beta=FALSE, gamma=FALSE)
  lines(fitted(smoothedseries)[,1], col=cols[i], lwd=2)
}
legend("topleft", labels, col=cols, lwd=2)

#Smoothing time series using Moving Average method
w <- 3
EU_Consumer_Quantity_Smoothed <- stats::filter(EU_Consumer_Quantity_TS,filter=rep(1/w,w,method='convolution',sides=2)) 
#Adding NA values to left & right introduced by smoothing
#Smoothing left end of the time series
diff <- EU_Consumer_Quantity_Smoothed[w+2] - EU_Consumer_Quantity_Smoothed[w+1]
for (i in seq(w,1,-1)) {
  EU_Consumer_Quantity_Smoothed[i] <- EU_Consumer_Quantity_Smoothed[i+1] - diff
}
#Smoothing right end of the time series
n <- length(EU_Consumer_Quantity_Smoothed)
diff <- EU_Consumer_Quantity_Smoothed[n-w] - EU_Consumer_Quantity_Smoothed[n-w-1]
for (i in seq(n-w+1, n)) {
  EU_Consumer_Quantity_Smoothed[i] <- EU_Consumer_Quantity_Smoothed[i-1] + diff
}

#Converting smoothed series to data frame with Order_Month & renaming columns
#Using moving average smoothed time series here
EU_Consumer_Quantity_df <- data.frame(cbind(EU_Consumer_Agg$Order_Month,EU_Consumer_Quantity_Smoothed))
colnames(EU_Consumer_Quantity_df) <- c("Month","Quantity")

#Changing Quantity type
EU_Consumer_Quantity_df$Quantity <- as.numeric(as.character((EU_Consumer_Quantity_df$Quantity)))

#Plotting smoothed series
plot(EU_Consumer_Quantity_TS)
lines(EU_Consumer_Quantity_Smoothed,col='blue',lwd=2)
lines(fitted(smoothedseries)[,1],col='red',lwd=2)

#Again creating time series from dataframe
EU_Consumer_Quantity_TS <- ts(EU_Consumer_Quantity_df$Quantity,frequency=12,start=c(2011,1),end=c(2014,12))

#Creating train & validation sets
ntest <- 6
nTrain <- length(EU_Consumer_Quantity_TS)-ntest
train.ts <- window(EU_Consumer_Quantity_TS,start = c(2011,1),end=c(2011,nTrain))
valid.ts <- window(EU_Consumer_Quantity_TS,start=c(2011,nTrain+1), end=c(2011,nTrain+ntest))
test.ts <- ts(EU_Consumer_Quantity_df$Quantity,frequency=12,start=c(2015,1),end=c(2015,6))

#Model 1: Regression model based on trend & seasonality sin(2*pi*trend/12)
train.lm <- tslm(train.ts~trend+I(sin(2*pi*trend/12))+season)
summary(train.lm)
train.lm.forecast <- forecast(train.lm,h=ntest,level=0)
plot(EU_Consumer_Quantity_Smoothed,ylab="Quantity",xlab="Time",bty='l',xaxt='n',col="red")  
lines(train.lm.forecast$fitted,lwd=2,col="black",lty=3)
lines(train.lm.forecast$lower,col="blue")
#Checking MAPE for our linear model
EU_consumer_accuracy <- accuracy(train.lm.forecast$mean,valid.ts)  
EU_consumer_accuracy #MAPE=13.72469
#ACF and PACF plots 
acf(train.lm.forecast$residuals,lag.max = 12)
pacf(train.lm.forecast$residuals,lag.max = 12)

#Model 2: Auto ARIMA model
autoarima_ts <- auto.arima(train.ts)
tsdiag(autoarima_ts)
autoarima_forecast <- forecast(autoarima_ts,h=ntest)
autoarima_acc <- accuracy(autoarima_forecast,valid.ts)
plot(autoarima_forecast)
autoarima_acc
#MAPE=6.244317 for Training set & 19.685536 for Validation set for Auto ARIMA

#Forecast for months 49 to 54(Jan 2015 to June 2015) using Auto Regression model
train.lm.model <- tslm(EU_Consumer_Quantity_TS~trend+I(sin(2*pi*trend/12))+season)
summary(train.lm.model)
train.lm.total.forecast <- forecast(train.lm.model,h=6,level=0)
train.lm.total.forecast
plot(train.lm.total.forecast,col="black")


#Forecast for months 49 to 54(Jan 2015 to June 2015) using Auto ARIMA model
autoarima_ts <- auto.arima(EU_Consumer_Quantity_TS)
tsdiag(autoarima_ts)
autoarima_forecast <- forecast(autoarima_ts,h=ntest)
plot(autoarima_forecast)








