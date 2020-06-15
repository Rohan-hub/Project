library(forecast)
library(fpp)
library(smooth)
install.packages("imputeTS")
library(imputeTS)
library(padr)
library(dplyr)
library(ggplot2)
library(readxl)

## Importing Data set
air_pollution<-read_excel("D:/Project/Dataset and Requirements/Air_Pollution.xlsx")

### Creating copy of data set. 
df<-air_pollution ### Research to be made on this data set

df$pm25<-as.numeric(df$pm25) # converting pm25 value into numeric for further research

## Since there are missing dates and time.Using pad function we are adding those missing
##Valuses in Data and creating new data set
install.packages("padr")
library(padr)
new_data<-pad(df, interval = NULL, start_val = NULL, end_val = NULL,
              by = NULL, group = NULL, break_above = 1)

### Using moving average imputation to fill the NA value in data set
new_data$pm25 <-na_seasplit(new_data$pm25,algorithm = 'ma',find_frequency = TRUE)

# dividing entire data into training and testing data 
train<-new_data[1:2593,]
test<-new_data[2594:2617,]
class(train)

# converting time series object
train<-ts(train$pm25,frequency = 24)
test<-ts(test$pm25,frequency = 24)

# Optimum values
# with alpha = 0.2
# Simple Exponential smoothing 
library(smooth)
library(forecast)
library(fpp)
ses_a<-ses(train,alpha = 0.2) 
ses_a
sesa_pred<-data.frame(predict(ses_a,h=4))
forcastses<-forecast(ses_a,n.ahead=24)
autoplot(forecast(ses_a,n.ahead=24))
autoplot(forcastses$fitted,x= test)
accuracy(forcastses$fitted,x= test) ### RMSE - 304.601

# with alpha = 0.2, beta = 0.1

holt_ab<-holt(train,alpha = 0.2,beta = 0.1)
holt_ab
holtab_pred<-data.frame(predict(holt_ab,h=4))
forcast_holt<-forecast(holt_ab,n.aheadh=24)
forcast_holt
autoplot(forcast_holt$fitted,x = test)
accuracy(forcast_holt$fitted,x= test) ## RMSE - 300.27
