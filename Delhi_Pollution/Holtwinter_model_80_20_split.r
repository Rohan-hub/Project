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
tail(new_data)
head(new_data)
# Converting data into time series object
new_data$pm25<-ts(new_data$pm25,frequency = 24*365,start=c(2018,1) , end = c(2018,2617))



# dividing entire data into training and testing data 
train<-new_data[1:2093,]
test<-new_data[2094:2617,]
nrow(test)
class(train)

# converting time series object
train<-ts(train$pm25,frequency = 24*365)
test<-ts(test$pm25,frequency = 24*365)

#### USING HoltWinters function ################
# Optimum values
# with alpha = 0.2 which is default value
# Assuming time series data has only level parameter
hw_a<-HoltWinters(train,alpha = 0.2,beta = F,gamma = F)
hw_a
accuracy(hw_a$fitted,train)  ### RMSE - 88.91
  hwa_pred<-data.frame(predict(hw_a,n.ahead=524))
# By looking at the plot the forecasted values are not showing any characters of train data 
plot(forecast(hw_a,h=524))
accuracy(hwa_pred$fit,test) ## RMSE - 87.62

# with alpha = 0.2, beta = 0.1
# Assuming time series data has level and trend parameter 
hw_ab<-HoltWinters(train,alpha = 0.2,beta = 0.1,gamma = F)
hw_ab
accuracy(hw_ab$fitted,train) ## RMSE - 101.522
hwab_pred<-data.frame(predict(hw_ab,n.ahead = 524))
# by looking at the plot the forecasted values are still missing some characters exhibited by train data
plot(forecast(hw_ab,h=524))
accuracy(hwab_pred$fit,test) ## RMSE - 247.15

# with alpha = 0.2, beta = 0.1, gamma = 0.1 
# Assuming time series data has level,trend and seasonality 
hw_abg<-HoltWinters(train,alpha = 0.2,beta = 0.1,gamma = 0.1 ) ## ERROR
hw_abg
hwabg_pred<-data.frame(predict(hw_abg,n.ahead = 12))
# by looking at the plot the characters of forecasted values are closely following historical data
plot(forecast(hw_abg,h=12))
hwabg_mape<-MAPE(hwabg_pred$fit,test)*100

# With out optimum values 
hw_na<-HoltWinters(train,beta = F,gamma = F)
hw_na
accuracy(hw_na$fitted,train) ## RMSE - 55.12
hwna_pred<-data.frame(predict(hw_na,n.ahead = 524))
hwna_pred
plot(forecast(hw_na,h=524))
accuracy(hwna_pred$fit,test)  ## RMSE - 87.93

hw_nab<-HoltWinters(train,gamma=F)
hw_nab
accuracy(hw_nab$fitted,train) ## RMSE - 55.27
hwnab_pred<-data.frame(predict(hw_nab,n.ahead=524))
hwnab_pred
plot(forecast(hw_nab,h=24))
accuracy(hwnab_pred$fit,test) ## RMSE - 86.83
