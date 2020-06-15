library(forecast)
library(fpp)
library(smooth)
library(caret)
install.packages("imputeTS")
library(imputeTS)
library(padr)
library(dplyr)
library(ggplot2)
library(readxl)
air_pollution<-read_excel("D:/Project/Dataset and Requirements/Air_Pollution.xlsx")
str(air_pollution)
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

View(new_data)

# dividing entire data into training and testing data 
train<-new_data[1:2094,]
test<-new_data[2095:2617,]
nrow(test)

# converting time series object
train$pm25<-ts(train$pm25)
test$pm25<-ts(test$pm25)

fit_nnetar<- nnetar(train$pm25)
accuracy(fit_nnetar)

forecast_air<-forecast(fit_nnetar,h=523)
pred<-predict(forecast_air,n.head=523)
pred1<-data.frame(predict(forecast_air,n.head=523))
pred1
accuracy(pred1$Point.Forecast,test)

