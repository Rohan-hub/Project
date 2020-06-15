##### Importing libraries #######
library(forecast)
library(fpp)
library(smooth)
library(caret)
library(imputeTS)
library(padr)
library(dplyr)
library(ggplot2)
library(readxl)
library(padr)

##### Importing the air pollution data set ########

air_pollution<-read_excel("D:/Project/Dataset and Requirements/Air_Pollution.xlsx")

#### Converting pm25 value into numeric #####

air_pollution$pm25<-as.numeric(air_pollution$pm25)



#### Imputing the missing values ######

new_data<-pad(air_pollution, interval = NULL, start_val = NULL, end_val = NULL,
              by = NULL, group = NULL, break_above = 1)


### Using moving average imputation to fill the NA value in data set ###
new_data$pm25 <-na_seasplit(new_data$pm25,algorithm = 'ma',find_frequency = TRUE)


# Converting data into time series object
new_data$pm25<-ts(new_data$pm25,frequency = 24*365,start=c(2018,1) , end = c(2018,2617))

### Building nnetar model ####

model<-nnetar(new_data$pm25)


### Save model to RDS file ####
saveRDS(model,"model.RDS")
