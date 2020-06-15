library(forecast)
library(fpp)
library(smooth)
install.packages("imputeTS")
library(imputeTS)
library(padr)
library(dplyr)
library(ggplot2)
library(readxl)
air_pollution<-read_excel("D:/Project/Dataset and Requirements/Air_Pollution.xlsx")
str(air_pollution)
air_pollution$pm25<-as.numeric(air_pollution$pm25)
summary(air_pollution)

### Creating a copy of data set
df<-air_pollution
range(df$date)

## Since there are missing dates and time.Using pad function we are adding those missing
##Valuses in Data
install.packages("padr")
library(padr)
new_data<-pad(df, interval = NULL, start_val = NULL, end_val = NULL,
    by = NULL, group = NULL, break_above = 1)
view(new_data)
summary(new_data)
nrow(new_data)

#### Scatter plot
plot(new_data,xlab="Month", ylab = "pm2.5",
     main="pm2.5 values from Jan to Apr", type = "o")

# Converting data into time series object
df1<-ts(new_data,frequency = 60,start=c(423))
view(df1)

### omitting na Value
df_omit<-na.omit(new_data)
df2<-ts(df_omit,frequency = 60,start=c(423))
view(df2)

### To Check first if data is stationary or not
 
### Decomposer ###
decomposedRes <- decompose(df2)
plot (decomposedRes)
decomposedRes_mult <- decompose(df2 , type = "mult")
plot (decomposedRes_mult)

##Autocorrelation is the correlation of a Time Series with lags of itself
##Partial Autocorrelation is the correlation of the time series with a lag of itself, 
##with the linear dependence of all the lags between them removed.

acf <- acf(df2) # autocorrelation
pacf <- pacf(df2)  # partial autocorrelation
ccf <- ccf(mdeaths, fdeaths, ylab = "cross-correlation") # computes cross correlation between 2 timeseries.

library(tseries)
adf.test(df2[,1],alternative = "stationary") # p-value < 0.05 indicates the TS is stationary
kpss.test(df2[,1])

#Ljung Box Test
Box.test(df2[,1],lag=3,type = 'Ljung-Box')

# Seasonal Differencing
#nsdiffs(df2)  # number for seasonal differencing needed
#> 1
#df2_seasdiff <- diff(df2, lag=frequency(df2), differences=1)  # seasonal differencing
#plot(ap_seasdiff, type="l", main="Seasonally Differenced")  # still not stationary!

# Make it stationary
#ndiffs(df2_seasdiff)  # number of differences need to make it stationary
#> 1 
#stationaryTS <- diff(df2_seasdiff, differences= 1)
#plot(stationaryTS, type="l", main="Differenced and Stationary")  # appears to be stationary



plotNA.distribution(new_data$pm25)
plotNA.distributionBar(new_data$pm25)	
statsNA(new_data$pm25) #library(imputeTS)

#### Imputation ########
#df_interpolation<-na_interpolation(new_data)
#plot(df_interpolation, type = "o")
new_data$df_interpolation<-na_seasplit(new_data$pm25,algorithm = 'interpolation',find_frequency = TRUE)
head(new_data)
ggplot(data = new_data ,aes(x= date,y= new_data$df_intrpolation)) + 
  geom_line(colour='red') +
  geom_line(colour='blue',data = new_data,aes(x= date,y= pm25))+
  theme(panel.background = element_blank())+
  ylim(0,700)+
  ggtitle("Interpolation Values")

#df_kalman<-na_kalman(new_data)
#plot(df_kalman,type = "o")
df_kalman <-na_seasplit(new_data$pm25,algorithm = 'kalman',find_frequency = TRUE)
plot(df_kalman , type = "o")

#df_locf<-na_locf(new_data)
#plot(df_locf, type = "o")
df_locf <-na_seasplit(new_data$pm25,algorithm = 'locf',find_frequency = TRUE)
plot(df_locf , type = "o")

#df_ma<-na_ma(new_data)
#plot(df_ma, type = "o")
df_ma <-na_seasplit(new_data$pm25,algorithm = 'ma',find_frequency = TRUE)
plot(df_ma_2, type = "o")

#df_mean<-na_mean(new_data)
#plot(df_mean, type = "o")
df_mean <-na_seasplit(new_data$pm25,algorithm = 'mean',find_frequency = TRUE)
plot(df_mean , type = "o")

#df_random<-na_random(new_data)
#plot(df_random, type = "o")
summary(df_random)
df_random <-na_seasplit(new_data$pm25,algorithm = 'random',find_frequency = TRUE)
plot(df_random , type = "o")

#df_remove<-na_remove(new_data$pm25)
#plot(df_remove, type = "o")
df_remove <-na_seasplit(new_data$pm25,algorithm = 'remove',find_frequency = TRUE)
plot(df_remove , type = "o")

#df_seadec<-na_seadec(new_data)
#plot(df_seadec, type = "o")
df_seadec <-na_seasplit(new_data$pm25,algorithm = 'seadec',find_frequency = TRUE)
plot(df_seadec , type = "o")

df_seasplit<-na_seasplit(new_data)
plot(df_seasplit, type = "o")


install.packages("mice")
library(mice)
md.pattern(new_data)

library(VIM)
aggr_plot <- aggr(new_data, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE,
                  labels=names(data), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))

