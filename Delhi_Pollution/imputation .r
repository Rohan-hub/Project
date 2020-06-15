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
frequency(air_pollution)

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
frequency(new_data)

# Converting data into time series object
df1<-ts(new_data$pm25,frequency = 1,start=c(423))
view(df1)

plotNA.distribution(new_data$pm25)
plotNA.distributionBar(new_data$pm25)	
statsNA(new_data$pm25)

install.packages("mice")
library(mice)
md.pattern(new_data)

library(VIM)
aggr_plot <- aggr(new_data, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE,
                  labels=names(data), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))


### omitting na Value
#df_omit<-na.omit(new_data)
#df2<-ts(df_omit,frequency = 60,start=c(423))
#view(df2)

### Decomposer ###
#decomposedRes <- decompose(df2)
#plot (decomposedRes)
#decomposedRes_mult <- decompose(df2 , type = "mult")
#plot (decomposedRes_mult)

#### Imputation ########

### Interpolation

#df_interpolation<-na_interpolation(new_data)
#plot(df_interpolation, type = "o")

new_data$df_interpolation<-na_seasplit(new_data$pm25,algorithm = 'interpolation',find_frequency = TRUE)

int_data_ts<-ts(new_data$df_interpolation,frequency = 24,start = 2018-01-01)
class(int_data_ts)
decomposedRes <- decompose(int_data_ts)
plot (decomposedRes)



new_data$int_log<-log(new_data$df_interpolation)
df2_seasdiff_int <- diff(new_data$int_log, lag=frequency(new_data$int_log), differences=1)  # seasonal differencing
plot(df2_seasdiff, type="l", main="Seasonally Differenced")  # still not stationary!
decomposedRes <- decompose(df2_seasdiff_int)
plot (decomposedRes)

View(df2_seasdiff)
summary(df2_seasdiff_int)

acf(df2_seasdiff_int)
pacf(df2_seasdiff_int)

adf.test(df2_seasdiff_int)
kpss.test(df2_seasdiff_int)

ggplot(data = new_data,aes(x= date,y= df_interpolation)) + 
  geom_line(colour='red') +
  geom_line(colour='blue',data = new_data,aes(x= date,y= pm25))+
  theme(panel.background = element_blank())+
  ylim(0,700)+
  ggtitle("Interpolation Values")

######  Kalman #########

#df_kalman<-na_kalman(new_data)
#plot(df_kalman,type = "o")

new_data$df_kalman <-na_seasplit(new_data$pm25,algorithm = 'kalman',find_frequency = TRUE)

ggplot(data = new_data,aes(x= date,y= df_kalman)) + 
  geom_line(colour='red') +
  geom_line(colour='blue',data = new_data,aes(x= date,y= pm25))+
  theme(panel.background = element_blank())+
  ylim(0,700)+
  ggtitle("kalman Values")

##### Locf ##########

#df_locf<-na_locf(new_data)
#plot(df_locf, type = "o")

new_data$df_locf <-na_seasplit(new_data$pm25,algorithm = 'locf',find_frequency = TRUE)

ggplot(data = new_data,aes(x= date,y= df_locf)) + 
  geom_line(colour='red') +
  geom_line(colour='blue',data = new_data,aes(x= date,y= pm25))+
  theme(panel.background = element_blank())+
  ylim(0,700)+
  ggtitle("locf values")

df_ma<-na_ma(new_data)
plot(df_ma)

new_data$df_ma <-na_seasplit(new_data$pm25,algorithm = 'ma',find_frequency = TRUE)
ma_data<-new_data[,c(1,5)]
View(ma_data)
ma_data_ts<-ts(ma_data$df_ma,frequency = 24,start = 2018-01-01)
class(ma_data_ts)
decomposedRes <- decompose(ma_data_ts)
plot (decomposedRes)


ma_data$ma_diff<-diff(ma_data$df_ma)
nsdiffs(ma_data_ts)  # number for seasonal differencing needed
#> 1
ma_data$ma_log<-log(ma_data$df_ma)
df2_seasdiff <- diff(ma_data$ma_log, lag=frequency(ma_data$ma_log), differences=1)  # seasonal differencing
summary(df2_seasdiff)
plot(df2_seasdiff, type="l", main="Seasonally Differenced")  # still not stationary!
decomposedRes <- decompose(df2_seasdiff)
plot (decomposedRes)

View(df2_seasdiff)

acf(df2_seasdiff) ## q order - 18
pacf(df2_seasdiff) ## p order -0

fit<-arima(ma_data$ma_log,c(0,3,18))
pred<-predict(fit,n.head = 24*7)
summary(fit)

pred1<-2.718^pred$pred
summary()


adf.test(df2_seasdiff)
kpss.test(df2_seasdiff)
Box.test(df2_seasdiff,lag=1,type = 'Ljung-Box')


new_data$log_ma<-diff(new_data$df_ma)
acf(new_data$log_ma)
ggplot(data = new_data,aes(x= date,y= df_ma)) + 
  geom_line(colour='red') +
  geom_line(colour='blue',data = new_data,aes(x= date,y= pm25))+
  theme(panel.background = element_blank())+
  ylim(0,700)+
  ggtitle("Moving average values")

###### Mean #############

#df_mean<-na_mean(new_data)
#plot(df_mean, type = "o")

new_data$df_mean <-na_seasplit(new_data$pm25,algorithm = 'mean',find_frequency = TRUE)


ggplot(data = new_data,aes(x= date,y= df_mean)) + 
  geom_line(colour='red') +
  geom_line(colour='blue',data = new_data,aes(x= date,y= pm25))+
  theme(panel.background = element_blank())+
  ylim(0,700)+
  ggtitle("Mean values")

###### Random ######

#df_random<-na_random(new_data)
#plot(df_random, type = "o")
new_data$df_random <-na_seasplit(new_data$pm25,algorithm = 'random',find_frequency = TRUE)
ggplot(data = new_data,aes(x= date,y= df_random)) + 
  geom_line(colour='red') +
  geom_line(colour='blue',data = new_data,aes(x= date,y= pm25))+
  theme(panel.background = element_blank())+
  ylim(0,700)+
  ggtitle("random values")

### Stationary check
new_data_ts<-as.ts(new_data[,2:5],frequency = 1,start=c(423))

delhi_data<-as.data.frame(new_data)

delhi_data_ts<-as.ts(new_data[,2:5],frequency = 24,start=c(423))

new_data$df_ma<-ts(new_data$df_ma,frequency = 24,start = 2018-01-01)
library(forecast)
model <-auto.arima(df2_seasdiff) 
summary(model$x)
plot(x=model$fitted,y=model$x)
attributes(model)
model$coef

library(forecast)
f <- forecast(model,n.ahead=168)
library(ggplot2)
autoplot(f)
accuracy(f)


class(new_data)

library(tseries)
adf.test(new_data$df_interpolation) # p-value < 0.05 indicates the TS is stationary
kpss.test(new_data$df_interpolation)

#Ljung Box Test
Box.test(new_data$df_interpolation,lag=1,type = 'Ljung-Box')

acf <- acf(new_data$df_ma) # autocorrelation
pacf <- pacf(new_data$df_ma)  # partial autocorrelation

model_1<-arima(ma, order = c(18,2,32))
model_1


