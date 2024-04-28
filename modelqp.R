library(fpp2) #it contain austa dataset 
library(ggplot2)
library(forecast)
library(fma)
library(expsmooth)


#Q1)

data(austa)
length(austa)

#a)
#Use auto.arima() to find an appropriate ARIMA model.
autoplot(austa)
#What model was selected
fita<-auto.arima(austa,seasonal = FALSE)
fita
#Check that the residuals look like white noise.
tsdisplay(residuals(fita), lag.max = 45, main = "(ARIMA(0,1,1) model residuals" )
#Plot forecasts for the next 10 periods.
fita %>% forecast(h=10) %>% autoplot(include=80)


#b) 
#Plot forecasts from an ARIMA(0,1,1) model with no drift and compare these to part a. Remove the MA term and plot again. 
#remvoe the drift 
fita$coef
austa_b<-austa-0.1735
fitb<-auto.arima(austa_b,seasonal = FALSE)
fitb %>% forecast(h=10) %>% autoplot(include=80)

#c)
#Plot forecasts from an ARIMA(2,1,3) model with drift. Remove the constant and see what happens. 

fitc<-arima(austa,order = c(2,1,3),method="ML")
fitc
fitc %>% forecast(h=10) %>% autoplot(include=80)

#After remove the constanct( mean ), arima(2,1,3) keeps the same shape and shift down.

#mean is constant
austa3<-austa-mean(austa)
fitc_2<-arima(austa3,order = c(2,1,3),method="ML");fitc_2
fitc_2 %>% forecast(h=10) %>% autoplot(include=80)

#d)
#Plot forecasts from an ARIMA(0,0,1) model with a constant.

fitd<-arima(austa,order = c(0,0,1),method="ML")
fitd
fitd %>% forecast(h=10) %>% autoplot(include=80)

#Remove the MA term and plot again.

fitd_2<-arima(austa,order = c(0,0,0),method="ML")
fitd_2

fitd_2 %>% forecast(h=10) %>% autoplot(include=80)

#e)
#Plot forecasts from an ARIMA(0,2,1) model with no constant

fite<-arima(austa3,order = c(0,2,1),method="ML")
fite
fite %>% forecast(h=10) %>% autoplot(include=80)

#------------------------------------------------------------------------------#

#Q2)
library(fpp)

#a) Plot the data and describe the main features of the series.


cars <- ukcars
plot(ukcars)

# There is a strong seasonal effect throughout the series. The trend was declining until
# the early 80s then it started increasing until 2000. Production recovered in a year or
# two.

#b) Decompose the series using STL and obtain the seasonally adjusted data. 


decomposed <- stl(cars, s.window="periodic", robust=TRUE)
seasonal <- decomposed$time.series[,1]

cars_sa <- cars - seasonal


#c) Forecast the next two years of the series using an additive damped trend method applied
#   to the seasonally adjusted data. Then reseasonalize the forecasts. Record the 
#   parameters of the method and report the RMSE of the one-step forecasts from your method. 


fit1 <- holt(cars_sa, h=8, damped = TRUE)

lastyear <- rep(decomposed$time.series[110:113,"seasonal"],2)
reseasonalized_fc <- fit$mean + lastyear

summary(fit)


# d) Forecast the next two years of the series using Holts linear method applied to the 
#    seasonally adjusted data. Then reseasonalize the forecasts. Record the parameters of
#    the method and report the RMSE of of the one-step forecasts from your method.

fit2 <- holt(cars_sa, h=8)

reseasonalized_fc <- fit$mean + lastyear

summary(fit)

#e) Now use ets() to choose a seasonal model for the data.


fit3 <- ets(cars_sa)



# f) Compare the RMSE of the fitted model with the RMSE of the model you obtained using an 
#    STL decomposition with Holt''s method. Which gives the better in-sample fits? 


## ETS Model
m1 <- ets(ukcars)
round(forecast::accuracy(m1),2)

## STL Decomposition Model
ukcarsSTL <- stl(ukcars, s.window="periodic")
ukcarsSTLTS <- ts(ukcarsSTL)
round(forecast::accuracy(ukcarsSTLTS),2)


#g) Compare the forecasts from the two approaches? Which seems most reasonable? 


predict(ets(cars_sa))

## ETS Model 2 Forecast
m2 <- stlf(ukcars, etsmodel="AAN", damped=FALSE, h=8)
forecast(m2, h=8)

## SSTL Decomposition Model Forecast
forecast(ukcarsSTL, h=8)

# Holt's linear method appears to be the most reasonable as it shows some trend.

## h. Check the residuals of your preferred model.
mForecast1 <- forecast(m2, h=8)
mForecast1$residuals
summary(mForecast1$residuals)

