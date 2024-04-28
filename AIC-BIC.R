# Experiment : AIC, BIC, AICC

## AIC
library(AICcmodavg)

data <- mtcars
model1 <- lm(mpg~(disp+hp+wt+qsec), data = data)
model2 <- lm(mpg~(disp+qsec), data = data)
model3 <- lm(mpg~(disp+wt), data = data)


models <- list(model1, model2, model3)
mod_names <- c("disp+hp+wt+qsec", "disp+qsec", "disp+wt")

aictab(cand.set = models, modnames = mod_names)


## BIC
library(flexmix)
data <- mtcars
model1 <- lm(mpg~(disp+hp), data = data)
model2 <- lm(mpg~(disp+qsec), data = data)
model3 <- lm(mpg~(disp+wt), data = data)

BIC(model1)
BIC(model2)
BIC(model3)


## Question
library(forecast)
library(flexmix)
# Get data point in form of vectors
rain <- c(987,1025,978,774,1563,569,1456,789,1479,566,1563,1698)
# Convert to TS
rain_ts <- ts(rain, start = c(2020,1), frequency = 12)
print(rain_ts)
plot(rain_ts)
model1 <- arima(rain_ts, order = c(1,0,0))
model1
model2 <- arima(rain_ts, order = c(2,0,0))
model2

BIC(model1)
BIC(model2)


## BOX - TEST
data <- AirPassengers
class(data)
data
sum(is.na(AirPassengers))
plot(AirPassengers)
mymodel <- auto.arima(AirPassengers)
mymodel
plot(mymodel$residuals)

#Forecast values for next years
forecasted <- forecast(mymodel, level = 95, h = 3*12)
plot(forecasted)

# Validate the model by selecting lag values via Ljung-Box test
Box.test(mymodel$residuals, lag = 5, type = "Ljung-Box")
Box.test(mymodel$residuals, lag = 10, type = "Ljung-Box")
Box.test(mymodel$residuals, lag = 15, type = "Ljung-Box")
