# Exponential Smoothing Technique (Single, Double and Triple)

# Simple Exponential Smoothing
library(forecast)
y <- c(71,70,69,68,64,65,72,78,75,75,75,70)
y_timeseries <- ts(y, start = c(1,1), frequency = 1) 
y_timeseries
fit1 <- ets(y_timeseries, model = "ANN", alpha = 0.1)
A1 <- fit1$fitted
fit2 <- ets(y_timeseries, model = "ANN", alpha = 0.3)
A2 <- fit2$fitted
fit3 <- ets(y_timeseries, model = "ANN", alpha = 0.5)
A3 <- fit3$fitted
fit4 <- ets(y_timeseries, model = "ANN", alpha = 0.7)
A4 <- fit4$fitted
fit5 <- ets(y_timeseries, model = "ANN", alpha = 0.9)
A5 <- fit5$fitted

plot(y_timeseries, col = "red")
lines(A1, lty = 1)
lines(A2, lty = 2)
lines(A3, lty = 3)
lines(A4, lty = 4)
lines(A5, lty = 5)
legend("topleft", c("Raw Data", "alpha = 0.1", "alpha = 0.3", "alpha = 0.5", "alpha = 0.7", "alpha = 0.9"), lty = c(1,1,2,3,4,5), col = c('red','black','black','black','black','black'))


# Double Exponential Smoothing
s <- c(7,6,5,4,8,9,10,11,10,7)
s_timeseries <- ts(s, start = c(1,1), frequency = 1)
fit <- ets(s_timeseries, model = "AAN")
pred <- forecast(fit, 4)
plot(fit)
plot(pred)


# Triple exponential Smoothing
fit <- ets(AirPassengers, model = "AAA")
fit
pred <- forecast(fit, 5)
pred
plot(fit)
plot(pred)
