# Lab5: Auto Regressive Integrated Moving Average (ARIMA) for Non-Stationary Time Series
library(forecast)

plot(BJsales, main = "Graph without Forecasting")

# Fitting model using ARIMA
fit <- auto.arima(BJsales)
fit$coef
# Forecasting next 10 values
forecasted_values <- forecast(fit,10)
forecasted_values
plot(forecasted_values, main = "Graph with Forecasted Values")


# 

plot(EuStockMarkets[,"DAX"], main = "Graph without Forecasting")

market_fit <- auto.arima(EuStockMarkets[,"DAX"])
forecasted_values_market <- forecast(market_fit, 10)
market_fit$coef
# forecasted_values_market <- forecast(market_fit, 10)
plot(forecasted_values_market, main = "Grah with Forecasted Values")
