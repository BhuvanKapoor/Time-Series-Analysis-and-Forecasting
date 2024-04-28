# Experiment 1
# Visualization of Time Stationary and Non-Stationary Time Series

library(forecast)

rainfall <- c(799,1174.8,865.1,1334.6,635.4,918.5,685.5,998.6,784.2,985,882.8,1071)
# convert the data into timeseries
rainfall.timeseries <- ts(rainfall, start = c(2012,1),frequency = 12)
print(rainfall.timeseries)

# plotting graph of the time series
plot.ts(rainfall.timeseries)


# Multiple Time Series

rainfall_1 <- c(799,1174.8,865.1,1334.6,635.4,918.5,685.5,998.6,784.2,985,882.8,1071)
rainfall_2 <- c(655,1306.9,1323.4,1172.2,562.2,824,822.4,1265.5,799.6,1105.6,1106.7,1337.8)

# convert to matrix
combined_rainfall <- matrix(c(rainfall_1,rainfall_2),nrow = 12)

# Converting to time series 
combined_rainfall_timeseries <- ts(combined_rainfall, start = c(2012,1), frequency = 12) 
print(combined_rainfall_timeseries)

plot.ts(combined_rainfall_timeseries)


# AIR PASSENGERS
data("AirPassengers")
class(AirPassengers)
start(AirPassengers)
end(AirPassengers)

# Finding missing values
sum(is.na(AirPassengers))

summary(AirPassengers)

plot.ts(AirPassengers)

# Decomposing data into 4 components
ts_data <- ts(AirPassengers, frequency = 12)
d_data <- decompose(ts_data,"multiplicative")
d_data
plot(d_data)
plot.ts(d_data$trend)
plot.ts(d_data$seasonal)
plot.ts(d_data$random)

# Plotting trend line 
plot.ts(AirPassengers)
abline(reg = lm(AirPassengers~time(AirPassengers)))

# Creating box plot by cycle
boxplot(AirPassengers~cycle(AirPassengers, xlab = "Date", ylab = "# Passenger (in 1000's", main = "Monthly air passengers from 1949-60"))

#seasonal plot
ggseasonplot(AirPassengers)


# Dataset: NILE

print(Nile)
length(Nile)
head(Nile,10)
tail(Nile, 12)
plot(Nile, xlab = "Year", ylab = "River Volume (1e9 m^(3))")



## Stationary and Random Time Series
esp <- rnorm(100,0,1)
mu <- 2
X_t <- mu + esp
plot.ts(X_t, main = "Example of random stationary time series", ylab = expression(X[t]))
# Auto Covariance Function
acf(X_t, main = "Auto Covariance Function of X")


# Random Process
z <- rnorm(100,0.5,1.5)
X <- 0
for (i in 2:length(z)) {
  X[i] <- X[i-1] +z[i]
}

plot.ts(X, main = "Random walk process")
