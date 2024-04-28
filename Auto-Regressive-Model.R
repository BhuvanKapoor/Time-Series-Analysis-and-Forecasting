# Auto-Regressive Model for Stationary Time-series

set.seed(0)

# AR(1) Model
y <- ts(rnorm(250,0,2))
y1 <- numeric(250)
y1[1] <- y[1]
for(i in 2:250){
  y1[i] <- 0.5*y1[i-1]+y[i]
}
plot.ts(y1)
acf(y1)


# AR(2) Model
y <- ts(rnorm(250,0,2))
y1 <- numeric(250)
y1[1] <- y[1]
y1[2] <- y[2]
for(i in 3:250){
  y1[i] <- 0.7*y1[i-1]+0.3*y1[i-2]+y[i]
}
plot.ts(y1)
acf(y1)
