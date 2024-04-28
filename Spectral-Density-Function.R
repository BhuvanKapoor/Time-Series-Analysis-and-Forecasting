## Experiment 7
## Spectral Density Function

library(zoo)
meas <- read.table("https://web.stanford.edu/class/earthsys214/data/nycmeas.dat.txt", header = F)
meas

dimnames(meas)[[2]] <- c("Date","Cases")
meas1 <- zoo(meas$Cases, order.by = meas$Date)
plot(meas1, xlab = "Date", ylab = "Cases")

kernel('modified.daniell',c(2,2))
mspect <- spectrum(meas$Cases, log = "no", spans = c(2,2), plot = F)
delta <- 1/12
specx <- mspect$freq/delta
specy <- 2*mspect$spec
plot(specx, specy, xlab = 'Periods (Yrs)', ylab = "Spectral Density", type = "l")


## Dengue

dengue <- read.csv("https://web.stanford.edu/class/earthsys214/data/San_Juan_Training_Data.csv", header = T)
tcases <- zoo(dengue$total_cases, as.Date(dengue$week_start_date))
plot(tcases, xlab = "Date", ylab = "Total Cases")

acases <- zoo(dengue[,4:7], as.Date(dengue$week_start_date))
plot(acases, xlab = "Date", ylab = list("Dengue 1","Dengue 2", "Dengue 3", "Dengue 4"), main = "")

# Power Spectrum
dspect <- spectrum(dengue$total_cases, log = "no", spans = c(5,5), plot = F)
delta <- 7/365
specx <- dspect$freq/delta
specy <- 2*dspect$spec

plot(specx[1:100], specy[1:100], xlab = "Period (Yrs)", ylab = "Spectral Density", type = "l")
