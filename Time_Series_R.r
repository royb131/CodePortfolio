# Time Series from BMW_Data
install.packages("evir")
library(evir)
data(bmw)
print(bmw)
plot(bmw)
# Print the Nile dataset
print(Nile)
plot(Nile)
# List the number of observations in the Nile dataset
length(Nile)
# Display the first 10 elements of the Nile dataset
head(Nile,n=10)
# Display the last 12 elements of the Nile dataset
tail(Nile, n=12)
# Plot the Nile data
plot(Nile)
# Plot the Nile data with xlab and ylab arguments
plot(Nile, xlab = "Year", ylab = "River Volume (1e9 m^{3})")
# Plot the Nile data with xlab, ylab, main, and type arguments
# Plot the continuous_series using continuous time indexing
par(mfrow=c(2,1))
plot(continuous_time_index,continuous_series, type = "b")

# Make a discrete time index using 1:20 
discrete_time_index <-c(1:20)

# Now plot the continuous_series using discrete time indexing
plot(discrete_time_index,continuous_series,type = "b")

# Plot AirPassengers
plot(AirPassengers)

# View the start and end dates of AirPassengers

start(AirPassengers)
end(AirPassengers)
# Use time(), deltat(), frequency(), and cycle() with AirPassengers 
#deltat() function returns the fixed time interval between observations and the frequency() function returns the number of observations per unit time. Finally, the cycle() function returns the position in the cycle of each observation
time(AirPassengers)
deltat(AirPassengers)
frequency(AirPassengers)
cycle(AirPassengers)

#Missing values
# Plot the AirPassengers data
plot(AirPassengers)

# Compute the mean of AirPassengers
mean(AirPassengers,na.rm=TRUE)

# Impute mean values to NA in AirPassengers
AirPassengers[85:96] <- mean(AirPassengers, na.rm = TRUE)

# Generate another plot of AirPassengers
plot(AirPassengers)

# Add the complete AirPassengers data to your plot
rm(AirPassengers)
points(AirPassengers, type = "l", col = 2, lty = 3)

#The function ts() can be applied to create time series objects. A time series object is a vector (univariate) or matrix (multivariate) with additional attributes, including time indices for each observation, the sampling frequency and time increment between observations, and the cycle length for periodic data. Such objects are of the ts class, and represent data that has been observed at (approximately) equally spaced time points. Now you will create time series objects yourself.
#The advantage of creating and working with time series objects of the ts class is that many methods are available for utilizing time series attributes, such as time index information. For example, as you've seen in earlier exercises, calling plot() on a ts object will automatically generate a plot over time.
#In this exercise, you'll familiarize yourself with the ts class by encoding some time series data (saved as data_vector) into ts and exploring the result. Your time series data_vector starts in the year 2004 and has 4 observations per year (i.e. it is quarterly data).

# Use print() and plot() to view data_vector
print(data_vector)
plot(data_vector)

# Convert data_vector to a ts object with start = 2004 and frequency = 4
time_series <- ts(data_vector,start=2004,frequency=4)

# Use print() and plot() to view time_series
print(time_series)
plot(time_series)

# Check whether data_vector and time_series are ts objects
is.ts(data_vector)
is.ts(time_series)

# Check whether Nile is a ts object
is.ts(Nile)

# Check whether AirPassengers is a ts object
is.ts(AirPassengers)
# Check whether eu_stocks is a ts object
is.ts(eu_stocks)

# View the start, end, and frequency of eu_stocks
start(eu_stocks)
end(eu_stocks)
frequency(eu_stocks)

# Generate a simple plot of eu_stocks
plot(eu_stocks)

# Use ts.plot with eu_stocks
ts.plot(eu_stocks, col = 1:4, xlab = "Year", ylab = "Index Value", main = "Major European Stock Indices, 1991-1998")

# Add a legend to your ts.plot
legend("topleft", colnames(eu_stocks), lty = 1, col = 1:4, bty = "n")

#Airpassenger

data(AirPassengers)
AP <- AirPassengers
AP
class(AP)
#start and end of data component
start(AP); end(AP); frequency(AP)

#plot Airpassenger

plot(AP, ylab = "Passengers (1000's)")

#layout function,which takes as input a vector (or matrix) for the location of each plot
#in the display window. The resulting boxplot and annual series
layout(1:2)
plot(aggregate(AP))
boxplot(AP ~ cycle(AP))

#unemployment Marine data 

www <- "http://www.massey.ac.nz/~pscowper/ts/Maine.dat"
Maine.month <- read.table(www, header = TRUE)
attach(Maine.month)
class(Maine.month)

Maine.month.ts <- ts(unemploy, start = c(1996, 1), freq = 12)
#The average (mean) over the twelve months of each year is another example of aggregated data, but this
#time we divide by 12 to give a mean annual rate
Maine.annual.ts <- aggregate(Maine.month.ts)/12

layout(1:2)
plot(Maine.month.ts, ylab = "unemployed (%)")
plot(Maine.annual.ts, ylab = "unemployed (%)")


Maine.Feb <- window(Maine.month.ts, start = c(1996,2), freq = TRUE)
Maine.Aug <- window(Maine.month.ts, start = c(1996,8), freq = TRUE)
Feb.ratio <- mean(Maine.Feb) / mean(Maine.month.ts)
Aug.ratio <- mean(Maine.Aug) / mean(Maine.month.ts)


www <- "http://www.massey.ac.nz/~pscowper/ts/USunemp.dat"
US.month <- read.table(www, header = T)
attach(US.month)
US.month.ts <- ts(USun, start=c(1996,1), end=c(2006,10), freq = 12)
plot(US.month.ts, ylab = "unemployed (%)")

#Multiple time series

www <- "http://www.massey.ac.nz/~pscowper/ts/cbe.dat"
CBE <- read.table(www, header = T)
class(CBE)

#create time series objects for the electricity, beer, and chocolate data

Elec.ts <- ts(CBE[, 3], start = 1958, freq = 12)
Beer.ts <- ts(CBE[, 2], start = 1958, freq = 12)
Choc.ts <- ts(CBE[, 1], start = 1958, freq = 12)
plot(cbind(Elec.ts, Beer.ts, Choc.ts))

AP.elec <- ts.intersect(AP, Elec.ts)
start(AP.elec)
end(AP.elec)
AP.elec[1:3, ]
AP <- AP.elec[,1]; Elec <- AP.elec[,2]

layout(1:2)
plot(AP, main = "", ylab = "Air passengers / 1000's")
plot(Elec, main = "", ylab = "Electricity production / MkWh")
plot(as.vector(AP), as.vector(Elec),
     xlab = "Air passengers / 1000's",
     ylab = "Electricity production / MWh")
abline(reg = lm(Elec ~ AP))

#corelation
cor(AP, Elec)

#Quarterly exchange rate: GBP to NZ dollar
www <- "http://www.massey.ac.nz/~pscowper/ts/pounds_nz.dat"
Z <- read.table(www, header = T)
Z[1:4, ]

Z.ts <- ts(Z, st = 1991, fr = 4)
plot(Z.ts, xlab = "time / years",
     ylab = "Quarterly exchange rate in $NZ / pound")



