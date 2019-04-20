# R useful statistics courses

("sum(x), length(x), mean(x), var(x), sd(x), max(x), min(x), median(x)")

("weighted.mean(x, weight), weightedMedian(x, weight) , IQR(x), mad(x)")

# library(matrixStats)

#Decomposition of series
#function decompose estimates trends and seasonal effects using a moving average method

install.packages("TSA")
library(TSA)
data(electricity)

Elec.ts<- ts(electricity)
plot(decompose(Elec.ts))
Elec.decom <- decompose(Elec.ts, type = "mult")
plot(Elec.decom)
Trend <- Elec.decom$trend
Seasonal <- Elec.decom$seasonal
ts.plot(cbind(Trend, Trend * Seasonal), lty = 1:2)

("read.table reads data into a data frame
  attach makes names of column variables available
  ts produces a time series object
  aggregate creates an aggregated series
  ts.plot produces a time plot for one or more series
  window extracts a subset of a time series
  time extracts the time from a time series object
  ts.intersect creates the intersection of one or more time series
  cycle returns the season for each value in a series
  decompose decomposes a series into the components
  trend, seasonal effect, and residual
  stl decomposes a series using loess smoothing
  summary summarises an R object")

#Loading  Libraries
library(astsa, quietly=TRUE, warn.conflicts=FALSE)
library(ggplot2)
library(knitr)
library(printr)
library(plyr)
library(dplyr)
library(lubridate)
library(gridExtra)
library(reshape2)
library(TTR)

#Reading Time Series

kings <- scan('http://robjhyndman.com/tsdldata/misc/kings.dat', skip=3)
head(kings)

#Converting the data into a time series

kings <- ts(kings)
kings

births <- scan("http://robjhyndman.com/tsdldata/data/nybirths.dat")

births <- ts(births, frequency = 12, start = c(1946, 1))
births

gift <- scan("http://robjhyndman.com/tsdldata/data/fancy.dat")
gift<- ts(gift, frequency=12, start=c(1987,1))
gift

#Plotting Time Series

plot.ts(kings)
plot.ts(births)
plot.ts(gift)

#transform the time series so that we can model the data with a classic additive model.
logGift <- log(gift)
plot.ts(logGift)

#Decomposing non-Seasonal Data
kingsSMA3 <- SMA(kings, n=3)
plot.ts(kingsSMA3)

#Decomposing Seasonal Data

birthsComp <- decompose(births)

birthsComp
plot(birthsComp)

#Seasonally Adjusting
birthsSeasonAdj <- births - birthsComp$seasonal
plot.ts(birthsSeasonAdj)


