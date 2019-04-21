

#Datacamp - Forecast in R codes- Rob Hyndman
# read tab delimited file

data<- read.csv("cbe.dat",sep="\t", header=TRUE)

# Read the data from Excel into R
mydata <- read_excel("exercise1.xlsx")

# Look at the first few lines of mydata
head(mydata)

# Create a ts object called myts
myts <- ts(mydata[c(2:4)], start = c(1981,1), frequency = 4)

#class(myts)
#class(myts)

# Plot the data with facetting
autoplot(myts, facets = TRUE)

# Plot the data without facetting
autoplot(myts, facets = FALSE)

# Plot the three series
autoplot(gold)
autoplot(woolyrnq)
autoplot(gas)

# Find the outlier in the gold series
#which.max()-which.max() can be used to identify the smallest index of the maximum value
#x <- c(4, 5, 5)
#which.max(x)
#[1] 2

goldoutlier <- which.max(gold)

# Look at the seasonal frequencies of the three series
frequency(gold)
frequency(woolyrnq)
frequency(gas)

A seasonal plot is similar to a time plot except that the data are plotted against the individual “seasons” 
in which the data were observed. You can create one using the ggseasonplot() function the same way you do with autoplot()

# Load the fpp2 package
library(fpp2)

# Create plots of the a10 data.a10 contains monthly sales volumes for anti-diabetic drugs in Australia.
ggseasonplot(a10)
autoplot(a10)

# Produce a polar coordinate season plot for the a10 data.An interesting variant of a season plot uses polar coordinates, where the time axis is circular rather than horizontal; to make one, simply add a polar argument and set it to TRUE
ggseasonplot(a10, polar = TRUE)

# Restrict the ausbeer data to start in 1992
beer <- window(ausbeer, start=1992)

# Make plots of the beer data
ggsubseriesplot(beer)
autoplot(beer)

# Create an autoplot of the oil data
autoplot(oil)

# Create a lag plot of the oil data
gglagplot(oil)
# Create an ACF plot of the oil data
ggAcf(oil)
# Plot the annual sunspot numbers
autoplot(sunspot.year)
ggAcf(sunspot.year)

# Save the lag corresponding to maximum autocorrelation
maxlag_sunspot <- 1
# Plot the traffic on the Hyndsight blog
autoplot(hyndsight)
ggAcf(hyndsight)

# Save the lag corresponding to maximum autocorrelation
maxlag_hyndsight <- 7
# Plot the original series
autoplot(goog)

# Plot the differenced series
autoplot(diff(goog))

# ACF of the differenced series
ggAcf(diff(goog))

# Ljung-Box test of the differenced series
Box.test(diff(goog), lag = 10, type = "Ljung")

# Use naive() to forecast the goog series,h, which specifies the number of values you want to forecast
fcgoog <- naive(goog, h=20)

# Plot and summarize the forecasts
autoplot(fcgoog)
summary(fcgoog)

# Use snaive() to forecast the ausbeer series
fcbeer <- snaive(ausbeer, h = 16 )

# Plot and summarize the forecasts
autoplot(fcbeer)
summary(fcbeer)
# Check the residuals from the naive forecasts applied to the goog series
goog %>% naive(goog) %>% checkresiduals()

# Do they look like white noise (TRUE or FALSE)
googwn <- TRUE

# Check the residuals from the seasonal naive forecasts applied to the ausbeer series
ausbeer %>% snaive(ausbeer) %>% checkresiduals()

# Do they look like white noise (TRUE or FALSE)
beerwn <-FALSE

_____________________________________________________________________________________________\
# Create the training data as train.  comprising the first 1000 observations
train <- subset(gold, end = 1000)

# Compute naive forecasts and save to naive_fc
naive_fc <- naive(train, h =108)

# Compute mean forecasts and save to mean_fc
mean_fc <- meanf(train, h = 108)

# Use accuracy() to compute RMSE statistics
accuracy(naive_fc, gold)
accuracy(mean_fc, gold)

# Assign one of the two forecasts as bestforecasts
bestforecasts <- naive_fc

_______________________________________________________________________________________________\
#Here, you will use the Melbourne quarterly visitor numbers (vn[, "Melbourne"]) to create three different training sets
# Create three training series omitting the last 1, 2, and 3 years
train1 <- window(vn[, "Melbourne"], end = c(2014, 4))
train2 <- window(vn[, "Melbourne"], end = c(2013, 4))
train3 <- window(vn[, "Melbourne"], end = c(2012, 4))
vn
# Produce forecasts using snaive() #To compute one year of forecasts, you need to set h equal to the number of quarters in one year.
fc1 <- snaive(train1, h = 4)
fc2 <-snaive(train2, h = 4)
fc3 <- snaive(train3, h = 4)

# Use accuracy() to compare the MAPE of each series
accuracy(fc1, vn[, "Melbourne"])["Test set", "MAPE"]
accuracy(fc2, vn[, "Melbourne"])["Test set", "MAPE"]
accuracy(fc3, vn[, "Melbourne"])["Test set", "MAPE"]

__________________________________________________________________________________________________\
# Compute cross-validated errors for up to 8 steps ahead
e <- tsCV(goog, forecastfunction = naive, h = 8)

# Compute the MSE values and remove missing values
mse <- colMeans(e^2, na.rm = TRUE)

# Plot the MSE values against the forecast horizon.Plot the resulting MSE values (y) against the forecast horizon (x). Think through your knowledge of functions. If MSE = mse is provided in the list of function arguments, then mse should refer to an object that exists in your workspace outside the function, whereas MSE is the variable that you refers to this object within your function.
data.frame(h = 1:8, MSE = mse) %>%
  ggplot(aes(x = h, y = MSE)) + geom_point()
___________________________________________________________________________________________________\
# Use ses() to forecast the next 10 years of winning times
fc <- ses(marathon, h = 10)

# Use summary() to see the model parameters
summary(fc)

# Use autoplot() to plot the forecasts
autoplot(fc)

# Add the one-step forecasts for the training data to the plot
autoplot(fc) + autolayer(fitted(fc))
_______________________________________________________________________________________________________\
# Create a training set using subset().Using subset.ts(), create a training set for marathon comprising all but the last 20 years of the data which you will reserve for testing.
train <- subset(marathon, end = length(marathon) - 20)

# Compute SES and naive forecasts, save to fcses and fcnaive
fcses <- ses(train, h = 20)
fcnaive <- naive(train, h = 20)

# Calculate forecast accuracy measures.To view the results, use the accuracy() function with the forecast as the first argument and original data (or test set) as the second.
accuracy(fcses, marathon)
accuracy(fcnaive, marathon)

# Save the best forecasts as fcbest
fcbest <- fcnaive

____________________________________________________________\\
forecasting with holt's function

# Produce 10 year forecasts of austa using holt()
fcholt <- holt(austa,h=10)

# Look at fitted model using summary()
summary(fcholt)
# Plot the forecasts
autoplot(fcholt)

# Check that the residuals look like white noise
checkresiduals(fcholt)

______________________________________________________________\\
# Plot the data.a10, the monthly sales of anti-diabetic drugs in Australia from 1991 to 2008
autoplot(a10)

# Produce 3 year forecasts , in months
fc <- hw(a10, seasonal = "multiplicative", h = 36)

# Check if residuals look like white noise
checkresiduals(fc)
whitenoise <- FALSE

# Plot forecasts
autoplot(fc)

__________________________________________________________________\\
# Create training data with subset().hyndsight data//appropriate unit of time for h is in days.
train <- subset(hyndsight, end = length(hyndsight) - 28)

# Holt-Winters additive forecasts as fchw
fchw <- hw(train, seasonal = "additive", h = 28)

# Seasonal naive forecasts as fcsn
fcsn <- snaive(train,h=28)

# Find better forecasts with accuracy()
accuracy(fchw, hyndsight)
accuracy(fcsn, hyndsight)

# Plot the better forecasts
autoplot(fchw)
_____________________________________________________________________\\
State space models for exponential smoothing
# Fit ETS model to austa in fitaus.Using ets(), fit an ETS model to austa and save this to fitaus
fitaus <- ets(austa)

# Check residuals
checkresiduals(fitaus)

# Plot forecasts
autoplot(forecast(fitaus))

# Repeat for hyndsight data in fiths
fiths <- ets(hyndsight)
checkresiduals(fiths)
autoplot(forecast(fiths))

# Which model(s) fails test? (TRUE or FALSE)
fitausfail <- FALSE
fithsfail <- TRUE


_____________________________________________________________________\\# Function to return ETS forecasts.A function to return ETS forecasts, fets(), has been written for you.
fets <- function(y, h) {
forecast(ets(y), h = h)
}

# Apply tsCV() for both methods.Apply tsCV() for both ETS and seasonal naive methods to the cement data for a forecast horizon of 4. 
Use the newly created fets and the existing snaive functions as your forecast function argument for e1 and e2, respectively.
e1 <- tsCV(cement, fets, h = 4)
e2 <- tsCV(cement, snaive, h = 4)

# Compute MSE of resulting errors (watch out for missing values)
mean(e1^2, na.rm=TRUE)
mean(e2^2, na.rm=TRUE)

# Copy the best forecast MSE
bestmse <- mean(e2^2, na.rm=TRUE)

_____________________________________________________________________________________________\

# Plot the lynx series
autoplot(lynx)

# Use ets() to model the lynx series
fit <- ets(lynx)

# Use summary() to look at model and parameters
summary(fit)

# Plot 20-year forecasts of the lynx series.Plot forecasts for the next 20 years using the pipe operator. Note that you only need to specify h for one particular function.
lynx %>% ets() %>% forecast(h=20) %>% autoplot()

____________________________________________________________\\
# Plot the series
autoplot(a10)

# Try four values of lambda in Box-Cox transformations
a10 %>% BoxCox(lambda =0.0 ) %>% autoplot()
a10 %>% BoxCox(lambda =0.1 ) %>% autoplot()
a10 %>% BoxCox(lambda =0.2 ) %>% autoplot()
a10 %>% BoxCox(lambda =0.3 ) %>% autoplot()
# Compare with BoxCox.lambda()
BoxCox.lambda(a10)
____________________________________________________________\\
# Plot the US female murder rate.Plot the wmurders data and observe how it has changed over time.
autoplot(wmurders)

# Plot the differenced murder rate using diff() function.
autoplot(diff(wmurders))

# Plot the ACF of the differenced murder rate
ggAcf(diff(wmurders))

____________________________________________________________\\
Seasonal differencing for stationarity
# Plot the data
autoplot(h02)

# Take logs and seasonal differences of h02
difflogh02 <- diff(log(h02), lag = 12)

# Plot difflogh02
autoplot(difflogh02)

# Take another difference and plot
ddifflogh02 <- diff(difflogh02)
autoplot(ddifflogh02)

# Plot ACF of ddifflogh02
ggAcf(ddifflogh02)

____________________________________________________________\\
ARIMA
# Fit an automatic ARIMA model to the austa series
fit <- auto.arima(austa)

# Check that the residuals look like white noise
checkresiduals(fit)
residualsok <- TRUE

# Summarize the model
summary(fit)

# Find the AICc value and the number of differences used.The ARIMA model specification shown in the summary contains the number of differences used. d is the second integer.
AICc <- -14.46
d <-1

# Plot forecasts of fit
fit %>% forecast(h = 10) %>% autoplot()

____________________________________________________________\\
Forecasting with ARIMA models
# Plot forecasts from an ARIMA(0,1,1) model with no drift
austa %>% Arima(order = c(0, 1, 1), include.constant = FALSE) %>% forecast() %>% autoplot()

# Plot forecasts from an ARIMA(2,1,3) model with drift
austa %>% Arima(order = c(2, 1, 3), include.constant = TRUE) %>% forecast() %>% autoplot()

# Plot forecasts from an ARIMA(0,0,1) model with a constant
austa %>% Arima(order = c(0, 0, 1), include.constant = TRUE) %>% forecast() %>% autoplot()

# Plot forecasts from an ARIMA(0,2,1) model with no constant
austa %>% Arima(order = c(0, 2, 1), include.constant = FALSE) %>% forecast() %>% autoplot()

____________________________________________________________\\
Comparing auto.arima() and ets() on non-seasonal data

# Set up forecast functions for ETS and ARIMA models
fets <- function(x, h) {
forecast(ets(x), h = h)
}
farima <- function(x, h) {
forecast(auto.arima(x), h=h)
}

# Compute CV errors for ETS as e1
e1 <- tsCV(austa, fets, h=1)

# Compute CV errors for ARIMA as e2
e2 <- tsCV(austa, farima, h=1)

# Find MSE of each model class
mean(e1^2, na.rm=TRUE)
mean(e2^2, na.rm=TRUE)

# Plot 10-year forecasts using the best model class
austa %>% farima(h=10) %>% autoplot()

____________________________________________________________\\
Automatic ARIMA models for seasonal time series

# Check that the logged h02 data have stable variance
h02 %>% log() %>% autoplot()

# Fit a seasonal ARIMA model to h02 with lambda = 0
fit <- auto.arima(h02,lambda=0)

# Summarize the fitted model
summary(fit)
# Record the amount of lag-1 differencing and seasonal differencing used.(p,d,q)(P,D,Q)[m]:
d <- 1
D <-1

# Plot 2-year forecasts/ h02 has month;y sales data
fit %>% forecast(h=24) %>% autoplot()
____________________________________________________________\\
Exploring auto.arima() options

# Find an ARIMA model for euretail
fit1 <- auto.arima(euretail)
summary(fit1)
#AICc=70.38
# Don't use a stepwise search
fit2 <- auto.arima(euretail,stepwise = FALSE)
summary(fit2)
#AICc=68.39
# AICc of better model. lower is better
AICc <-68.39

# Compute 2-year forecasts from better model.euretail data is quarterly.
fit2 %>% forecast(h=8) %>% autoplot()

____________________________________________________________\\
Comparing auto.arima() and ets() on seasonal data

# Use 20 years of the qcement data beginning in 1988
train <- window(qcement, start = 1988, end = c(2007,4))

# Fit an ARIMA and an ETS model to the training data
fit1 <- auto.arima(train)
fit2 <- ets(train)

# Check that both models have white noise residuals
checkresiduals(fit1)
checkresiduals(fit2)

# Produce forecasts for each model
fc1 <- forecast(fit1, h = 25)
fc2 <- forecast(fit2, h = 25)

# Use accuracy() to find better model based on RMSE. lower better
accuracy(fc1, qcement)
accuracy(fc2, qcement)
bettermodel <- fit2

____________________________________________________________\\
# Time plot of both variables
autoplot(advert, sales,facets=TRUE)

# Fit ARIMA model
fit <- auto.arima(advert[,"sales"], xreg = advert[,"advert"], stationary = TRUE)
fit
# Check model. Increase in sales for each unit increase in advertising
salesincrease <- coefficients(fit)[3]

# Forecast fit as fc
fc <- forecast(fit, xreg = rep(10,6))
fc
# Plot fc with x and y labels
autoplot(fc) + xlab("Month") + ylab("Sales")

____________________________________________________________\\
Forecasting electricity demand
# Time plots of demand and temperatures
autoplot(elec[, c("Demand", "Temperature")], facets = TRUE)

# Matrix of regressors
xreg <- cbind(MaxTemp = elec[, "Temperature"], 
              MaxTempSq = MaxTemp^2, 
              Workday =elec[, "Workday"] )
xreg
# Fit model
fit <- auto.arima(elec[,"Demand"], xreg = xreg)

# Forecast fit one day ahead
forecast(fit, xreg = cbind(20, 20^2, 1))

____________________________________________________________\\
# Set up harmonic regressors of order 13.Set up an xreg matrix called harmonics using the fourier() method on gasoline with order K=13 which has been chosen to minimize the AICc.
harmonics <- fourier(gasoline, K = 13)

# Fit regression model with ARIMA errors
fit <- auto.arima(gasoline, xreg = harmonics, seasonal = FALSE)

# Forecasts next 3 years in weeks
newharmonics <- fourier(gasoline, K = 13, h = 156)
fc <- forecast(fit, xreg = newharmonics)

# Plot forecasts fc
autoplot(fc)

