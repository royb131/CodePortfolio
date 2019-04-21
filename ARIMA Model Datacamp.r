
#----------------ARIMA MODEL-------------Datacamp#
# View a detailed description of AirPassengers
help(AirPassengers)

# Plot AirPassengers
plot(AirPassengers)

# Plot the DJIA daily closings

plot(djia$Close)
# Plot the Southern Oscillation Index
plot(soi)

#_________________________________________\\
# astsa and xts are preloaded 

# Plot GNP series (gnp) and its growth rate
par(mfrow = c(2,1))
plot(diff(gnp))
plot(log(djia$Close))

# Plot DJIA closings (djia$Close) and its returns
par(mfrow = c(2,1))
plot(djia$Close)

plot(diff(djia$Close))
plot(log(djia$Close))

#_________________________________________\\
Simulating ARMA Models
As we saw in the video, any stationary time series can be written as a linear combination of white noise. In addition, any ARMA model
has this form, so it is a good choice for modeling stationary time series.

R provides a simple function called arima.sim() to generate data from an ARMA model. For example, the syntax for generating 100 
observations from an MA(1) with parameter .9 is arima.sim(model = list(order = c(0, 0, 1), ma = .9 ), n = 100). 
You can also use order = c(0, 0, 0) to generate white noise.

# Generate and plot white noise
WN <- arima.sim(model = list(order = c(0, 0, 0)), n = 200)
plot(WN)
# Generate and plot an MA(1) with parameter .9 
MA <- arima.sim(model = list(order = c(0, 0, 1), ma = .9 ), n = 200)
plot(MA)

# Generate and plot an AR(2) with parameters 1.5 and -.75
AR <- arima.sim(model = list(order = c(2, 0, 0), ar = c(1.5,-.75)), n = 200)
plot(AR)
#_________________________________________\\

Throughout this course, you will be using sarima() from the astsa package to easily fit models to data. 
The command produces a residual diagnostic graphic that can be ignored until diagnostics is discussed later in the chapter.

# Generate 100 observations from the AR(1) model
x <- arima.sim(model = list(order = c(1, 0, 0), ar = .9), n = 100) 

# Plot the generated data 
plot(x)
# Plot the sample P/ACF pair
acf2(x)

# Fit an AR(1) to the data and examine the t-table.Use sarima() from astsa to fit an AR(1) to the previously generated data. 
Examine the t-table and compare the estimates to the true values. For example, if the time series is in x, to fit an AR(1) to the data, 
use sarima(x, p = 1, d = 0, q = 0) or simply sarima(x, 1, 0, 0).

sarima(x, p = 1, d = 0, q = 0) 

#_________________________________________\\
# astsa is preloaded.he package astsa is preloaded. x contains the 200 AR(2) observations.
x <- arima.sim(model = list(order = c(2, 0, 0), ar = c(1.5, -.75)), n = 200)
x
# Plot x
plot(x)
# Plot the sample P/ACF of x
acf2(x)

# Fit an AR(2) to the data and examine the t-table
sarima(x, p = 2, d = 0, q = 0) 
#_________________________________________\\
Fitting an MA(1) Model
x <- arima.sim(model = list(order = c(0, 0, 1), ma = -.8), n = 100). Look at the simulated data and the sample ACF and 
PACF to determine the order based on the table given in the first exercise. Then fit the model.

Recall that for pure MA(q) models, the theoretical ACF will cut off at lag q while the PACF will tail off.

#_________________________________________\\
# astsa is preloaded.The package astsa is preloaded. 100 MA(1) observations are available in your workspace as x
x <- arima.sim(model = list(order = c(0, 0, 1), ma = -.8), n = 100)
# Plot x
plot(x)

# Plot the sample P/ACF of x
acf2(x)

# Fit an MA(1) to the data and examine the t-table

sarima(x, p = 0, d = 0, q = 1) 

#_________________________________________\\
Fitting an ARMA model
# astsa is preloaded.The package astsa is preloaded. 250 ARMA(2,1) observations are in x.
x <- arima.sim(model = list(order = c(2, 0, 1), ar = c(1, -.9), ma = .8), n = 250)
# Plot x
plot(x)

# Plot the sample P/ACF of x
acf2(x)

# Fit an ARMA(2,1) to the data and examine the t-table

sarima(x, p = 2, d = 0, q = 1) 

#_________________________________________\\
Model Choice - I

Based on the sample P/ACF pair of the logged and differenced varve data (dl_varve), an MA(1) was indicated. The best approach to fitting ARMA is to start with a low order model, and then try to add a parameter at a time to see if the results change.

In this exercise, you will fit various models to the dl_varve data and note the AIC and BIC for each model. In the next exercise, you will use these AICs and BICs to choose a model. Remember that you want to retain the model with the smallest AIC and/or BIC value.

A note before you start:
  
  sarima(x, p = 0, d = 0, q = 1) and sarima(x, 0, 0, 1) are the same.

#The package astsa is preloaded. The varve series has been logged and differenced as dl_varve <- diff(log(varve)).

# Fit an MA(1) to dl_varve.   
sarima(dl_varve, p = 0, d = 0, q = 1) 

# Fit an MA(2) to dl_varve. Improvement?
sarima(dl_varve, p = 0, d = 0, q = 2) 

# Fit an ARMA(1,1) to dl_varve. Improvement?

sarima(dl_varve, p = 1, d = 0, q = 1)

#_________________________________________\\
Residual Analysis - I
As you saw in the video, an sarima() run includes a residual analysis graphic. Specifically, the output shows (1) the standardized residuals, (2) the sample ACF of the residuals, (3) a normal Q-Q plot, and (4) the p-values corresponding to the Box-Ljung-Pierce Q-statistic.

In each run, check the four residual plots as follows:
  
  The standardized residuals should behave as a white noise sequence with mean zero and variance one. Examine the residual plot for departures from this behavior.
The sample ACF of the residuals should look like that of white noise. Examine the ACF for departures from this behavior.
Normality is an essential assumption when fitting ARMA models. Examine the Q-Q plot for departures from normality and to identify outliers.
Use the Q-statistic plot to help test for departures from whiteness of the residuals.
As in the previous exercise, dl_varve <- diff(log(varve)), which is plotted below a plot of varve. The astsa package is preloaded.

# Fit an MA(1) to dl_varve. Examine the residuals  
sarima(dl_varve, p = 0, d = 0, q = 1) 

# Fit an ARMA(1,1) to dl_varve. Examine the residuals

sarima(dl_varve, p = 1, d = 0, q = 1) 

#_________________________________________\\
ARMA get in
By now you have gained considerable experience fitting ARMA models to data, but before you start celebrating, try one more exercise (sort of) on your own.
The data in oil are crude oil, WTI spot price FOB (in dollars per barrel), weekly data from 2000 to 2008. Use your skills to fit an ARMA model to the returns. The weekly crude oil prices (oil) are plotted for you. Throughout the exercise, work with the returns, which you will calculate.
As before, the astsa package is preloaded for you. The data are preloaded as oil and plotted on the right.

# Calculate approximate oil returns.Calculate the approximate crude oil price returns using diff() and log(). Put the returns in oil_returns.
oil_returns <-diff(log(oil))

# Plot oil_returns. Notice the outliers.
plot(oil_returns)

# Plot the P/ACF pair for oil_returns
acf2(oil_returns)

# Assuming both P/ACF are tailing, fit a model to oil_returns.From the P/ACF pair, it is apparent that the correlations are small and the returns are nearly noise. But it could be that both the ACF and PACF are tailing off. If this is the case, then an ARMA(1,1) is suggested. Fit this model to the oil returns using sarima(). Does the model fit well? Can you see the outliers in the residual plot?

sarima(oil_returns, p = 1, d = 0, q = 1)
#_________________________________________\\
ARIMA - Plug and Play

# Plot x
plot(x)

# Plot the P/ACF pair of x
acf2(x)
# Plot the differenced data
plot(diff(x))

# Plot the P/ACF pair of the differenced data
plot(diff(acf2(x)))

#_________________________________________\\
Simulated ARIMA
# Plot sample P/ACF of differenced data and determine model

plot(acf2(diff(x)))

# Estimate parameters and examine output

sarima(x, p = 2, d = 1, q = 0) 

#_________________________________________\\
Global Warming
Now that you have some experience fitting an ARIMA model to simulated data, your next task is to apply your skills to some real world data.

The data in globtemp (from astsa) are the annual global temperature deviations to 2015. In this exercise, you will use established techniques to fit an ARIMA model to the data. A plot of the data shows random walk behavior,
which suggests you should work with the differenced data. The differenced data diff(globtemp) are also plotted.

# Plot the sample P/ACF pair of the differenced data 
plot(acf2(diff(globtemp)))

# Fit an ARIMA(1,1,1) model to globtemp
sarima(globtemp, p = 1, d = 1, q = 1)

# Fit an ARIMA(0,1,2) model to globtemp. Which model is better?
sarima(globtemp, p = 0, d = 1, q = 2)

#_________________________________________\\
Diagnostics - Simulated Overfitting

# Plot sample P/ACF pair of the differenced data
plot(acf2(diff(x)))

# Fit the first model, compare parameters, check diagnostics.Fit an ARIMA(0,1,1) model to the simulated data using sarima(). Compare the MA parameter estimate to the actual value of .9, and examine the residual plots.
sarima(x, p = 0, d = 1, q = 1) 
# Fit the second model and compare fit.Overfit the model by adding an additional MA parameter. That is, fit an ARIMA(0,1,2) to the data and compare it to the ARIMA(0,1,1) run.
sarima(x, p = 0, d = 1, q = 2) 

#_________________________________________\\
Diagnostics - Global Temperatures

You can now finish your analysis of global temperatures. Recall that you previously fit two models to the data in globtemp, an ARIMA(1,1,1) 
and an ARIMA(0,1,2). In the final analysis, check the residual diagnostics and use AIC and BIC for model choice.

# Fit ARIMA(0,1,2) to globtemp and check diagnostics  
sarima(globtemp, p = 0, d = 1, q = 2)

# Fit ARIMA(1,1,1) to globtemp and check diagnostics
sarima(globtemp, p = 1, d = 1, q = 1)

# Which is the better model?
print("ARIMA(0,1,2)")

#_________________________________________\\
Forecasting Simulated ARIMA
# Plot P/ACF pair of differenced data 
plot(acf2(diff(x)))

# Fit model - check t-table and diagnostics

sarima(x, p = 1, d = 1, q = 0) 
# Forecast the data 20 time periods ahead
#Use sarima.for() to forecast the data 20 time periods ahead. Compare it to the actual values.
sarima.for(x, n.ahead = 20, p = 1, d = 1, q = 0) 
lines(y)  

#_________________________________________\\
Forecasting Global Temperatures

Here, you will forecast the annual global temperature deviations globtemp to 2050. Recall that in previous exercises, you fit an ARIMA(0,1,2)
model to the data. You will refit the model to confirm it, and then forecast the series 35 years into the future.
# Fit an ARIMA(0,1,2) to globtemp and check the fit
sarima(globtemp, p = 0, d = 1, q = 2)

# Forecast data 35 years into the future
#Use sarima.for() to forceast your global temperature data 35 years ahead to 2050 using the ARIMA(0,1,2) fit.
sarima.for(globtemp, n.ahead = 35, p = 0, d = 1, q = 2) 
#_________________________________________\\
Fit a Pure Seasonal Model
# Plot sample P/ACF to lag 60 and compare to the true values.Use acf2() to plot the sample ACF and 
PACF of the generated data to lag 60 and compare to actual values. To estimate to lag 60, set the max.lag argument equal to 60.
plot(acf2(x, max.lag = 60))

# Fit the seasonal model to x.Fit the model to generated data using sarima(). In addition to the p, d,
and q arguments in your sarima() command, specify P, D, Q, and S (note that R is case sensitive).
sarima(x, p = 0, d = 0, q = 0, P = 1, D = 0, Q = 1, S = 12)

#_________________________________________\\
Fit a Mixed Seasonal Model

# Plot sample P/ACF pair to lag 60 and compare to actual

plot(acf2(x, max.lag = 60))
# Fit the seasonal model to x
sarima(x,  p = 0, d = 0, q = 1,P=0,
       D = 0, Q = 1, S = 12  ) 
#_________________________________________\\
# Plot unempplot(un)
plot(unemp)
# Difference your data and plot it.Detrend and plot the data. Save this as d_unemp. Notice the seasonal persistence
d_unemp <- diff(unemp)
plot(d_unemp)


# Seasonally difference d_unemp and plot it
dd_unemp <- diff(d_unemp,12)
plot(dd_unemp)
# Plot P/ACF pair of fully differenced data to lag 60
dd_unemp <- diff(diff(unemp), lag = 12)
plot(acf2(dd_unemp, max.lag = 60))
acf2(dd_unemp, max.lag = 60)
# Fit an appropriate model
sarima(unemp, p = 2, d = 1, q = 0,P=0,
       D = 1, Q = 1, S=12) 

#_________________________________________\\

# Plot differenced chicken.Plot the differenced (d = 1) data diff(chicken). Note that the trend is removed and note the seasonal behavior.
plot(diff(chicken))
plot(chicken)
# Plot P/ACF pair of differenced data to lag 60.Plot the sample ACF and PACF of the differenced data to lag 60 (5 years). Notice that an AR(2) seems appropriate but there is a small but significant seasonal component remaining in the detrended data.
plot(acf2(diff(chicken), max.lag = 60))

# Fit ARIMA(2,1,0) to chicken - not so good
sarima(chicken, p = 2, d = 1, q = 0)

# Fit SARIMA(2,1,0,1,0,0,12) to chicken - that works

sarima(chicken, p = 2, d = 1, q = 0, P = 1, D = 0, Q = 0, S = 12)

#_________________________________________\\
# Plot P/ACF to lag 60 of differenced data.astsa birth packags
d_birth <- diff(birth)
plot(acf2(d_birth,max.lag=60))

# Plot P/ACF to lag 60 of seasonal differenced data
dd_birth <- diff(d_birth, lag = 12)
plot(acf2(dd_birth,max.lag=60))

# Fit SARIMA(0,1,1)x(0,1,1)_12. What happens?

sarima(birth, p = 0, d = 1, q = 1, P = 0, D = 1, Q = 1, S = 12)
# Add AR term and conclude.Add an additional AR (nonseasonal, p = 1) parameter to account for additional correlation. Does the model fit well?
sarima(birth, p = 1, d = 1, q = 1, P = 0, D = 1, Q = 1, S = 12)

#_________________________________________\\
# Fit your previous model to unemp and check the diagnostics

sarima(unemp, p = 2, d = 1, q = 0, P = 0, D = 1, Q = 1, S = 12)
# Forecast the data 3 years into the future

sarima.for(unemp,n.ahead=36,
           2,1,0,0,1,1,12)
#_________________________________________\\
# Fit the chicken model again and check diagnostics
sarima(chicken, p = 2, d = 1, q = 0, P = 1, D = 0, Q = 0, S = 12)

# Forecast the chicken data 5 years into the future

sarima.for(chicken,n.ahead=60,
           2,1,0,1,0,0,12)
