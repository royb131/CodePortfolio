# Time series ARIMA case study

#Plot tractor sales data as time series
data = read.csv('http://ucanalytics.com/blogs/wp-content/uploads/2015/06/Tractor-Sales.csv')
data = ts(data[,2],start = c(2003,1),frequency = 12)
plot(data, xlab='Years', ylab = 'Tractor Sales')

#Step 2: Difference data to make data stationary on mean (remove trend)

plot(diff(data),ylab='Differenced Tractor Sales')

#Step 3: log transform data to make data stationary on variance

plot(log10(data),ylab='Log (Tractor Sales)')

#Step 4: Difference log transform data to make data stationary on both mean and variance

plot(diff(log10(data)),ylab='Differenced Log (Tractor Sales)')

#Step 5: Plot ACF and PACF to identify potential AR and MA model

par(mfrow = c(1,2))
acf(ts(diff(log10(data))),main='ACF Tractor Sales')
pacf(ts(diff(log10(data))),main='PACF Tractor Sales')

#Step 6: Identification of best fit ARIMA model

require(forecast)
ARIMAfit = auto.arima(log10(data), approximation=FALSE,trace=FALSE)
summary(ARIMAfit)

#Step 6: Forecast sales using the best fit ARIMA model

par(mfrow = c(1,1))
pred = predict(ARIMAfit, n.ahead = 36)
pred
plot(data,type='l',xlim=c(2004,2018),ylim=c(1,1600),xlab = 'Year',ylab = 'Tractor Sales')
lines(10^(pred$pred),col='blue')
lines(10^(pred$pred+2*pred$se),col='orange')
lines(10^(pred$pred-2*pred$se),col='orange')

#Step 7: Plot ACF and PACF for residuals of ARIMA model to ensure no more information is left for extraction
par(mfrow=c(1,2))
acf(ts(ARIMAfit$residuals),main='ACF Residual')
pacf(ts(ARIMAfit$residuals),main='PACF Residual')



