#Stationarity and Non-stationarity - datacamp
#Stationary Time Series:ARMA
"syntax arima.sim(model, n, ...)"

#Generating and plo!ing MA(1)

x <- arima.sim(list(order = c(0, 0, 1), ma = 0.9), n = 100)
plot(x)

#Generating and plo!ing AR(2)

x <- arima.sim(list(order = c(2, 0, 0), ar = c(0, -0.9)), n = 100)
 plot(x)
 
#AR and MA Models
 
x <- arima.sim(list(order = c(1, 0, 0), ar = -.7), n = 200)
y <- arima.sim(list(order = c(0, 0, 1), ma = -.7), n = 200)
par(mfrow = c(1, 2))
plot(x, main = "AR(1)")
plot(y, main = "MA(1)")

#Estimation with astsa- AR(2) with mean 50:
x <- arima.sim(list(order = c(2, 0, 0),
                      ar = c(1.5, -.75)),
                 n = 200) + 50
x_fit <- sarima(x, p = 2, d = 0, q = 0)
x_fit$ttable

#Estimation with astsa- AR(2) with mean o:
y <- arima.sim(list(order = c(0, 0, 1), ma = -.7), n = 200)
y_fit <- sarima(y, p = 0, d = 0, q = 1)
y_fit$ttable

#AR and MA Together: ARMA

x <- arima.sim(list(order = c(1, 0, 1),
                      ar = .9,
                      ma = -.4),
                 n = 200)
plot(x, main = "ARMA(1, 1)")

#Estimation
x <- arima.sim(list(order = c(1, 0, 1),
                      ar = .9,
                      ma = -.4),
                 n = 200) + 50
x_fit <- sarima(x, p = 1, d = 0, q = 1)
x_fit$ttable

#Model Choice and Residual Analysis
#Model Choice: AR(1) vs. MA(2)

gnpgr <- diff(log(gnp))
sarima(gnpgr, p = 1, d = 0, q = 0)
sarima(gnpgr, p = 0, d = 0, q = 2)

#ARIMA - Integrated ARMA
#Identifying ARIMA

# Simulation ARIMA(p = 1, d = 1, q = 0)
x <- arima.sim(list(order = c(1, 1, 0), ar = .9), n = 200)
plot(x, main = "ARIMA(p = 1, d = 1, q = 0)")
plot(diff(x), main = "ARMA(p = 1, d = 0, q = 0)")

#ACF and PCF of an Integrated ARMA
x <- arima.sim(list(order = c(1, 1, 0), ar = .9), n = 200)
acf2(x)

#ACF and PCF of a Differenced ARIMA
x <- arima.sim(list(order = c(1, 1, 0), ar = .9), n = 200)
acf2(diff(x))

#-----------------------Chapter 3. ARIMA Model (Shumway and Stooffer, 2016)-------------#

#Autoregressive Moving Average Models
par(mfrow=c(2,1))
plot(arima.sim(list(order=c(1,0,0), ar=.9), n=100), ylab="x",
     main=(expression(AR(1)~~~phi==+.9)))
plot(arima.sim(list(order=c(1,0,0), ar=-.9), n=100), ylab="x",
     main=(expression(AR(1)~~~phi==-.9)))

#calculate the roots of the polynomial and solve for arg in R
z = c(1,-1.5,.75) # coefficients of the polynomial
(a = polyroot(z)[1]) # print one root = 1 + i/sqrt(3)
[1] 1+0.57735i
arg = Arg(a)/(2*pi) # arg in cycles/pt
1/arg

set.seed(8675309)
ar2 = arima.sim(list(order=c(2,0,0), ar=c(1.5,-.75)), n = 144)
plot(ar2, axes=FALSE, xlab="Time")
axis(2); axis(1, at=seq(0,144,by=12)); box()
abline(v=seq(0,144,by=12), lty=2)

#calculate and display the ACF

ACF = ARMAacf(ar=c(1.5,-.75), ma=0, 50)
plot(ACF, type="h", xlab="lag")
abline(h=0)




