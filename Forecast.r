# Forecasting DATACAMP

#Fi!ed values and residuals

#Example: oil production

fc <- naive(oil)
autoplot(oil, series = "Data") + xlab("Year") +
  autolayer(fitted(fc), series = "Fitted") +
  ggtitle("Oil production in Saudi Arabia")

autoplot(residuals(fc))

#checkresiduals()

checkresiduals(fc)

#Example: Saudi Arabian oil production
training <- window(oil, end = 2003)
test <- window(oil, start = 2004)
fc <- naive(training, h = 10)
autoplot(fc) +
  autolayer(test, series = "Test data")

#The accuracy() command
accuracy(fc, test)

#Variance stabilization

autoplot(usmelec) +
  xlab("Year") + ylab("") +
  ggtitle("US monthly net electricity generation")

autoplot(usmelec^0.5) +
  xlab("Year") + ylab("") +
  ggtitle("Square root electricity generation")

autoplot(usmelec^0.33333) +
  xlab("Year") + ylab("") +
  ggtitle("Cube root electricity generation")

autoplot(log(usmelec)) +
  xlab("Year") + ylab("") +
  ggtitle("Log electricity generation")

autoplot(-1/usmelec) +
  xlab("Year") + ylab("") +
  ggtitle("Inverse electricity generation")

#Box-Cox transformations

BoxCox.lambda(usmelec)

#Back-transformation

usmelec %>%
  ets(lambda = -0.57) %>%
  forecast(h = 60) %>%
  autoplot()

#ARIMA models

#US net electricity generation

autoplot(usnetelec) +
  xlab("Year") +
  ylab("billion kwh") +
  ggtitle("US net electricity generation")

fit <- auto.arima(usnetelec)
summary(fit)
fit %>% forecast() %>% autoplot()

#Seasonal  ARIMA models
#Monthly retail debit card usage in Iceland


autoplot(debitcards) +
  xlab("Year") + ylab("million ISK") +
  ggtitle("Retail debit card usage in Iceland")

fit <- auto.arima(debitcards, lambda = 0)
fit

fit %>%
  forecast(h = 36) %>%
  autoplot() + xlab("Year")

#------------------------Chapter 6.6 ARMA Models. Empirical Analysis. Cowpertwait and Meltcalfe (2009)----#
#ARMA models: Empirical analysis

set.seed(1)
x <- arima.sim(n = 10000, list(ar = -0.6, ma = 0.5))
coef(arima(x, order = c(1, 0, 1)))

x.ma <- arima(x.ts, order = c(0, 0, 1))
x.ar <- arima(x.ts, order = c(1, 0, 0))
x.arma <- arima(x.ts, order = c(1, 0, 1))
AIC(x.ma)
AIC(x.ar)
AIC(x.arma)
x.arma
acf(resid(x.arma))

#Electricity production series

www <- "http://www.massey.ac.nz/~pscowper/ts/cbe.dat"
CBE <- read.table(www, header = T)
Elec.ts <- ts(CBE[, 3], start = 1958, freq = 12)
Time <- 1:length(Elec.ts)
Imth <- cycle(Elec.ts)
Elec.lm <- lm(log(Elec.ts) ~ Time + I(Time^2) + factor(Imth))
acf(resid(Elec.lm))

best.order <- c(0, 0, 0)
best.aic <- Inf

for (i in 0:2) for (j in 0:2) {
  fit.aic <- AIC(arima(resid(Elec.lm), order = c(i, 0,
                                                 j)))
  if (fit.aic < best.aic) {
    best.order <- c(i, 0, j)
    best.arma <- arima(resid(Elec.lm), order = best.order)
    best.aic <- fit.aic
  }
}

best.order
acf(resid(best.arma))

new.time <- seq(length(Elec.ts), length = 36)
new.data <- data.frame(Time = new.time, Imth = rep(1:12,
                                                     3))
predict.lm <- predict(Elec.lm, new.data)
predict.arma <- predict(best.arma, n.ahead = 36)
elec.pred <- ts(exp(predict.lm + predict.arma$pred), start = 1991,
                freq = 12)
ts.plot(cbind(Elec.ts, elec.pred), lty = 1:2)

#Wave tank data
www <- "http://www.massey.ac.nz/~pscowper/ts/wave.dat"
wave.dat <- read.table(www, header = T)
attach (wave.dat)
layout(1:3)
plot (as.ts(waveht), ylab = 'Wave height')
acf (waveht)
pacf (waveht)
wave.arma <- arima(waveht, order = c(4,0,4))
acf (wave.arma$res[-(1:4)])
pacf (wave.arma$res[-(1:4)])
hist(wave.arma$res[-(1:4)], xlab='height / mm', main='')


