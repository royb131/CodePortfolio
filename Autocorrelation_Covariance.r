#Scatterplot

#Stock Prices: Stock A and B over time

ts.plot(cbind(stock_A, stock_B))

plot(stock_A, stock_B)

#Log returns for Stock A and B

stock_A_logreturn = diff(log(stock_A))
stock_B_logreturn = diff(log(stock_B))
ts.plot(cbind(stock_A_logreturn, stock_B_logreturn))
plot(stock_A_logreturn, stock_B_logreturn)
mean(stock_A)
sd(stock_A)
#Covariance of Stock A and B
cov(stock_A, stock_B)
cov(stock_A, stock_B) / (sd(stock_A) * sd(stock_B))

#Covariance and Correlation: log returns

cov(stock_A_logreturn, stock_B_logreturn)
cor(stock_A_logreturn, stock_B_logreturn)

#Autocorrelation - I

# Lag 1 Autocorrelation:
# Correlation of Stock A “today” and stock A “yesterday”
cor(stock_A[-100],stock_A[-1])

#Autocorrelation - II
# Lag 2 Autocorrelation:
# Correlation of Stock A “today” and stock A “Two Days Earlier”
cor(stock_A[-(99:100)],stock_A[-(1:2)])

#Autocorrelations at lag 1 and 2 - I
cor(stock_A[-100],stock_A[-1])
cor(stock_A[-(99:100)],stock_A[-(1:2)])
acf(stock_A, lag.max = 2, plot = FALSE)

#The Autocorrelation Function - I
# Autocorrelation by lag: “The Autocorrelation Function” (ACF)
acf(stock_A, plot = FALSE)
acf(stock_A, plot = TRUE)

www <- "http://www.massey.ac.nz/~pscowper/ts/Herald.dat"
Herald.dat <- read.table(www, header = T)
attach (Herald.dat)

#calculate the covariance for the Herald Square pairs
x <- CO; y <- Benzoa; n <- length(x)
sum((x - mean(x))*(y - mean(y))) / (n - 1)
mean((x - mean(x)) * (y - mean(y)))
cov(x, y)
cov(x,y) / (sd(x)*sd(y))
cor(x,y)

www <- "http://www.massey.ac.nz/~pscowper/ts/wave.dat"
wave.dat <- read.table (www, header=T) ; attach(wave.dat)plot(ts(waveht)) ; plot(ts(waveht[1:60]))
acf(waveht)$acf[2]
acf(waveht, type = c("covariance"))$acf[2]

#Airpassenger

data(AirPassengers)
AP <- AirPassengers
AP.decom <- decompose(AP, "multiplicative")
plot(ts(AP.decom$random[7:138]))
acf(AP.decom$random[7:138])
sd(AP[7:138])
sd(AP[7:138] - AP.decom$trend[7:138])
sd(AP.decom$random[7:138])

#Example based on the Font Reservoir series
www <- "http://www.massey.ac.nz/~pscowper/ts/Fontdsdt.dat"
Fontdsdt.dat <- read.table(www, header=T)
 attach(Fontdsdt.dat)
plot(ts(adflow), ylab = 'adflow')
acf(adflow, xlab = 'lag (months)', main="")