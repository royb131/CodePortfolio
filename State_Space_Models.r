#Identifying temporal trends in data
library(jsonlite)
library(ggplot2)
library(scales)
library(dplyr)
library(xts)
library(tseries)
library(forecast)
library(lubridate)
library(epitools)
json_file <- 'https://datahub.io/core/global-temp/datapackage.json'
json_data <- fromJSON(paste(readLines(json_file), collapse=""))
print(json_data$resources$name)
str(monthly_global_temp)
ts.plot(globtemp, globtempl, col=c(6,4), ylab='Temperature Deviations')

# generate data
set.seed(1); num = 50
w = rnorm(num+1,0,1); v = rnorm(num,0,1)
mu = cumsum(w) # state: mu[0], mu[1],..., mu[50]
y = mu[-1] + v # obs: y[1],..., y[50]
# filter and smooth (Ksmooth0 does both)
ks = Ksmooth0(num, y, A=1, mu0=0, Sigma0=1, Phi=1, cQ=1, cR=1)
# start figure
par(mfrow=c(3,1)); Time = 1:num
plot(Time, mu[-1], main='Predict', ylim=c(-5,10))
lines(ks$xp)
lines(ks$xp+2*sqrt(ks$Pp), lty=2, col=4)
lines(ks$xp-2*sqrt(ks$Pp), lty=2, col=4)
plot(Time, mu[-1], main='Filter', ylim=c(-5,10))
lines(ks$xf)
lines(ks$xf+2*sqrt(ks$Pf), lty=2, col=4)
lines(ks$xf-2*sqrt(ks$Pf), lty=2, col=4)
plot(Time, mu[-1], main='Smooth', ylim=c(-5,10))
lines(ks$xs)
lines(ks$xs+2*sqrt(ks$Ps), lty=2, col=4)
lines(ks$xs-2*sqrt(ks$Ps), lty=2, col=4)
mu[1]; ks$x0n; sqrt(ks$P0n) # initial

library(nlme) # loads package nlme
# Generate data (same as Example 6.6)
set.seed(999); num = 100
x = arima.sim(n=num+1, list(ar = .8), sd=1)
y = ts(x[-1] + rnorm(num,0,1))
# Initial Estimates (same as Example 6.6)
u = ts.intersect(y, lag(y,-1), lag(y,-2))
varu = var(u); coru = cor(u)
phi = coru[1,3]/coru[1,2]
q = (1-phi^2)*varu[1,2]/phi
r = varu[1,1] - q/(1-phi^2)
(em = EM0(num, y, A=1, mu0=0, Sigma0=2.8, Phi=phi, cQ=sqrt(q), cR=sqrt(r),
          max.iter=75, tol=.00001))
# Standard Errors (this uses nlme)
phi = em$Phi; cq = sqrt(em$Q); cr = sqrt(em$R)
mu0 = em$mu0; Sigma0 = em$Sigma0
para = c(phi, cq, cr)
Linn = function(para){ # to evaluate likelihood at estimates
  kf = Kfilter0(num, y, 1, mu0, Sigma0, para[1], para[2], para[3])
  return(kf$like) }
emhess = fdHess(para, function(para) Linn(para))
SE = sqrt(diag(solve(emhess$Hessian)))
# Display Summary of Estimation
estimate = c(para, em$mu0, em$Sigma0); SE = c(SE, NA, NA)
u = cbind(estimate, SE)
rownames(u) = c('phi','sigw','sigv','mu0','Sigma0'); u

#Chapter 6-6.1 with examples and 6.4 with 6.9 example (Shumway and Stoffer)
library(sspir)
set.seed(1)
x1 <- 1:30
x1 <- x1/10 + 2
a <- c(rep(4,15), rep(5,15))
b <- c(rep(2,15), rep(-1,15))
n <- length(x1)
y1 <- a + b * x1 + rnorm(n)
x0 <- rep(1, n)
xx <- cbind(x0, x1)
x.mat <- matrix(xx, nrow = n, ncol = 2)
y.mat <- matrix(y1, nrow = n, ncol = 1)
m1 <- SS(y = y.mat, x = x.mat,
           Fmat = function(tt,x,phi)
             return( matrix(c(x[tt,1], x[tt,2]), nrow = 2, ncol = 1)),
           Gmat = function(tt,x,phi) return (diag(2)),
           Wmat = function(tt, x, phi) return (0.1*diag(2)),
           Vmat = function(tt,x,phi) return (matrix(1)),
           m0 = matrix(c(5,3),nrow=1,ncol=2),C0=10*diag(2)
)
m1.f <- kfilter(m1)
par(mfcol=c(2,1))
plot(m1.f$m[,1], type='l')
plot(m1.f$m[,2], type='l')

#Chapter 12.1-12.2; 12.4-12.5 (Cowpertwait and Metcalfe)

install.packages("sspir")
library(sspir)
set.seed(1)
Plummet.dat <- 20 + 2*rnorm(20) + c(rep(0,10), rep(-10,10))
n <- length(Plummet.dat)
Plummet.mat <- matrix(Plummet.dat, nrow = n, ncol = 1)
m1 <- SS(y = Plummet.mat,
           Fmat = function(tt,x,phi) return( matrix(1) ),
           Gmat = function(tt,x,phi) return( matrix(1) ),
           Wmat = function(tt,x,phi) return( matrix(0.1)),
           Vmat = function(tt,x,phi) return( matrix(2) ),
           m0 = matrix(25), C0 = matrix(10)
)
plot(m1$y, ylab = "Closing price", main = "Simulated")
m1.f <- kfilter(m1)m1.s <- smoother(m1.f)
lines(m1.f$m, lty = 2)
lines(m1.s$m, lty = 3)

plot(m1$y, ylab = "Closing price", main = "Simulated")
m1.f <- kfilter(m1)
lines(m1.f$m, lty = 2)
m2 <- m1
Wmat(m2) <- function(tt, x, phi) {
  if (tt == 12) return(matrix(10)) else return(matrix(0.1))
}
m2.f <- kfilter(m2) lines(m2.f$m,lty=4)

library(sspir)
set.seed(1)
x1 <- 1:30
x1 <- x1/10 + 2
a <- c(rep(4,15), rep(5,15))
b <- c(rep(2,15), rep(-1,15))
n <- length(x1)
y1 <- a + b * x1 + rnorm(n)
x0 <- rep(1, n)
xx <- cbind(x0, x1)
x.mat <- matrix(xx, nrow = n, ncol = 2)
y.mat <- matrix(y1, nrow = n, ncol = 1)
m1 <- SS(y = y.mat, x = x.mat,
         Fmat = function(tt,x,phi)
           return( matrix(c(x[tt,1], x[tt,2]), nrow = 2, ncol = 1)),
         Gmat = function(tt,x,phi) return (diag(2)),
         Wmat = function(tt, x, phi) return (0.1*diag(2)),
         Vmat = function(tt,x,phi) return (matrix(1)),
         m0 = matrix(c(5,3),nrow=1,ncol=2),C0=10*diag(2)
)
m1.f <- kfilter(m1)
par(mfcol=c(2,1))
plot(m1.f$m[,1], type='l')
plot(m1.f$m[,2], type='l')
        

library(sspir)
www <- 'http://www.massey.ac.nz/~pscowper/ts/Murray.txt'
Salt.dat <- read.table(www, header=T) ; attach(Salt.dat)
n <- 81 ; Time <- 1:n
SIN <- sin(2 * pi * Time /12)[-1]
COS <- cos(2 * pi * Time /12)[-1]
Chowilla <- Chowilla - mean(Chowilla)
Flow <- Flow - mean(Flow)
Chow <- Chowilla[-1]
Chow.L1 <- Chowilla[-n]
Flo <- Flow[-1]
Flo.L1 <- Flow[-n]
Sal.mat <- matrix(c(Chow, Flo), nrow = 80, ncol = 2)
x0 <- rep(1, (n-1))
xx <- cbind(x0, Chow.L1, Flo.L1, COS, SIN)
x.mat <- matrix(xx, nrow = n-1, ncol = 5)
G.mat <- diag(10)
W.mat <- diag(rep(c(10, 0.0001, 0.0001, 0.0001, 0.0001), 2))
m1 <- SS(y = Sal.mat, x = x.mat,
         Fmat =
           function(tt, x, phi) return (matrix(
             c(x[tt,1], x[tt,2], x[tt,3], x[tt,4], x[tt,5], rep(0,10),
               x[tt,1], x[tt,2], x[tt,3], x[tt,4], x[tt,5]),
             nrow=10,ncol=2)),
         Gmat = function(tt, x, phi) return (G.mat),
         Wmat = function(tt, x, phi) return (W.mat),
         Vmat = function(tt, x, phi) return
         (matrix(c(839, -348, -348, 1612), nrow=2, ncol=2)),
         m0=matrix(c(0,0.9,0.1,-15,-10,0,0,0.7,30,20),nrow=1,ncol=10),
         C0 = 100 * W.mat
)
m1.f <- kfilter (m1)
par(mfcol=c(2,3))
plot(m1.f$m[,1], type='l')
plot(m1.f$m[,2], type='l')
plot(m1.f$m[,3], type='l')
plot(m1.f$m[,6], type='l')
plot(m1.f$m[,7], type='l')
plot(m1.f$m[,8], type='l')

par(mfcol=c(2,2))
plot(m1.f$m[,4], type='l')
plot(m1.f$m[,5], type='l')
242 12 State Space Models
plot(m1.f$m[,9], type='l')
plot(m1.f$m[,10], type='l')
        