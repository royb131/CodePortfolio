install.packages("devtools")
devtools::install_github("nickpoison/astsa")
library(astsa)
data(chicken)
head(chicken)
summary(fit <- lm(chicken~time(chicken), na.action=NULL))
summary(aov(fit <- lm(chicken~time(chicken), na.action=NULL)))
AIC(lm(chicken~time(chicken), na.action=NULL))
BIC(lm(chicken~time(chicken), na.action=NULL))

plot(chicken, ylab="cents per pound")
abline(fit)

data(unemp)
head(unemp)
summary(fit <- lm(unemp~time(unemp), na.action=NULL))
summary(aov(fit <- lm(unemp~time(unemp), na.action=NULL)))
AIC(lm(unemp~time(unemp), na.action=NULL))
BIC(lm(unemp~time(unemp), na.action=NULL))

plot(unemp)
abline(fit)

summary(fit) # regression results
summary(aov(fit)) # ANOVA table (compare to next line)

#Reduce the model by removing one independent variable, starting from the last one:
#cmort~ trend + temp + temp2 + part  becomes cmort~ trend + temp + temp2

#Load econ5 data from astsa

data(econ5, package="astsa")
?econ5
View(econ5)
econ5e<-  econ5[1:3]
View(econ5e)
plot(econ5e)

#pair columns in RA
unemp<- econ5$unemp
gnp<-econ5$gnp
consum<-econ5$consum
econdata<- c(unemp,gnp,consum)
View(econdata)
class(econ5e)
time_series <- ts(econ5, start=c(1948, 3), end=c(1988, 2), frequency=4 )

#create 4 variables

trend = time(time_series)
gnp <- time_series[,2]
consum <- time_series[,3]
response <- time_series[,1]

fit = lm(response~ trend+gnp+consum, na.action=NULL)
summary(fit)

# create a linear model in R
library(astsa)
data(econ5)
View(econ5)
class(econ5)
head(econ5)
#drop columns from dara frame
df <- econ5[ -c(3,5) ]
head(econ5)
time_series <- ts(econ5)

trend = time(time_series)
gnp <- time_series[,2]
govinv<- time_series[,4]
unemp<- time_series[,1]

fit = lm(unemp~ trend+govinv+gnp, na.action=NULL)
summary(fit)

#Use pairs to plot unemployment, government investment and gnp.
pairs(cbind(unemp,govinv,gnp))

#Estimating a Linear Trend
summary(fit <- lm(chicken~time(chicken), na.action=NULL))
plot(chicken, ylab="cents per pound")
abline(fit)

par(mfrow=c(3,1)) # plot the data
plot(cmort, main="Cardiovascular Mortality", xlab="", ylab="")
plot(tempr, main="Temperature", xlab="", ylab="")
plot(part, main="Particulates", xlab="", ylab="")
dev.new() # open a new graphic device
ts.plot(cmort,tempr,part, col=1:3) # all on same plot (not shown)
dev.new()
pairs(cbind(Mortality=cmort, Temperature=tempr, Particulates=part))
temp = tempr-mean(tempr) # center temperature
temp2 = temp^2
trend = time(cmort) # time
fit = lm(cmort~ trend + temp + temp2 + part, na.action=NULL)
summary(fit) # regression results
summary(aov(fit)) # ANOVA table (compare to next line)
summary(aov(lm(cmort~cbind(trend, temp, temp2, part)))) # Table 2.1
num = length(cmort) # sample size
AIC(fit)/num - log(2*pi) # AIC
BIC(fit)/num - log(2*pi) # BIC
(AICc = log(sum(resid(fit)^2)/num) + (num+5)/(num-5-2)) # AICc