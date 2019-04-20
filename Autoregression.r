#The Autoregressive Model
#AR Processes: Inflation Rate
data(Mishkin, package = "Ecdat")
inflation <- as.ts(Mishkin[, 1])
ts.plot(inflation) ; acf(inflation)

AR_inflation <- arima(inflation, order = c(1, 0, 0))
print(AR_inflation)

#AR Processes: Fi!ed Values - II

ts.plot(inflation)
AR_inflation_fitted <- inflation - residuals(AR_inflation)
points(AR_inflation_fitted, type = "l", col = "red", lty = 2)

#1-step ahead forecasts
predict(AR_inflation)

#h-step ahead forecasts

predict(AR_inflation, n.ahead = 6)

#xts and zoo objects

# xts = matrix + index
x <- matrix(1:4, ncol = 2, nrow = 2)
x
idx <- as.Date(c("2015-01-01", "2015-02-01"))
idx

# xts = matrix + index
X <- xts(x, order.by = idx)
X

#The xts constructor

xts(x = NULL,
    order.by = index(x),
    frequency = NULL,
    unique = NULL,
    tzone = Sys.getenv("TZ"))
#An xts example
# xts = matrix + index
x <- xts(x, order.by = idx)
x

#coredata() is used to extract the data component
coredata(x, fmt = FALSE)

#index() is used to extract the index a.k.a. times
index(x)

#Converting using as.xts()

#Load data from R datasets
data(sunspots)
class(sunspots)

sunspots_xts <- as.xts(sunspots)
class(sunspots_xts)
head(sunspots_xts)

#Read data into R using built in (or external) functions read.table(), read.csv(),and read.zoo()
#Coerce data to xts using as.xts()
as.xts(read.table("file"))
as.xts(read.zoo("file"))

#Use write.zoo() for external use (i.e. text files)
write.zoo(x, "file")

#Use saveRDS() for R use
saveRDS(x, "file")

#---------------------------------Working with Dates----------------------#'
install.packages("zoo")
install.packages("xts")
#Step 1. The default format is "YYYY/m/d" or “YYYY-m-d"
my_date = as.Date("1970/1/1")

my.date

class(my.date)

as.numeric(my.date)

myDates = c("2013-12-19", "2003-12-20")

as.Date(myDates)

#Step 2 Use the format argument to specify the input format of the date if it is not in the default format
as.Date("1/1/1970", format="%m/%d/%Y")

as.Date("January 1, 1970", format="%B %d, %Y")

as.Date("01JAN70", format="%d%b%y")

# Step 3 Extract information

myYear = format(my.date, "%Y")

myYear

class(myYear)

as.numeric(myYear)

as.numeric(format(my.date, "%Y"))

#Step 4.  Other components of data

weekdays(my.date)

months(my.date)

quarters(my.date)

#Step 5. Manipulating Dates
my.date

my.date + 1

my.date - 1

my.date + 31




