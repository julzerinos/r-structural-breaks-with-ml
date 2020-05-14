library("ggplot2")
library("zoo")
# install.packages('forecast', dependencies = TRUE)
library(forecast)
library(ggplot2)

library(readxl)
pwt91 <- read_excel("sample_data/pwt91.xlsx", sheet = "Data")

greece <- subset(pwt91,country=="Egypt",select = c("year","rgdpe"))
# remove rows with NA
greece <- greece[complete.cases(greece),]

# plot gdp 
# ggplot(greece, aes(x = year, y = rgdpe)) + geom_line()

#start_year <-greece$year[1]
greece <- ts(greece$rgdpe,start = greece$year[1], frequency=1)
#greece <- Nile
start_year = time(greece)[1]


plot(greece)
# cusum and mosum for residuals (value - mean) 
#plot cumsum
CUSUM = cumsum(greece - mean(greece))
CUSUM <- ts(CUSUM,start =start_year)
plot(CUSUM, type ="l")
abline(a=0,b=0, col="green")

  
# plot moving sum
MOSUM = rollapply(greece - mean(greece), 15, sum)
plot(MOSUM)
abline(a=0,b=0, col="green")

# plot rec-cumsum (mean of all observations prior to given time)
rec_cusum <- numeric(length(greece))
for (i in 1:length(greece)){
  rec_cusum[i] = mean(greece[1:(i-1)])
}
rec_cusum <- ts(rec_cusum,start = start_year,frequency = 1)

plot(greece)
lines(rec_cusum,col="red")

# plot cumulated recursive residuals
model <- lm(greece ~ 1)
plot(greece)
abline(model)
# one step ahead prediction errors
rec_residuals <- numeric(length(greece))
for (i in 2:length(greece)){
  rec_residuals[i-1] = predict.lm(model,greece[i])
}
  # calculate residual sum of squares
  
  rss =  sapply(5:(length(greece)-5), function(i) {
     before <- 1:i
     after <- (i+1):length(greece)
    res <- c(Nile[before] - mean(Nile[before]), Nile[after] - mean(Nile[after]))
     sum(res^2)
     })

  plot(start_year + 5:(length(greece)-5), rss, type = "b", xlab = "Time", ylab = "RSS")
  
  rss <- ts(rss,start=start_year-1+5,frequency=1)
  #breakpoint <- min(rss)
  breakpoint <- time(rss)[which.min(rss)]
  # local minimma
 # which(diff(sign(diff(rss)))==+2)+1
  # combine timeseries with lag
  
  
  
  plot(greece)
  abline(v=breakpoint, col="red",lty=2)
  lines(ts(predict(lm((greece[1:(breakpoint-start_year+1)]) ~ 1)),start=start_year,freq=1),col='darkgreen',lwd=2)
  lines(ts(predict(lm((greece[(breakpoint-start_year+1):length(greece)]) ~ 1)),start=breakpoint,freq=1),col='darkgreen',lwd=2)








#https://robjhyndman.com/hyndsight/piecewise-linear-trends/
x1 <- 1:length(y)
fit <- auto.arima(y, xreg=x1)
library(fpp)
T <- length(greece)
x1 <- seq(T)
fit <- auto.arima(greece, xreg=x1)
fc <- forecast(fit, xreg=T+seq(10))
b0 <- coef(fit)["intercept"]
b1 <- coef(fit)["x1"]
t <- seq(T+10)
trend <- ts(b0 + b1*t, start=start(greece))

plot(fc, main="Linear trend with AR(1) errors")
lines(trend, col='red')

tau = 20
fit <- auto.arima(y, xreg=cbind(x1, pmax(0,x1-tau)))

x2 <- pmax(0, x1-30)
x3 <- pmax(0, x1-32)
fit <- auto.arima(greece, xreg=cbind(x1,x2,x3))
fc <- forecast(fit,xreg=cbind(max(x1)+seq(10), max(x2)+seq(10), max(x3)+seq(10)))
b0 <- coef(fit)["intercept"]
b1 <- coef(fit)["x1"]
b2 <- coef(fit)["x2"]
b3 <- coef(fit)["x3"]
trend <- ts(b0 + b1*t + b2*pmax(0,t-30) + b3*pmax(0,t-32),
            start=start(greece))

plot(fc, main="Piecewise linear trend with AR(1) errors")
lines(trend, col='red')
