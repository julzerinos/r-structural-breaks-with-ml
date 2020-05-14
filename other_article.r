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