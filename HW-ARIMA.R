# Homework - ARIMA

install.packages("rmarkdown")
install.packages("readr")
library(fpp)
library(fpp2)
library(forecast)

cwindow <- window(usconsumption[,"income"], start = 1977)

plot(cwindow)
ndiffs(cwindow)
tsdisplay(cwindow)
cwindwodiff1 <- diff(cwindow, differences=1)
plot(cwindwodiff1)
tsdisplay(cwindwodiff1)
auto_fit <- auto.arima(cwindow, trace=TRUE, stepwise = FALSE)
auto_fit
attributes(auto_fit)
plot(forecast(auto_fit,h=5,level=c(99.5)))

#Residual Analysis

Acf(auto_fit$residuals)
Box.test(residuals(auto_fit), lag=20, type="Ljung")
plot.ts(residuals(auto_fit))
hist(auto_fit$residuals)
tsdiag(auto_fit)

# Seasonal Data

plot(cwindow)
nsdiffs(cwindow)
ndiffs(cwindow)
ndiffs((diff(cwindow,4)))
tsdisplay(diff(diff(cwindow,4)))
fit3 <- auto.arima(cwindow,trace=TRUE, stepwise = FALSE )
fit3

#Residual Analysis

Acf(fit3$residuals)
Box.test(residuals(fit3), lag=20, type="Ljung")
plot.ts(residuals(fit3))
hist(fit3$residuals)
tsdiag(fit3)

