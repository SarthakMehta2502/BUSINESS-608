#Homework 3

install.packages("fpp") #installing the forecast package with data sets
library(fpp) #loading the required package

# choosing a Time Series

usconsumption #choice of data set
mydf <- data.frame(usconsumption) #converting the dataset/time series into a data frame
mydf #printing my new data frame
data("usconsumption")

#Description of the Time Series

#The time series I chose is the US consumption which gives us the percent change in quarterly personal consumption 
#expenditure and personal disposable income for the US from 1970 to 2010. From the US consumption TS I chose the income data 
#to forecast.
min(usconsumption[,"income"])
max(usconsumption[,"income"])

plot(usconsumption) #plotting usconsumption to understand it better and choose which column to use
plot(usconsumption[,"income"])    #plotting income separately to get a better view

#I chose Income column from the TS

cwindow <- window(usconsumption[,"income"], start = 1977)
plot(cwindow)

#choosing the income column from the data set from 1977 to 2010 for forecasting and future purposes as there was a huge spike 
#in the income around the year 1975 which I would like to disregard so that it does not hamper my forecast

fit <- stl(cwindow, s.window=5) #using the decomposition method stl to describe the time series
plot(fit)
fit

# Plotting the Time Series

plot(cwindow)   #the value of the seasonal change is somewhat constant hence we can say that these components are additive
                #the plot has a mixture of cyclicl movement and seasonal pattern

#ACF Plot

Acf(cwindow) #since all the values that we got from the Acf plot are within the blue zone (threshold areas) it means that 
             #none of the values are dependent on the previous value hence they are independent.

#forecasting using Holt-Winters and checking the accuracy

tmp <- HoltWinters(cwindow)
attributes(tmp)
#$names
#[1] "fitted"       "x"            "alpha"        "beta"         "gamma"        "coefficients" "seasonal"     "SSE"  "call"        

#$class
#[1] "HoltWinters"


plot(tmp)

tmp_f <- forecast(tmp)
attributes(tmp_f)
#$names
# [1] "method"    "model"     "level"     "mean"      "lower"     "upper"     "x"         "series"    "fitted"    "residuals"
#$class
#[1] "forecast"

plot(tmp_f)

plot(tmp_f$residuals)

hist(tmp_f$residuals)

Acf(tmp_f$residuals)

accuracy(tmp_f)

#                   ME     RMSE       MAE       MPE     MAPE      MASE       ACF1
#Training set 0.07083846 0.9383384 0.7201747 -41.16379 206.858 0.7791495 -0.095718
