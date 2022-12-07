# Final

library(readr)
library(fpp)
library(fpp2)

IPG3113N <- read_csv("/Users/sarthakmehta/Desktop/IPG3113N.csv")

candy_ts <- ts(IPG3113N$IPG3113N,frequency = 12,start=c(2012,10))

#Plot and Inference
candy_ts
plot(candy_ts) #plotting the candy_ts data

#Observation: Through the graph we can that the monthly production of candies in the US has been pretty cyclical means that we 
#can see a clear pattern to it. And there are sharp declines and rises throughout the data, so to cut out a window to forecast 
#would not be correct. Hence would be going on with the full data set provided.

#Central Tendency

min(candy_ts)
# [1] 77.0192

max(candy_ts)
# [1] 130.2894

mean(candy_ts)
# [1] 99.44454

median(candy_ts)
# [1] 99.8525

quantile(candy_ts)
#   25%      75%    
# 92.0523  106.4516  

boxplot((candy_ts))

#Summary: Well the summaries show us that the amongst the median values calculated min value of candy production in the US
#period of 2012 to 2022 was 77.0192 and the max value was 130.2894. Furthermore, the mean value was 99.44454$ and 
#the median value was 99.8525. The 1st quartile was 92.0523 and the 3rd quartile was 106.4516. The box plot tells us that
#that the there are 2 outliers, and the minimum value is a bit more closer to the 1st quartile than the maximum value is to 
#the 3rd quatile,and the median is approximate in between the 1st and 3rd quartile. But when we look closely at the mean and 
#median we can see that as mean is a bit lower than the median (2nd quartile line) hence would me that data is negatively skewed.


#Decomposition

stl_decomp <- stl(candy_ts,s.window =12)
stl_decomp
plot(stl_decomp) #Decomposition Plot
#Yes the time series is seasonal, and it is additive seasonal not multiplicative as the magnitude is constant. Initially from 
#the month of October - December had pretty high seasonal values and later we also see that this trend is continuing and the 
#months of October - December have really high seasonal values. I believe the reason for it is that around that time of the year 
#we have the holidays season meaning thanksgiving, Christmas and new years so naturally the consumers would be needing more 
#candies to celebrate the holidays. As for the rest of the years as expected the seasonal values are negative because no season 
#is like the holiday season and the production during those months is really high therefore in the months following the 
#seasonal values tend to become very low or even negative.

seasadj(stl_decomp)

plot(candy_ts)
lines(seasadj(stl_decomp), col="Orange")
#After taking out the seasonal component the graph still looks the same in the overall direction and trend but the values have
#shrunk dramatically this means that the magnitude of values in the timeseries is affected but the seasonal component. Yes, 
#seasonality has big fluctuations to the values of the timeseries.


#Naive

naive_forecast <- naive(candy_ts,12) #give me the forecast for the next 12 months using Naive forecast method
plot(naive_forecast)

attributes(naive_forecast)
#$names
#[1] "method"    "model"     "lambda"    "x"         "fitted"    "residuals" "series"    "mean"      "level"     "lower"    
#[11] "upper"    

#$class
#[1] "forecast"

plot(naive_forecast$residuals) #the residual plot shows 6 anomalies(not within range from what I can see); 1st one being at the
#start of year 2014 where there is a sharp decline in production same goes for the start of year 2015,2020,2022 and in the 
#beginning months of year 2022 and the 6th one being the sharp increase in the production just at the end of year 2021.

hist(naive_forecast$residuals) #shows a pretty decent normal distribution

Acf(naive_forecast$residuals) #shows most values are out of the threshold region meaning they are dependent on the previous 
#values (correaltion present).

accuracy(naive_forecast)
#                 ME     RMSE      MAE        MPE      MAPE       MASE      ACF1
#Training set 0.2536275 6.305638 4.625101 0.02809525 4.635846   1.05288   0.2207691


ets_forecast <- ets(candy_ts)
plot(candy_ts)

forecast_ets <- forecast(ets_forecast, h=12) #forecasting for the next 12 months
plot(forecast_ets)
forecast_ets

#        Point Forecast    Lo 80    Hi 80    Lo 95    Hi 95
#Nov 2022       129.6266 123.89521 135.3579 120.86121 138.3919
#Dec 2022       131.4585 124.44955 138.4674 120.73925 142.1777
#Jan 2023       120.2379 112.53103 127.9448 108.45126 132.0245
#Feb 2023       119.7004 111.19327 128.2074 106.68990 132.7108
#Mar 2023       117.3170 108.13076 126.5032 103.26787 131.3661
#Apr 2023       112.4141 102.67755 122.1506  97.52333 127.3049
#May 2023       109.4166  99.16483 119.6684  93.73787 125.0953
#Jun 2023       110.4319  99.63670 121.2271  93.92206 126.9418
#Jul 2023       110.2762  98.97651 121.5760  92.99479 127.5577
#Aug 2023       117.4103 105.49177 129.3289  99.18246 135.6382
#Sep 2023       123.6545 111.09743 136.2116 104.45011 142.8589
#Oct 2023       131.8069 118.54708 145.0667 111.52776 152.0860

ets_forecast$mse
#[1] 10.95792
#RSME is 6.305638 on average forecast values were 6.305638 away from the actual
#MPE is 2.8% percenatge of error is around 2.8%


#Simple Moving Average

plot(candy_ts)
MA3_forecast <- ma(candy_ts,order=3) #taking into account the 5 most recent values
MA6_forecast <- ma(candy_ts,order=6) #taking into account the 9 most recent values
MA9_forecast <- ma(candy_ts,order=9) #taking into account the 9 most recent values


rwf_forecast <- rwf(candy_ts,12)
snaive_forecast <- snaive(candy_ts, 12)

plot(candy_ts)
lines(rwf_forecast$mean,col="Orange")
lines(snaive_forecast$mean,col="brown") #works best for my time series: snaive method
lines(MA3_forecast,col="Red")
lines(MA6_forecast,col="Blue")
lines(MA9_forecast,col="Green")

#as the order goes up the line comes up shorter and shorter from the end, and from the start as well, the sample in getting 
#smaller. And also as the order goes up the trend is somewhat followed but the magnitude shrinks in size as well.

#Simple Smoothing

SSE_Simple <- holt(candy_ts,gamma=FALSE)
attributes(SSE_Simple)
plot(SSE_Simple)

attributes(SSE_Simple)
#$names
#[1] "model"     "mean"      "level"     "x"         "upper"     "lower"     "fitted"    "method"    "series"    "residuals"

#$class
#[1] "forecast"

Acf(SSE_Simple$residuals)
Box.test(residuals(SSE_Simple), lag=12, type="Ljung")
#data:  residuals(auto_fit)
#X-squared = 13.084, df = 12, p-value < 2.2e-16

#As we can see from the Ljung box statistic that the p-values are < 2.2e-16 which makes them significant and we accept the null 
#hypothesis which means that the residual values are dependent

plot.ts(residuals(SSE_Simple))
hist(SSE_Simple$residuals)  #normal distribution

accuracy(SSE_Simple)
#                 ME     RMSE      MAE        MPE     MAPE     MASE      ACF1
#Training set 0.04457017 6.29552 4.615979 -0.1819999 4.631386 1.050803 0.2185404

SSE_Simple$mse
#[1] NULL
#RSME is 6.29552 on average forecast values were 6.29552 away from the actual
#MPE is 18% percenatge of error is around 18%


#Holt Winters

HW_forecast <- HoltWinters(candy_ts)
plot(HW_forecast)
HW_forecast

HWForecast <- predict(HW_forecast, 12)
plot(HWForecast)
HWForecast
HW_forecast #Alpha = 0.7137653 (the newest values does not have maximum weight but has relatively more weight than the last value)
#beta : 0 (this is the coefficient of trend smoothing in HW)
#gamma: 0.3677262 (seasonal model)

ets_forecast #sigma: 0.0345 (standard dev. of residuals)
#Initial states:l = 90.6178  
#b = 0.2533 

# AIC     AICc      BIC 
#894.2047 900.1464 941.7331 

attributes(HW_forecast)
#$names
#[1] "fitted"       "x"            "alpha"        "beta"         "gamma"        "coefficients" "seasonal"     "SSE"         
#[9] "call"        

#$class
#[1] "HoltWinters"

attributes(HWForecast)
#$dim
#[1] 12  1

#$dimnames
#$dimnames[[1]]
#NULL

#$dimnames[[2]]
#[1] "fit"

#$tsp
#[1] 2022.833 2023.750   12.000

#$class
#[1] "ts"

HWFResiduals <-residuals(HW_forecast)

Acf(HWFResiduals) #most values seem to be in the threshold region meaning apart from the 2 values outside of the region all 
#values are dependent on the last value

Box.test(HWFResiduals, lag=12, type="Ljung")
#Box-Ljung test
#data:  HWFResiduals
#X-squared = 27.423, df = 12, p-value = 0.006713

#As we can see from the Ljung box statistic that the p-values are 0.006713 which makes them significant and we accept the null 
#hypothesis which means that the residual values are dependent.

plot.ts(HWFResiduals)
hist(HWFResiduals)  #normal distribution


#ARIMA or Box-Jenkins

plot(candy_ts)
ndiffs(candy_ts) #as the ndiffs function gives out the value 1 it means that the model is not stationary, and that would mean 
#that we need 1 difference to get the stationary value.
#Seasonality is not needed for this.

tsdisplay(candy_ts)
candydiff1 <- diff(candy_ts, differences=1)
plot(candydiff1)
tsdisplay(candydiff1)

auto_fit <- auto.arima(candy_ts, trace=TRUE, stepwise = FALSE)
# Best model: ARIMA(1,0,0)(2,1,0)[12] with drift

auto_fit
# Series: candy_ts 
#ARIMA(1,0,0)(2,1,0)[12] with drift 

#Coefficients:
#        ar1     sar1     sar2   drift
#      0.6441  -0.3042  -0.2351  0.2083
#s.e.  0.0742   0.1040   0.1099  0.0579

#sigma^2 = 14.95:  log likelihood = -301.36
#AIC=612.72   AICc=613.31   BIC=626.18


attributes(auto_fit)
#$names
#[1] "coef"      "sigma2"    "var.coef"  "mask"      "loglik"    "aic"       "arma"      "residuals" "call"      "series"   
#[11] "code"      "n.cond"    "nobs"      "model"     "xreg"      "bic"       "aicc"      "x"         "fitted"   

#$class
#[1] "forecast_ARIMA" "ARIMA"          "Arima" 


Acf(auto_fit$residuals)
Box.test(residuals(auto_fit), lag=12, type="Ljung")
#data:  residuals(auto_fit)
#X-squared = 13.084, df = 12, p-value = 0.363

#As we can see from the Ljung box statistic that the p-values are 0.363 which makes them non-significant and we reject the null 
#hypothesis which means that the residual values are independent

plot.ts(residuals(auto_fit))
hist(auto_fit$residuals)  #normal distribution
tsdiag(auto_fit) #ACF test shows no relative significance amongst residuals as well same as the p-values for the Ljung-Box

accuracy(auto_fit)
#                   ME        RMSE      MAE      MPE      MAPE      MASE        ACF1
#Training set -0.004444951 3.602064 2.583457 -0.1100037 2.571757 0.5881103 -0.06809078

ARIMAF12 <- plot(forecast(auto_fit,h=12,level=c(99.5)))
ARIMAF12
ARIMAF24 <-plot(forecast(auto_fit,h=24,level=c(99.5)))
ARIMAF24

auto_fit$mse
#[1] NULL
#RSME is 3.602064 on average forecast values were 3.602064 away from the actual
#MPE is 11% percenatge of error is around 11%


#accuracy summary

accuracy(ets_forecast)
accuracy(naive_forecast)
accuracy(rwf_forecast)
accuracy(snaive_forecast)
accuracy(SSE_Simple)
accuracy(auto_fit)

#I would choose ets_forecast as it has the lowest MAPE. I chose MAPE because the units are in Percent, it is extremely useful 
#when observations are large numbers and also It can be used to compare same or different techniques as units is in %.


#Best MAPE Forecast

accuracy(ets_forecast)

#worst MAPE Forecast

accuracy(naive_forecast)

#conclusion 

#based on my analysis the value of time series will be cyclical as it has been so far in 1 and 2 both years


