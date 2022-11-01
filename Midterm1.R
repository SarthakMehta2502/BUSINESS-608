#Midterm-1

install.packages("readr")
library(readr)
library(fpp)
library(fpp2)

NJ_MedianListingPrice_AllHomes <- read_csv("/Users/sarthakmehta/Desktop/NJ_MedianListingPrice_AllHomes.csv")
NJ_Home_Raw <- NJ_MedianListingPrice_AllHomes$Value
NJ_Home_TS <- ts(NJ_Home_Raw,frequency = 12, start = c(1996,4))

#Plot and Inference
NJ_Home_TS
plot(NJ_Home_TS) #plotting the TS plot
                 #Observation: from 1996 to ~2007-08 there was a sharp increase in the in the median listing of price and then 
                 #there was a sudden drop till the year 2012, and was in a constant decline(thanks to the 2008 financial 
                 #crisis). hence I would like to choose the data from the start of 2012 so that my forecast is not tampered due 
                 #to these fluctuations.

CNJ_Home_TS <- window(NJ_Home_TS, start = 2012)
plot(CNJ_Home_TS)

#mCentral Tendency

min(CNJ_Home_TS)
# [1] 265600

max(CNJ_Home_TS)
# [1] 311600

mean(CNJ_Home_TS)
# [1] 284414.7

median(CNJ_Home_TS)
# [1] 283350

quantile(CNJ_Home_TS)
#   25%    75%    
# 271950  293575  

boxplot((CNJ_Home_TS))

#Summary: Well the summaries show us that the amongst the median values calculated min median value of a house from the 
#period of 2012 to 2017 was 265,600$ and the max median value was 311,600$. Furthermore, the mean value was 284,414.7$ and 
#the median value was 283,350$. The 1st quartile was 271,950$ and the 3rd quartile was 293,575$. The box plot tells us that
#that the there are no outliers, and the minimum value is a bit more closer to the 1st quartile than the maximum value is to 
#the 3rd quatile,and the median is approximate in between the 1st and 3rd quartile. But when we look closely at the mean and 
#median we can see that as mean is a bit greater than median the median (2nd quartile line) would be positively skewed.


#Decomposition

stl_decomp <- stl(CNJ_Home_TS,s.window =12)
stl_decomp
plot(stl_decomp) #Decomposition Plot
#Yes the time series is seasonal, and it is additive seasonal not multiplicative as the magnitude is constant. For the month of
#January, September, October and December the values for the seasonal component are pretty high, i believe the reason for it is that 
#around the time of new-years people like to list their houses. And in the months of March, April, May and June the values are 
#really negative, the reason could be Summer season being around the corner and summer vacations.

seasadj(stl_decomp)

plot(CNJ_Home_TS)
lines(seasadj(stl_decomp), col="Orange")
#even after taking out the seasonal component the graph still looks the same this means that all the change that is happening 
#in the time series is happening due to fundamental changes and are not affected by seasonality.

#Naive

naive_forecast <- naive(CNJ_Home_TS,12) #give me the forecast for the next 12 months using Naive forecast method
plot(naive_forecast)

attributes(naive_forecast)
#$names
#[1] "method"    "model"     "lambda"    "x"         "fitted"    "residuals" "series"    "mean"      "level"     "lower"    
#[11] "upper"    

#$class
#[1] "forecast"

plot(naive_forecast$residuals) #the residual plot shows an anomaly around the end of year 2016 and another sharp decline
                               #just at the start of year 2017.

hist(naive_forecast$residuals) #shows a pretty decent normal distribution

Acf(naive_forecast$residuals) #shows all values are within thresholds apart from the 1st and 2nd value they are dependent on 
                              #the previous values (correaltion present) apart from them no values are dependent on each other.

accuracy(naive_forecast)
#                 ME     RMSE      MAE       MPE      MAPE       MASE      ACF1
#Training set  659.7015 848.3522 725.3731 0.2278946 0.2521256 0.08562583 0.6964852

ets_forecast <- ets(CNJ_Home_TS)
plot(CNJ_Home_TS)

forecast_ets <- forecast(ets_forecast, h=12) #forecasting for the next 12 months
plot(forecast_ets)
forecast_ets

#         Point Forecast  Lo 80    Hi 80    Lo 95    Hi 95
#Sep 2017       312821.2 312231.6 313410.8 311919.4 313722.9
#Oct 2017       314042.3 312753.0 315331.5 312070.5 316014.0
#Nov 2017       315263.4 313125.9 317400.9 311994.3 318532.5
#Dec 2017       316484.5 313370.3 319598.8 311721.7 321247.4
#Jan 2018       317705.6 313500.2 321911.1 311273.9 324137.4
#Feb 2018       318926.8 313525.7 324327.8 310666.6 327187.0
#Mar 2018       320147.9 313454.6 326841.2 309911.4 330384.4
#Apr 2018       321369.0 313293.0 329445.0 309017.9 333720.2
#May 2018       322590.1 313045.9 332134.3 307993.6 337186.7
#Jun 2018       323811.2 312717.6 334904.9 306844.9 340777.6
#Jul 2018       325032.4 312311.4 337753.3 305577.4 344487.4
#Aug 2018       326253.5 311830.6 340676.4 304195.6 348311.4

ets_forecast$mse
#[1] 169276.5 
#RSME is 848.3522 on average forecast values were 848.3522 away from the actual
#MPE is 22% percenatge of error is 22%
#MAPE is 25% for this model avg deviation from actual values is 25%


#Simple Moving Average

plot(CNJ_Home_TS)
MA3_forecast <- ma(CNJ_Home_TS,order=3) #taking into account the 5 most recent values
MA6_forecast <- ma(CNJ_Home_TS,order=6) #taking into account the 9 most recent values
MA9_forecast <- ma(CNJ_Home_TS,order=9) #taking into account the 9 most recent values


rwf_forecast <- rwf(CNJ_Home_TS,12)
snaive_forecast <- snaive(CNJ_Home_TS, 12)

plot(CNJ_Home_TS)
lines(rwf_forecast$mean,col="blue") #works best for my time series random walk 
lines(MA3_forecast,col="Red")
lines(MA6_forecast,col="Blue")
lines(MA9_forecast,col="Green")

#as the order goes up the line comes up shorter and shorter from the end, and from the start as well, the sample in getting 
#smaller

#Simple Smoothing

SSE_Simple <- HoltWinters(CNJ_Home_TS)
attributes(SSE_Simple)
#$names
#[1] "fitted"       "x"            "alpha"        "beta"         "gamma"        "coefficients" "seasonal"     "SSE"         
#[9] "call"        

#$class
#[1] "HoltWinters"

plot(SSE_Simple)
SSE_Simple$SSE
#[1] 20477725

#Holt Winters

HW_forecast <- HoltWinters(CNJ_Home_TS)
plot(HW_forecast)
HW_forecast

HWForecast <- predict(HW_forecast, 12)
plot(HWForecast)
HWForecast
HW_forecast #Alpha = 1 (the last value has maximum weight)
            #beta : 0.1539464 (this is the coefficient of trend smoothing in HW)
            #gamma: 0 (non-seasonal model)

ets_forecast #sigma:  0.0015 (standard dev. of residuals)
             #Initial states:l = 267559.0133 
                            #b = -1450.1725

#No residuals


#accuracy summary


accuracy(ets_forecast)
accuracy(naive_forecast)
accuracy(rwf_forecast)
accuracy(snaive_forecast)

#I would choose ets_forecast as it has the lowest MAPE. I chose MAPE because the units are in Percent, it is extremely useful 
#when observations are large numbers and also It can be used to compare same or different techniques as units is in %.


#Best MAPE Forecast

accuracy(ets_forecast)

#worst MAPE Forecast

accuracy(snaive_forecast)

#conclusion 

#based on my analysis the value of time series will increase in 1 and 2 both years


