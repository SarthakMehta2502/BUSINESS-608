#Homework-4

library(fpp)
library(fpp2)
library(TTR)  #library for moving average

usconsumption #is my chosen timeseries
usconsumption[,"income"] #is my chosen column from the above timeseries

attributes(usconsumption[,"income"]) # it is timeseries, starts from year 1970 to 2010, the periodicity is quaterly meaning 
                                     #4 months data.
#$tsp
#[1] 1970.00 2010.75    4.00

#$class
#[1] "ts"

# biggest change is around 1975, getting rid of all of it before forecasting so that it does not hamper the forecast.

cwindow <- window(usconsumption[,"income"], start = 1977) #my chosen window to use for the forecast
plot(cwindow) 
Acf(cwindow)        

#take Mean of all available history

mean_forecast <- meanf(cwindow,10) #give me the forecast for the next 10 months using mean forecast method
plot(mean_forecast)

# Naive

naive_forecast <- naive(cwindow,8) #give me the forecast for the next 8 months using Naive forecast method
plot(naive_forecast)

# Random Walk

rwf_forecast <- rwf(cwindow,5)  #basically random walk forecast predicts the future values where the future is not 
                                #dependent on the past values.
rwf_forecast2 <- rwf(cwindow,5, drift=TRUE) #trying to include a trend component in the model but no effect from it 
                                            #can be observed.
plot(rwf_forecast)
plot(rwf_forecast2)

# Seasonal Naive

snaive_forecast <- snaive(cwindow,5)
plot(snaive_forecast)

# Moving Averages

MA5_forecast <- ma(cwindow,order=5) #taking into account the 5 most recent values
MA9_forecast <- ma(cwindow,order=9) #taking into account the 9 most recent values
plot(MA5_forecast)  #plotting the forecast based on the recent 5 values
plot(MA9_forecast)  #plotting the forecast based on the recent 9 values

# plot all in a single chart
plot(mean_forecast)
lines(naive_forecast$mean,col="red")
lines(rwf_forecast$mean,col="green")
lines(rwf_forecast2$mean,col="purple")  #slight effect can be observed from the drift component
lines(snaive_forecast$mean,col="black")
lines(MA5_forecast,col="orange")
lines(MA9_forecast,col="Blue")

attributes(naive_forecast)

#$names
#[1] "method"    "model"     "lambda"    "x"         "fitted"    "residuals" "series"    "mean"      "level"     "lower"    
#[11] "upper"    

#$class
#[1] "forecast"


# Decomposition

ets_forecast <- ets(cwindow)
plot(ets_forecast)
attributes(ets_forecast)

#$names
#[1] "loglik"     "aic"        "bic"        "aicc"       "mse"        "amse"       "fit"        "residuals"  "fitted"    
#[10] "states"     "par"        "m"          "method"     "series"     "components" "call"       "initstate"  "sigma2"    
#[19] "x"         

#$class
#[1] "ets"

ets_forecast$mse
#[1] 0.710923

# HoltWinters

HW_forecast <- HoltWinters(cwindow)
plot(HW_forecast)
SSE_Simple <- HoltWinters(cwindow,beta=FALSE,gamma=FALSE)
attributes(SSE_Simple)
#$names
#[1] "fitted"       "x"            "alpha"        "beta"         "gamma"        "coefficients" "seasonal"     "SSE"         
#[9] "call"        

#$class
#[1] "HoltWinters"


plot(SSE_Simple)
SSE_Simple$SSE
#[1] 103.1529

head(SSE_Simple$fitted)

            #xhat      level
#1977 Q2 0.08238505 0.08238505
#1977 Q3 0.15983743 0.15983743
#1977 Q4 0.25808875 0.25808875
#1978 Q1 0.38641004 0.38641004
#1978 Q2 0.40328611 0.40328611
#1978 Q3 0.45779735 0.45779735


#Forecast

forecast_ets_1 <- forecast.ets(ets_forecast, h=5)
plot(forecast_ets_1)
forecast_ets_2 <- forecast(ets_forecast, h=5) #same result but a better method to plot the ets forecast
plot(forecast_ets_2)
