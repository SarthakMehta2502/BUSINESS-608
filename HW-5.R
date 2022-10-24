#Homework-5

library(fpp)
library(fpp2)

cwindow <- window(usconsumption[,"income"], start = 1977) #my chosen window to use for the forecast

head(cwindow)
#         Qtr1       Qtr2       Qtr3       Qtr4
#1977   0.08238505 1.13105430 1.49011470 1.99549955
#1978   0.61490413 1.14134313  

attributes(cwindow)
#$tsp
#[1] 1977.00 2010.75    4.00

#$class
#[1] "ts"

plot(cwindow)   
stl_decomp <- stl(cwindow,s.window ="periodic")
stl_decomp
#components
#           seasonal      trend      remainder
#1977 Q1 -0.0003766242  0.55739860 -0.474636919
#1977 Q2 -0.0063059771  0.87764105  0.259719232
#1977 Q3 -0.0449910528  1.14120298  0.393902779
#1977 Q4  0.0516727941  1.33806381  0.605762943
#1978 Q1 -0.0003766242  1.20821215 -0.592931400 ...

plot(stl_decomp)  #Seasonal component no does not increase in magnitude, in trend -0.6 lowest; 1.8 highest ratio is
                  #0.6/1.8 = 33%, and seasonality os about 4% therefore total influence on data of S and T is about 37% and 
                  #rest is remainder,in the worst case. Which makes this a bad model as it only explains around ~40% of the data.

attributes(stl_decomp)
#$names
#[1] "time.series" "weights"     "call"        "win"         "deg"         "jump"        "inner"       "outer"      

#$class
#[1] "stl"


# Lets print out a seasonal adjustment

seasadj(stl_decomp)

# Plot a line on the graph

plot(cwindow)
lines(seasadj(stl_decomp), col="yellow") #even after taking out the seasonal component the graph still looks the same this 
                                         #means that all the change that is happening in the time series is happening due to 
                                         #fundamental changes and are not affected by seasonality.

# Default period forecast

f_stl <- forecast(stl_decomp)

# you can pass the # of period

f_stl <- forecast(stl_decomp,h=20) #forecasting for the next 20 periods.
f_stl
plot(f_stl) #as the current model is only explaning 37-40% of the data we get such a forecast which is not good at all.

# There is more than one way to do things

decomp_cwindow <- decompose(cwindow)

# Each one shows different attributes 

attributes(decomp_cwindow)
#$names
#[1] "x"        "seasonal" "trend"    "random"   "figure"   "type"    

#$class
#[1] "decomposed.ts"

seasadj(decomp_cwindow)

#         Qtr1          Qtr2          Qtr3          Qtr4
#1977  0.0702040256  1.1540900801  1.5399052790  1.9348542283
#1978  0.6027231026  1.1643789081  0.8578174810  0.7218947543
#1979  1.0750688056 -0.6775957699  0.6186914900  0.7364892283
#1980  0.2676503246 -1.3863795939  1.0742356250  1.9824850143
#1981 -0.2414810744  0.0368086151  2.2428410080  0.1412480553
#1982  0.1042829616  0.7212084251  0.4867291560  0.2906236913

