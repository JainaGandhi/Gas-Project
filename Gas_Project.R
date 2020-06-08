#Time Series Forecasting Project - Gas Dataset

library(forecast)
GasData = gas

class(GasData) #Already it is time series object so no need to convert into time series object using ts()

# To print start of the series
start(GasData)

# To print end of the series
end(GasData)

# To print frequency of the series
frequency(GasData) #here we get 12 as a frequency value which means we have monthly data

# to print summary of the data
summary(GasData)


#here we are starting from year 1970, Jan month onwards
GasData = window(GasData,start = c(1970,1),frequency = 12) 
plot(GasData,main = "Monthly Gas Production in Australia",xlab = "Year",ylab = "Gas Production")

# Aggregation at a Quarter level
gasQuarter = aggregate(GasData, nfrequency=4)
plot.ts(gasQuarter, main = "Quarterly Gas Production in Australia", xlab = "Year", ylab = "Gas Production")

# Aggregation at a year level
gasYear = aggregate(GasData, nfrequency=1)
plot.ts(gasYear, main = "Yearly Gas Production in Australia", xlab = "Year", ylab = "Gas Production")


monthplot(GasData,main = "Monthly Gas Production in Australia",xlab = "Year",ylab = "Gas Production")


#install.packages("dygraphs")
library(dygraphs)

dygraph(GasData)
# There is an increasing trend in gas production in australia
# gas production has strong seasonality as there intra-year stable fluctuations repeatable every year  after year with respect to timing, direction & magnitude.

#_________________________________________________________________________________

library(xts)
#?periodicity
periodicity(GasData)

#Arima model

# Null Hypothesis Ho : Time Series Non - Stationery
# Alt Hypothesis  Ha : Time Series is Stationery

#check for stationery
library(tseries)
adf.test(GasData,k=12) #-- adf.test on whole data (1970 till end)
#p-value(0.8488) is greater then alpha(0.05) we fail to reject null hypothesis 
#hence, time series is not stationary

#Decompose the data
decomp = stl(GasData, s.window = 5)
plot(decomp)
deseasonal_gas=seasadj(decomp)
plot(deseasonal_gas)

#Differencing the time series data
count_d1 = diff(deseasonal_gas, differences = 1)
plot(count_d1)
adf.test(count_d1, alternative = "stationary",k=12)
#p-value(0.01) is less then alpha(0.05) we reject null hypothesis 
#hence, differenced time series is  stationary


#acf and pacf for differenced time series
acf(count_d1, main='ACF for Differenced Series') #q = 0
pacf(count_d1, main='PACF for Differenced Series') #p= 2
#From the ACF plot, there is a cut off after lag 0. This implies that q=0. 
#PACF cuts off after lag 2. Hence p=2.


#Splitting into training and test sets
gasTrain = window(deseasonal_gas, start=c(1970,1), end=c(1993,12))
gasTest= window(deseasonal_gas, start=c(1994,1))

gasTrain
gasTest


#gasARIMA = arima(gasTrain, order=c(1,1,1))

gasARIMA = arima(gasTrain, order=c(2,1,0))
gasARIMA
#p=2 d=1 q=0 aic = 4840.91


acf(gasARIMA$residuals)


#Ljung box test
#H0: Residuals are independent
#Ha: Residuals are not independent

Box.test(gasARIMA$residuals, type="Ljung-Box")
#Residuals are independent (p-value is greater then alpha 0.47 > 0.05)

#Forecasting with the ARIMA model for 20 periods
fcast <- forecast(gasARIMA, h=20)
fcast
plot(fcast)

#Accuracy of the forecast for 20 periods
accuracy(fcast, gasTest)
#mape(train & test = 2.70 & 3.23)


#Forecasting with the ARIMA model for 12 periods
fcast <- forecast(gasARIMA, h=12)
fcast
plot(fcast)

#Accuracy of the forecast for 12 periods
accuracy(fcast, gasTest)
#mape(train & test = 2.70 & 3.60)

#__________________________________________________________________________
#Auto-arima
fit.arima = auto.arima(gasTrain, seasonal = TRUE)
fit.arima
#Auto arima considers p=1 d=1 q=0
# aic = 4701.22

#Forecasting with the Auto-ARIMA model for 20 periods
gasArima = forecast(fit.arima, h = 20)
plot(gasArima)

#Accuracy of the forecast for 20 periods
accuracy(gasArima, gasTest)
#mape(train & test = 1.94 & 2.98)

#Forecasting with the Aauto-RIMA model for 12 periods
gasArima = forecast(fit.arima, h = 12)
plot(gasArima)

#Accuracy of the forecast for 12 periods
accuracy(gasArima, gasTest)
#mape(train & test = 1.94 & 3.31)

