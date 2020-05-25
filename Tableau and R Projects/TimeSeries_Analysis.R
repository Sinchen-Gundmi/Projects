install.packages('forecast', dependencies = TRUE)
library(sweep)
library(timetk)
library(ggplot2)


data<-("AirPassengers")
MF <- AirPassengers
str(MF)
head(MF)
ts(MF, frequency = 12, start = c(1949,1))
attributes(MF)
AP <- log(MF)
plot(MF)

#Decomposition of additive time series 
decomp <- decompose(MF)
decomp$figure
plot(decomp$figure,
     type = 'b',
     xlab = 'Month',
     ylab = 'Seasonality Index',
     col = 'blue',
     las = 2)

#ARIMA - Autoregressive Integrated Moving Avergae

x  <- forecast::auto.arima(MF)
attributes(forecast::auto.arima(MF))

#ACF and PACF Plots
# We use residuals values as it is the difference between actual and fitted values

acf(x$residuals, main = 'Correlogram')
pacf(x$residuals, main = 'Partial Correlogram')

#Ljung-Box Test

Box.test(x$residuals, lag = 20, type = 'Ljung-Box')

#Residual Plot

hist(x$residuals,
     col = 'red',
     xlab = 'Error',
     main = 'Histogram of residuals',
     freq = FALSE)
lines(density(x$residuals))

#Forecast

ff <- forecast::forecast(x,48)
ggplot2::autoplot(ff)
forecast::accuracy(ff)
