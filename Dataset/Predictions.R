#Forecasts functions: 

# Forecasting: from past values (x1,x2,x3,...,xn) want to predict future values x(n+k
#), 
##holtwinters explanation: 
#if alpha is near 1, little smoothing, at is approx. xt 
# alpha is zero, highly smoothed estimates
#a = 0.2 compromise figure, change in mean between t-1 and t likel smaller than variance
 
library(forecast)
auto.arima(ts.x)###if non-seasonal: bestfit 1,6,2 else seasonal 3,7,4 
ARIMA(1,1,1)(2,1,2)[12]

ts.arima = arima( ts.x , order=c(2,1,2))
ts.arima
forecast.arima = forecast.Arima(ts.arima)

par(mfrow=c(2,1))
plot.arima(ts.arima)
plot.forecast(forecast.arima)
plot.forecast(forecasts2)
plot(forecast(ts.arima))

#holt winters forecasts
#not specify aphla, beta, gamma to include errors, trend and seasonal component in the forecast

forecasts <- HoltWinters(ts.x)
library( forecast)

forecasts2 <- forecast.HoltWinters(forecasts,h=120)
par(mfrow=c(1,1))
plot.forecast(forecasts2)
summary(forecasts2)

#check for the autocorrelation of the future values
 acf(forecasts2$residuals, lag.max=12)#how is the lag.max defined? 

 Box.test(forecasts2$residuals, lag=11, type="Ljung-Box") # lag for season is df: m-1 ( 12-1)
#there is high evidence that there are non-zero autocorr. 

shapiro.test(forecasts2$residuals)
#normally distributed


plotForecastErrors <- function(forecasterrors)
{
  # make a histogram of the forecast errors:
  mybinsize <- IQR(forecasterrors)/4
  mysd   <- sd(forecasterrors)
  mymin  <- min(forecasterrors) - mysd*5
  mymax  <- max(forecasterrors) + mysd*3
  # generate normally distributed data with mean 0 and standard deviation mysd
  mynorm <- rnorm(10000, mean=0, sd=mysd)
  mymin2 <- min(mynorm)
  mymax2 <- max(mynorm)
  if (mymin2 < mymin) { mymin <- mymin2 }
  if (mymax2 > mymax) { mymax <- mymax2 }
  # make a red histogram of the forecast errors, with the normally distributed data overlaid:
  mybins <- seq(mymin, mymax, mybinsize)
  hist(forecasterrors, col="red", freq=FALSE, breaks=mybins)
  # freq=FALSE ensures the area under the histogram = 1
  # generate normally distributed data with mean 0 and standard deviation mysd
  myhist <- hist(mynorm, plot=FALSE, breaks=mybins)
  # plot the normal curve as a blue line on top of the histogram of forecast errors:
  points(myhist$mids, myhist$density, type="l", col="blue", lwd=2)
}

plotForecastErrors(forecasts2$residuals)
#fits good



#option of autmatized model selection by auto.arima 

auto.arima(ts.x)
#gives me perfect adjusted 

ts.arima = arima( ts.x , order=c(1,1,1)) #function of order autom.
ts.arima
forecast.arima = forecast.Arima(ts.arima, h=120) #für 10 jahre #automatizing 10*period

par(mfrow=c(1,2))
plot(forecast.arima, xlim=c(2010,2025), ylim=c(385,430))
plot.forecast(forecasts2 ,xlim=c(2010,2025), ylim=c(385,430))

plotForecastErrors(forecast.arima$residuals)



