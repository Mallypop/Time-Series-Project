#new model
#braucht zuvor die sachen aus gls or differencing

auto.arima(ts.x)
ARIMA(1,1,1)(2,1,2)[12]

ts.arima = arima( ts.x , order=c(2,1,2))
ts.arima
forecast.arima = forecast.Arima(ts.arima)

par(mfrow=c(2,1))
plot.forecast(forecast.arima)
plot.forecast(forecasts2)


#holt winters 

forecasts <- HoltWinters(gls.seasonallyadjusted, beta=FALSE, gamma=FALSE)
library( forecast)

forecasts2 <- forecast.HoltWinters(forecasts,h=120)
par(mfrow=c(1,1))
plot.forecast(forecasts2)


 acf(forecasts2$residuals, lag.max=40)

 Box.test(forecasts2$residuals, lag=40, type="Ljung-Box")

#there is high evidence that there are non-zero autocorr. 

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




#new model

auto.arima(ts.seasonallyadjusted)
#gives me perfect adjusted 

ts.arima = arima( ts.seasonallyadjusted , order=c(3,2,4)) #function of order autom.
ts.arima
forecast.arima = forecast.Arima(ts.arima, h=120) #für 10 jahre

par(mfrow=c(1,2))
plot(forecast.arima, xlim=c(2010,2025), ylim=c(385,430))
plot.forecast(forecasts2 ,xlim=c(2010,2025), ylim=c(385,430))

plotForecastErrors(forecast.arima$residuals)

shapiro.test(forecast.arima$residuals)
#normally distributed
