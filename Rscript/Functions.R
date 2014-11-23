function writing to organize our script 

first we can write a diagnostics function
with all the test we need to perform to check if our model is adequate enough to stop the model adaptation

well.. ok 
we need to be careful if we want to check for residuals or the whole model. 
so x will be the model and x$residuals and x$fitted are the other options we need

diagnostics <- function (x)
  {
  normality = shapiro.test(x$residuals); #check for normal distributed values # 
  stat.res =  adf.test(x$residuals); #check both residuals and fitted of the model for stationarity
  stat.fit = adf.test(x$fitted);
  x$residualsvector = as.vector(x$residuals);
 autocorr= dwt(x$residualsvector) ; #check for autocorrelation
 indep=  Box.test(x$residuals, type="Ljung-Box") #check for independence
  #lag for season is df: m-1 ( 12-1)
  #write if seasonal = TRUE lag=12-1, else write nothing 
  #there is high evidence that there are non-zero autocorr. 
  output = list(normality, stat.res, stat.fit, autocorr, indep)
  names (output) = c("norm. distrb. of residuals", "stationarity of residuals", "stationarity of fitted values", "autocorrelation of residuals", "independence of residuals")
  return ( output )
}

diagnostics(smoothed)


####################
plotting the histogram with the normal distribution to see wether the errors of the forecast model are well distributed


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
###########################


