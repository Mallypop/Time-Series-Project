function writing to organize our script 

first we can write a diagnostics function
with all the test we need to perform to check if our model is adequate enough to stop the model adaptation

well.. ok 
we need to be careful if we want to check for residuals or the whole model. 
so x will be the model and x$residuals and x$fitted are the other options we need

diagnostics <- function (x)
  {
  normality = shapiro.test(x$residuals); #check for normal distributed values
  stat.res =  adf.test(x$residuals); #check both residuals and fitted of the model for stationarity
  stat.fit = adf.test(x$fitted);
  x$residualsvector = as.vector(x$residuals);
 autocorr= dwt(x$residualsvector) ; #check for autocorrelation
 indep=  Box.test(x$residuals, type="Ljung-Box") #check for independence 
  output = list(normality, stat.res, stat.fit, autocorr, indep)
  names (output) = c("norm. distrb. of residuals", "stationarity of residuals", "stationarity of fitted values", "autocorrelation of residuals", "independence of residuals")
  return ( output )
}

diagnostics(smoothed)


####################


also we need to check for best model 
after having performed different options of gls 

we have a normal linear regression y ~ x 
we have a simple gls y~x with AR(1)
we have a simple gls y~x with AR(1) and seasonal factor which is seasfac=cycle(timeseries)
we have a 2-polynomial gls with AR(1) and the seasonal effect with COS/SIN curve

testall = function(x){
  
  ...  
  
}

###########################
op <- par(mfrow = c(3,1),
          mar = c(2,4,1,2)+.1,
          oma = c(0,0,2,0))
par(op)
mtext("Model residual correlogram", 
      line = 2.5, 
      font = 2, 
      cex = 1.2)
correlogram = function (x) { 

ACF= acf(resid(x),xlab = "")
plot(ACF)
}

correlogram(data.lm)
