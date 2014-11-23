#Forecasts functions: 


#1 fit predict values 
newtime= ts(start=c(2014, 10),end=c(2024,12),deltat=1/12)
pred = predict(smoothed.gls, newdata=newtime, se=T) #poission: type ="LINK", binomial: type="RESPONSE"

TIME <- as.numeric(time)
time.df <- data.frame(TIME=TIME, COS, SIN)
colnames(time.df)[-1] <- paste0("V", 1:12)
smoothed <- gls(as.numeric(ts.x) ~ TIME + I(TIME^2) + V1 + V2 + V3 + V4 + V5 +V6 +V7+V8 +V9 +V10 +V11 +V12, corr=corAR1(acf(dataseason.gls$residuals)$acf[2]), data=time.df)
new.df <- cbind.data.frame(TIME=as.numeric(time(newtime)), COS=COS[1:123,], SIN=SIN[1:123,])
colnames(new.df)[-1] <- paste0("V", 1:12)
pred = predictSE(smoothed, newdata=new.df, se.fit=T)
plot(ts.x, type="n",las=1, xlim=c(1960, 2025), ylim=c(300, 450), xlab="Year", ylab="CO2 conc. (ppm)", main="CO2 concentration in the atmosphere")
grid (NULL,NULL, lty = 6, col = "cornsilk2") 
points(ts.x ,type="l" )
par(mfrow=c(1,1))
lines(as.numeric(time(newtime)), pred$fit, col="red")
F=(pred$fit)
FSUP=(pred$fit+1.96*pred$se.fit) # make upper conf. int. 
FSLOW=(pred$fit-1.96*pred$se.fit) # make lower conf. int. 
lines(new.df$TIME, FSUP,lty=1, col="grey", lwd=3)
lines(new.df$TIME, FSLOW,lty=1, col="grey", lwd=3)
lines(new.df$TIME, F, lty=1, col="red", lwd=1)
legend("topleft",c("forecast for 10 years", "monthly mean data", "CI"), 
       pch=c(20,20), col=c("red", "black", "grey"))

# 2. Holtwinters 
#Forecasting: from past values (x1,x2,x3,...,xn) want to predict future values x(n+k
#), 
##holtwinters explanation: 
#if alpha is near 1, little smoothing, at is approx. xt 
# alpha is zero, highly smoothed estimates
#a = 0.2 compromise figure, change in mean between t-1 and t likel smaller than variance
#holt winters forecasts
#not specify aphla, beta, gamma to include errors, trend and seasonal component in the forecast

forecast <- HoltWinters(ts.x)
forecast10 <- forecast.HoltWinters(forecast,h=120)
par(mfrow=c(1,1))
plot.forecast(forecast10)

#check for the autocorrelation of the future values
acf(forecast10$residuals)#how is the lag.max defined? 
pacf(forecast10$residuals)
diagnostics(forecast10)

#3. auto arima 
#option of autmatized model selection by auto.arima 

autoarima = auto.arima(ts.x)
#gives me perfect adjusted 


forecast.arima = forecast.Arima(autoarima, h=120) #für 10 jahre #automatizing 10*period


par(mfrow=c(2,1))
plot.forecast(forecast.arima)
plot.forecast(forecast10)

#check for error distirbution 
par(mfrow=c(1,2))

plotForecastErrors(forecast10$residuals)
plotForecastErrors(forecast.arima$residuals)
#fits good

#run diagnostics
diagnostics( forecast10)
diagnostics(forecast.arima)

#look at both zoomed in forecasts
par(mfrow=c(1,2))

plot(forecast.arima, xlim=c(2010,2025), ylim=c(385,430))
plot.forecast(forecast10 ,xlim=c(2010,2025), ylim=c(385,430))




