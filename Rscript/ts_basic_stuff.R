co2<- read.table("~/Arbeitsflaeche/studium/Best Practice R/Time-Series-Project-master/Dataset/co2_mm_mlo_mod.txt", header=TRUE, quote="\"")

#first we can make sure that we have a time series data

class(co2) #here we have a data frame

#we can transform it to a time serie with
co2time <- ts(co2$interpolated, start = c(1958,1), freq=12) #12 weil 12 monate
co2time
class(co2time)

#data visualization
start(co2time) # january 1958
end(co2time) #august 2014
cycle(co2time) #here we can control the months/periods

# simple plot
plot(co2time)
#or plot.ts(co2time)
boxplot(co2time~cycle(co2time))

#we can see that our data follows a trend. The trend can be simply seen by aggregating
plot(aggregate(co2time))
#(we could also have smoothed the data, a good packeged for this is  the TTR packege with the SMA()function)

# a time serie consists ofo 3 components; a trend component, an irregunlar (random) component and (if it is a seosonal ts) seasonal component.
# we can decompose the ts and plot these components:
plot(decompose(co2time)) #it seems that our data can probably be described using an additive model, since the random fluctuations 
#in the data are roughly constant in size over time (constant seasonal component)

#we can see each component with:
co2timecomponents<- decompose(co2time)
co2timecomponents$seasonal
#it seems that our seasonal component is positiv until the sommer months, were it turns to be negativ and turning to be positiv again in the winter

#we can see the trend for the first year:
ts.plot(dec1$trend[1:12])
#and we can see that this seasonal component is constant over all the years 
ts.plot(aggregate(dec1$trend))


names(dec1)
#we can seasonally adjust our time series if we substract this component
co2adjusted<- co2time-co2timecomponents$seasonal
ts.plot(co2adjusted)
#now we have just the random and the trend component


#if we have a time series that can be described using an additive model,
#we can short-time forecast using exponential smoothing
hw<-HoltWinters(co2adjusted, beta = F, gamma = F)
hw
#the alpha value tells us the weight of the previous values for the forecasting
#values of alpha that are close to 0 mean that little weight is placed on the most recent observations when making forecasts of future values
hw2<-HoltWinters(co2time, beta = F, gamma = T) #gamma is for the seasonality
hw2
plot(hw2)
#the red line is the forecast
#Holtwinters just makes forecasts for the time period covered by the original data.
#if we want to forecast for the future, we need the packeged "forecast"

library(forecast)
hw3<- forecast.HoltWinters(hw2, h=15)
plot(hw3)
#for the next 15 years
plot.forecast(hw3)




#we have determinisic data -> we can use regressions
#we work with the data frame co2

attach(co2)

#lets try a linear model
#abline works
plot(year, interpolated, type="l")
abline(lm(co2time~time(co2time)), col=2, lwd=2)


mod1<- lm(interpolated~year)
coef(mod1)
summary(mod1)#that is not so meaningful yet

#fit predict values
co2new=data.frame(year=seq(from=(1958),to=2014, by=0.1))
f=predict(mod1, newdata=co2new, type="response", se=T) #poission: type ="LINK", binomial: type="RESPONSE"
plot(year, interpolated, type="l", las=1, xlab="Year", ylab="CO2 Conc. (ppm)", main="CO2 Concentration in the Atmosphere")

#with the conf. intervals
F=(f$fit)
FSUP=(f$fit+1.96*f$se.fit) 
FSLOW=(f$fit-1.96*f$se.fit) 
lines(co2new$year, F, lty=1, col=2)
lines(co2new$year, FSLOW, lty=2, col=2)
lines(co2new$year, FSUP, lty=2, col=2)


#we can check the errors
sqrt(diag(vcov(mod1)))

acf(resid(mod1))$acf[2]# it seems that our residuals are determinated by seasonality 
#that is the correlation value, we can use it at the gls
pacf(resid(mod1))

library(nlme)
modgls<- gls(co2time~time(co2time), cor=corAR1(acf(resid(mod1))$acf[2]))
modgls
confint(modgls) #CIs for slope do not encompass 0: 
#1) Estimates are significant
#2) Trend is significant

pacf(resid(modgls))
#also residuals are understimated and slope has a to narrow ci
sqrt(diag(vcov(modgls)))#lower errors



# predicting with seasonal indicators -------------------------------------

# we add seasonal indicators

cycleind<- cycle(co2time)
timeind<- time(co2time)
modgls2<-gls(co2time~timeind+factor(cycleind), cor=corAR1(0.97))
modgls2
acf(resid(modgls2))
#that means, that the resisuals are strong correlated with the previous values
pacf(resid(modgls2))# much better!

ts.plot(modgls2$residuals, ylab="GLS Residuals")
ts.plot(cbind(co2time, modgls2$fitted), col=c(1,2))
legend(1960,400,c("Original", "Fitted"),col=c(1,2),bty = "n", lty=c(1,1))

#to predict, we have to create a new time sequence
newtime<-time(ts(start=2010, end=c(2020, 12), fr=12))
TIME<-(newtime-mean(time(co2time)))/sd(time(co2time))
SIN<-COS<-matrix(nr=length(newtime), nc=6)
for (i in 1:6) {
  COS[,i] <-cos(2*pi*i*time(newtime))
  SIN[,i] <-sin(2*pi*i*time(newtime))
}
 SIN<-SIN[,-6]
 COS<-COS[,-(5:6)]

ml.shgls<-gls(co2time~ TIME + I(TIME^2) )

newdata<-data.frame(TIME=as.vector(TIME), SIN=SIN, COS=COS)
 pred<-ts(predict(modgls2, newdata), st=2010, fr=12)
plot(co2time, pred, col=c(1,2),lty=1)
