getwd()

setwd("C:/Users/schnuri/Desktop/Neuer Ordner/Dataset/")

#get from dataset the monthly corrected values for NAs 
#see what data you have
#eliminate columns you dont need 

library(tseries)

data = co2month[,c(3,5)]
colnames(data)= c("year", "co2")
attach(data)

#make a monthly time series:
x= co2
ts.x=ts(x, 1958,2014,12 )
class(ts.x)

#2. fit the linear model, that CO2 concentrations are dependent from time ( in months ) for time period of 1958 until 2014
data.lm = lm( co2 ~ year)

#visualize the residuals of the linear model:
par(mfrow=c(2,2))
plot(data.lm)
par(mfrow=c(1,1))
#problems in spread of variance and normal distribution

#check for normal distribution of residuals:
shapiro.test(data.lm$residuals)
#highly not normally distributed 

#check for differences of co2 values to 
qqnorm(diff(co2))
abline(h=0)
#look at estimated parameters: close to underlying parameters:
coef(data.lm)

#look at standard errors
sqrt(diag(vcov(data.lm)))
#highly underestimated, standard errors are higher in true ! 
summary(data.lm)
#p value is not right evidence , misleading 

#data tranformation not useful! 
#check for stationarity
adf.test(co2, alternative = "stationary")
#is nonstationary, has a trend 

#look at correlogram
#of autocorrelation (seasonal trend)
print(acf(data.lm$residuals))
print(acf(diff(x)))
#of partial autocorrelation (eliminates intermediate correlation between x(t) and x(t-1))
pacf(data.lm$residuals)
data.pacf=(pacf(data.lm$residuals))
*****
#make gls 
library(nlme)
#serial correlation model hast usually a first order autorregressive term as the lag 1 

data.gls = gls(co2 ~ year,cor= corAR1(0.94)) #use lag one data.acf[2]

data.gls
print(acf(data.gls$residuals))
summary(data.gls)
summary(data.lm)
#alright, RSE of new gls is less underestimated 

#check confidence interval
confint(data.gls)
#bigger interval 
#parameter estimates are significant, trend is significant


plot(data.gls)
plot(data.lm)
#well spread is already smaller of variances 

#seasonality
ts.x=ts(x, 1958,2014,12 )
class(ts.x)
length(ts.x)
time = time(ts.x)
time
length(time)
seas = cycle(ts.x)
seas
length(seas)


dataseason.gls = gls(ts.x ~ time + factor(seas),cor= corAR1(0.985)) #use lag one data.acf[2]
dataseason.gls

par(mfrow=c(3,1))
acf(dataseason.gls$residuals)[1]
print(acf(dataseason.gls$residuals))
pacf(dataseason.gls$residuals)
spectrum(dataseason.gls$residuals)
par(mfrow=c(1,1))
ts.plot(dataseason.gls$residuals) 
#puuhh gar nich mal soo gut

ts.plot(cbind(ts.x, dataseason.gls$fitted),lty=1:2, col=c(1,2))
legend(1960,400,c("Original", "Fitted for seasonality ?"),col=c(1,2),lty=c(1, 2))

##AIC #smallest value best value = best model (trade off blabla)

#for normal lm
AIC(data.lm)
AIC(data.gls)
AIC(dataseason.gls)

#ohkay but looks still strange no? 


##try to smoothen more the seasonal stuff
#include sin fct.
SIN<-COS<-matrix(nr=length(ts.x), nc=6)
for (i in 1:6) {
  COS[,i] <-cos(2*pi*i*time(ts.x))
  SIN[,i] <-sin(2*pi*i*time(ts.x)) }
#also was done by hand in self writing seasonality removal above
#thus we dont know how many terms we need to include, so for now we include all of them 

#before add a quadratic term ( poly (x,2)), x = years

time.norm <-(time(ts.x) - mean(time(ts.x)))/sd(time(ts.x)) 
##why: 
#x-mean(x)/sd(x) = standard normal distribution
acf(ts.x)[2]

smoothed.gls<-gls(ts.x~ time.norm + I(time.norm^2) +
                    COS[,1]+SIN[,1]+COS[,2]+SIN[,2]+
                    COS[,3]+SIN[,3]+COS[,4]+SIN[,4]+
                    COS[,5]+SIN[,5]+COS[,6]+SIN[,6], corr=corAR1(0.893))
AIC(smoothed.gls) #even smaller

smoothed.gls
summary(smoothed.gls)


plot(year, dataseason.gls$fittedvalues,type="l")
plot(year, co2, col="light grey")
#residuals
plot(smoothed.gls)
library(car)


#perform durbin watson test on residuals of new model 
names(smoothed.gls)
str(smoothed.gls$residuals)
smoothed.gls$residuals = as.vector(smoothed.gls$residuals)
str(smoothed.gls$residuals)
dwt(smoothed.gls$residuals)

#p value higher than 0.05, keep the null hyp 
#keine autokorrelation
