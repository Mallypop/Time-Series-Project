getwd()

setwd("C:/Users/schnuri/Desktop/Neuer Ordner/Dataset/")

#get from dataset the monthly corrected values for NAs 
#see what data you have
#eliminate columns you dont need 

library(tseries)

data = co2month[,c(3,5)]
colnames(data)= c("year", "co2")
attach(data)

#trend without seasonal effect
par(mfrow=c(1,2))
rm(ts.seasonallyadjusted)

dec1=decompose(ts.x)
plot(dec1)
ts.seasonallyadjusted <- ts.x - dec1$seasonal
#We can then plot the seasonally adjusted time series using the "plot()" function, by typing:
plot(ts.seasonallyadjusted, las=1, main="removed seasonal fluctuation")
plot(ts.x, main="TS with seasonal fl.", las=1)


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
sqrt(diag(vcov(smoothed.gls)))
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

data.gls = gls(co2 ~ year,cor= corAR1(acf(resid(data.lm))$acf[2])) #use lag one data.acf[2]))
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
ts.x=ts(co2, c(1958,3),c(2014,10),12 )
class(ts.x)
length(ts.x)
time = time(ts.x)
time
length(time)
seas = cycle(ts.x)
seas
length(seas)


dataseason.gls = gls(ts.x ~ time + factor(seas),cor= corAR1(acf(resid(data.lm))$acf[2])) #use lag one data.acf[2]
dataseason.gls

par(mfrow=c(3,1))
acf(dataseason.gls$residuals)[1]
pacf(dataseason.gls$residuals)
spectrum(dataseason.gls$residuals)
par(mfrow=c(1,1))
ts.plot(dataseason.gls$residuals) 
#puuhh gar nich mal soo gut

ts.plot(cbind(ts.x, dataseason.gls$fitted),lty=1:2, col=c(1,2), main="Compare mean monthly data with gls model")
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
  COS[,i] <- cos(2*pi*i*time(ts.x))
  SIN[,i] <- sin(2*pi*i*time(ts.x)) 
}
#also was done by hand in self writing seasonality removal above
#thus we dont know how many terms we need to include, so for now we include all of them 

#before add a quadratic term ( poly (x,2)), x = years

time.norm <- (time(ts.x) - mean(time(ts.x)))/sd(time(ts.x)) 
time.norm <- time(ts.x)
##why: 
#x-mean(x)/sd(x) = standard normal distribution
acf(dataseason.gls$residuals)[2]

smoothed.gls<-gls(ts.x ~ time.norm + I(time.norm^2) +
                    COS[,1]+SIN[,1]+COS[,2]+SIN[,2]+
                    COS[,3]+SIN[,3]+COS[,4]+SIN[,4]+
                    COS[,5]+SIN[,5]+COS[,6]+SIN[,6],
                  corr=corAR1(acf(dataseason.gls$residuals)$acf[2]))
AIC(smoothed.gls) #even smaller 

smoothed.gls
summary(smoothed.gls)
par(mfrow=c(1,1))
ts.plot(cbind(ts.x, dataseason.gls$fitted, smoothed.gls$fitted), col=c(1,2,3), main="Compare mean monthly data with gls model")
legend(1960,400,c("Original", "Included seasonality ", "Smoothed for seasonality"),col=c(1,2,3), pch=c(20,20,20))

plot(year, co2, col="light grey")
#residuals compare
par(mfrow=c(1,1))
plot(data.lm)
plot(dataseason.gls)
plot(smoothed.gls)
library(car)


#perform durbin watson test on residuals of new model 
names(smoothed.gls)
str(smoothed.gls$residuals)
smoothed.gls$residuals = as.vector(smoothed.gls$residuals)
str(smoothed.gls$residuals)

#test for autocorrelation
dwt(smoothed.gls$residuals)
#p value higher than 0.05, keep the null hyp 
#keine autokorrelation

#not independent:
Box.test(smoothed.gls$residuals, type="Ljung-Box")
#stationarity
adf.test(smoothed.gls$fitted)  
adf.test(smoothed.gls$residuals) 
#stationary both 

#2.1 fit predict values
newtime= ts(start=c(2014, 10),end=c(2024,12),deltat=1/12)
pred = predict(smoothed.gls, newdata=newtime, se=T) #poission: type ="LINK", binomial: type="RESPONSE"

TIME <- as.numeric(time)
time.df <- data.frame(TIME=TIME, COS, SIN)
colnames(time.df)[-1] <- paste0("V", 1:12)
smoothed <- gls(as.numeric(yourts) ~ TIME + I(TIME^2) + V1 + V2 + V3 + V4 + V5 +V6 +V7+V8 +V9 +V10 +V11 +V12, corr=corAR1(acf(dataseason.gls$residuals)$acf[2]), data=time.df)
new.df <- cbind.data.frame(TIME=as.numeric(time(newtime)), COS=COS[1:123,], SIN=SIN[1:123,])
colnames(new.df)[-1] <- paste0("V", 1:12)
library(AICcmodavg)
pred = predictSE(smoothed, newdata=new.df, se.fit=T)
plot(yourts, type="n",las=1, xlim=c(1960, 2025), ylim=c(300, 450), xlab="Year", ylab="CO2 conc. (ppm)", main="CO2 concentration in the atmosphere")
grid (NULL,NULL, lty = 6, col = "cornsilk2") 
points(yourts ,type="l" )

lines(as.numeric(time(newtime)), pred$fit, col="red")
F=(pred$fit)
FSUP=(pred$fit+1.96*pred$se.fit) # make upper conf. int. 
FSLOW=(pred$fit-1.96*pred$se.fit) # make lower conf. int. 
lines(new.df$TIME, FSUP,lty=1, col="grey", lwd=3)
lines(new.df$TIME, FSLOW,lty=1, col="grey", lwd=3)
lines(new.df$TIME, F, lty=1, col="red", lwd=1)


legend("topleft",c("simple linear regression y~x", "monthly mean data"), 
       pch=c(20,20), col=c("red", "cornflowerblue"))



 
*****
##different correlation structures 
  
  #make a function for testing all possible p (0, 1,2,3) q = (0,1,2,3) for ARMA processes
  
 data.gls = gls(ts.x ~ time,cor= corAR1(acf(resid(data.lm))$acf[2]), data=yourts) #use lag one data.acf[2]))

dataseason.gls = update(data.gls, ts.x ~ time + factor(seas),cor= corAR1(acf(resid(data.lm))$acf[2])) #use lag one data.acf[2]

smoothed.gls = update(dataseason.gls, ts.x ~ time.norm + I(time.norm^2) +
                        COS[,1]+SIN[,1]+COS[,2]+SIN[,2]+
                        COS[,3]+SIN[,3]+COS[,4]+SIN[,4]+
                        COS[,5]+SIN[,5]+COS[,6]+SIN[,6],
                      corr=corAR1(acf(dataseason.gls$residuals)$acf[2]))
*****
  
  #compare three models 
  
 anova.all= anova(data.gls, dataseason.gls, smoothed.gls)
anova.all
#bestmodel with smallest AIC: 

bestmodel= anova.all$call[anova.all$AIC ==min(anova.all$AIC)]

#do diagnostics again

#check for normal distribution
shapiro.test(smoothed.gls$residuals)
#not normally distributed, non-parametric model

#check for stationarity
kpss.test(smoothed.gls$residuals)

#if ok next test: check for autocorrelation
smoothed.gls$residuals = as.vector(smoothed.gls$residuals)
str(smoothed.gls$residuals)
dwt(smoothed.gls$residuals)

#if ok next test: independence in a given time series

Box.test(smoothed.gls$residuals, type="Ljung")

#highly dependent 

smoothed.gls$coef


#compare standard errors
sqrt(diag(vcov(data.lm)))
sqrt(diag(vcov(data.gls)))
sqrt(diag(vcov(dataseason.gls)))
sqrt(diag(vcov(smoothed.gls)))
###### still need to check for diff. correlation structures 


