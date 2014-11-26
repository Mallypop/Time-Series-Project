#1. get from dataset the monthly corrected values for NAs 
#see what data you have
#eliminate columns you dont need 

data = co2month[,c(3,5)]
colnames(data)= c("year", "co2")
attach(data)

#2. visualize your data

#lokks like time series 

#3. make a time series with our data
ts.x=ts(co2, c(1958,3),c(2014,10),12 )
class(ts.x)
length(ts.x)
time = time(ts.x)
time
length(co2)
length(year)
length(time)
seas = cycle(ts.x)
seas
length(seas)

#time series visualization
dec1=decompose(ts.x)
plot(dec1)
ts.seasonallyadjusted <- ts.x - dec1$seasonal
#in some cases it might be interesting to have the model without the seasonal fluctuations to depict change easier.
#We can then plot the seasonally adjusted time series using the "plot()" function, by typing:
par(mfrow=c(1,2))
plot(ts.x, main="TS with seasonal fl.", las=1)
plot(ts.seasonallyadjusted, las=1, main="removed seasonal fluctuation")
#trend without seasonal effect

#check for stationarity of time series
adf.test(ts.x, alternative = "stationary")
adf.test (co2tranform)
co2tranform= log(co2) # 0.69 still nonstationary

yourtsdiff= diff(ts.x)
adf.test (yourtsdiff) # now stationary ts.x!! 


#is highly nonstationary, has a trend we need to include later in the model

#4. fit the linear model, that CO2 concentrations are dependent from time ( in months ) for time period of 1958 until 2014
datalm = lm(co2~ year)
#in simple linear regression you cannot include a time series, we have to use the original data. 

#visualize the residuals of the linear model:
par(mfrow=c(2,2))
plot(datalm)
par(mfrow=c(1,1))
#problems in spread of variance and normal distribution

#check for normal distribution of residuals:
shapiro.test(datalm$residuals)
#highly not normally distributed 

#check for differences of co2 values to 
#qqnorm(diff(co2))

#look at correlogram
#of autocorrelation (seasonal trend)
print(acf(datalm$residuals))
print(acf(diff(ts.x)))
#of partial acf (eliminates intermediate correlation between x(t) and x(t-1))
pacf(datalm$residuals)
pacf(diff(ts.x))
#spectrum to look for cycles 
spectrum(data.lm$residuals)
A=(spectrum(data.lm$residuals))
#list(A$freq[A$spec==pmax(A$spec)])
#1/max = cycle , here 1/0.75 = 12 

#look at standard errors
sqrt(diag(vcov(data.lm)))
#highly underestimated, standard errors are higher in true ! 

summary(data.lm)
#p value is not right evidence , misleading 
*****
#6. make gls 
library(nlme)

#serial correlation model hast usually a first order autorregressive term as the lag 1 

data.glsAR = gls(ts.x ~ time,cor= corAR1(acf(resid(data.lm))$acf[2])) #use lag one data.acf[2] as general 
#try to fit other correlation structures: 
data.glsARMA1 = gls (ts.x ~ time, cor = corARMA (c(0.2,0.2),p=1, q=1 )) #lag 1 
data.glsARMA2 = gls (ts.x ~ time, cor=corARMA(c(0.2,0.2,0.2, 0.2), p=2, q=2)) #lag 2

print(anova(data.glsAR, data.glsARMA1, data.glsARMA2))

data.glsARMA1
data.glsARMA2
#run diagnostics
diagnostics(data.glsARMA2)
list = as.list(diagnostics(data.glsARMA2))
print(list)

acf(data.gls$residuals)
pacf(data.gls$residuals)
summary(data.gls)
summary(datalm)
#alright, RSE of new gls is less underestimated 
#parameter estimates are significant, trend is significant

plot(data.lm)

plot(data.glsAR)
plot(data.glsARMA1)
plot(data.glsARMA2) #residuals looking same
#well spread is already smaller of variances 


#function writing: if anova$AIC lowest, choose this as bestmodel
#go on with seasonal effect: function: 
#7. try to fit for seasonal component and autocorrelation with best corstruct
dataseasongls = gls(ts.x ~ time + factor(seas), cor=corARMA( c(0.2,0.2,0.2, 0.2),p=2, q=2)) #use lag order 2 and moving average from above 

#
par(mfrow=c(2,1))
acf(dataseasongls$residuals)
pacf(dataseasongls$residuals)

par(mfrow=c(1,1))
ts.plot(dataseasongls$residuals) 
#puuhh gar nich mal soo gut
anova(data.glsARMA1, data.glsARMA2, dataseasongls)
#oh boy cannot compare gls with different fixed effects, REML comparisons not meaningful, added a fixed effect: factor(seas)
#so, maybe just look at AIC separately 

##AIC #smallest value best value = best model (trade off blabla)

AIC(data.glsARMA2)
AIC(dataseasongls)
#write function choosing best model 


ts.plot(cbind(ts.x, data.glsARMA2$fitted,dataseasongls$fitted),lty=1:2, col=c(1,2,3), main="Compare mean monthly data with gls model")
legend(1960,400,c("Original", "Fitted for Autocorrelation","Fitted for seasonality"),col=c(1,2,3),lty=c(1, 2,3))




##try to generate better seasonal effect and add a quadratic term for the slight bending: 
#include continuous sin cos fct.
SIN<-COS<-matrix(nr=length(ts.x), nc=6)
for (i in 1:6) {
  COS[,i] <- cos(2*pi*i*time(ts.x))
  SIN[,i] <- sin(2*pi*i*time(ts.x)) 
}
#also was done by hand in self writing seasonality removal above
#thus we dont know how many terms we need to include, so for now we include all of them 

#before add a quadratic term ( polynomial (x,2)), x = years will change the linear regression to include the slight curve  


harmonizedgls<-gls(ts.x ~ time + I(time^2) +
                    COS[,1]+SIN[,1]+COS[,2]+SIN[,2]+
                    COS[,3]+SIN[,3]+COS[,4]+SIN[,4]+
                    COS[,5]+SIN[,5]+COS[,6]+SIN[,6],
                  corr=corAR1(acf(dataseasongls$residuals)$acf[2]))
AIC(harmonizedgls) #even smaller 

harmonizedARMAgls<-gls(ts.x ~ time + I(time^2) +
                      COS[,1]+SIN[,1]+COS[,2]+SIN[,2]+
                      COS[,3]+SIN[,3]+COS[,4]+SIN[,4]+
                      COS[,5]+SIN[,5]+COS[,6]+SIN[,6]
                    , cor=corARMA(p=2, q=2))
AIC(harmonizedARMAgls) #even smaller 
anova(harmonizedgls,harmonizedARMAgls) #significantly better ARMA

par(mfrow=c(1,1))
ts.plot(cbind(ts.x,harmonizedARMA.gls$fitted), col=c(1,3), main="Compare mean monthly data with gls model")
legend(1960,400,c("Original", "Included seasonality ", "Polynm. + seasonality"),col=c(1,2,3), pch=c(20,20,20))

#residuals compare
par(mfrow=c(2,2))
plot(data.lm, which = 1:1)
plot(data.glsARMA2, which=1:1)
plot(harmonizedARMA.gls, which = 1:1)
harmonizedARMA.gls$residuals
diagnostics(harmonizedARMA.gls)
qqnorm(harmonizedARMA.gls, abline=c(0,1))

plot(data.lm)
#bestmodel with smallest AIC: 
anova.all = anova( harmonized.gls,harmonizedARMA.gls)
bestmodel= anova.all$call[anova.all$AIC ==min(anova.all$AIC)]
bestmodel
#Our best model fitted by hand is the model: function bestmodel

***** 


#compare standard errors
sqrt(diag(vcov(data.lm)))
sqrt(diag(vcov(data.gls)))
sqrt(diag(vcov(data.glsARMA2)))
sqrt(diag(vcov(harmonizedARMA.gls)))
#you see now the estimated errors are not underestimated anymore

*****
  yes this was the gls 
#there are still some diagnostics failed! 
#non normally distributed
#not independent
#but the autocorrelation was solved
#and the model is now stationary 

now, run the auto arima for a lot easier model fitting 


*****
  Other option: running super fast 

  autoarima = auto.arima(ts.x)
#gives me perfect adjusted 
print(autoarima)
#even less AIC ! 
fitted=fitted(autoarima)
par(mfrow=c(1,1))
ts.plot(cbind(ts.x, harmonizedARMAgls$fitted), col=c(1,2))
lines( fitted, col=3)
legend(1960,400,c("Original",  "GLS", "Autoarima "),col=c(1,2,3), pch=c(20,20,20))

par(mfrow=c(1,3))
acf(autoarima$residuals)
pacf(autoarima$residuals)
spectrum(autoarima$residuals)


qqnorm(autoarima$residuals)
qqline(autoarima$residuals)
