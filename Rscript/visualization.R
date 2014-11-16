getwd()

setwd("C:/Users/schnuri/Desktop/Neuer Ordner/Dataset/")

#get from dataset the monthly corrected values for NAs 
#see what data you have
#eliminate columns you dont need 

data = co2month[,c(3,5)]
colnames(data)= c("year", "co2")

#visualize our data

attach(data)
plot(year, co2, add=T, type="n",las=1, xlab="Year", ylab="CO2 conc. (ppm)", main="CO2 concentration in the atmosphere")
grid (NULL,NULL, lty = 6, col = "cornsilk2") 
points(year, co2, col="cornflowerblue" )
#cant see much, maybe smooth the curve 
k <- 5
lines(year,filter(co2, rep(1/k,k)),col = 'red', type="l", lwd = 3 )

#check for data mistakes, ect. : visualize our data
x <- co2
op <- par(mfrow = c(1,2),
          mar = c(5,4,1,2)+.1,
          oma = c(0,0,2,0))
hist(x, freq=F,
     col = "light blue",
     xlab = "",
     main = "")
qqnorm(x,
       main = "")
qqline(x, 
       col = 'red')
par(op)
mtext("CO2 Concentration (ppm)", 
      line = 2.5, 
      font = 2, 
      cex = 1.2)

#differencing our variable
x <- diff(co2)
op <- par(mfrow = c(1,2),
          mar = c(5,4,1,2)+.1,
          oma = c(0,0,2,0))
hist(x, 
     col = "light blue",
     xlab = "",
     main = "")
qqnorm(x,
       main = "")
qqline(x, 
       col = 'red')
par(op)
mtext("CO2 Concentration (ppm) increments", 
      line = 2.5, 
      font = 2, 
      cex = 1.2)


op <- par(mfrow = c(3,1),
          mar = c(2,4,1,2)+.1,
          oma = c(0,0,2,0))
acf(x,      xlab = "")
pacf(x,     xlab = "")
spectrum(x, xlab = "", main = "")
par(op)
mtext("CO2 Concentration (ppm) diagnostics", 
      line = 2.5, 
      font = 2, 
      cex = 1.2)

#autocorrelation
#what it should look like, if you dont have autocorr. problems:
n <- 200
x <- rnorm(n)
acf(x)
#lag 0 is always y=1, because the data is compared with the true data, which are equal

x <- co2
acf(x)

#make a monthly time series
newco2=ts(co2, 1958,2014,12 )
class(newco2)
#decompose the time series into the trend, seasonal fluctiation and the random white noise
dec = decompose(newco2)
plot(dec)


#fit the linear model, that CO2 concentrations are dependent from time ( in months ) for time period of 1958 until 2014

data.lm = lm( co2 ~ year)


#fit predict values
MyData=data.frame(year=seq(from=(1958),to=2014, by=0.1))
G=predict(month.lm, newdata=MyData, type="response", se=T) #poission: type ="LINK", binomial: type="RESPONSE"

plot(year, co2, type="l", col="cornflowerblue", las=1, xlab="Year", ylab="CO2 Conc. (ppm)", main="CO2 Concentration in the Atmosphere")
#go for confidence interval 
F=(G$fit)
FSUP=(G$fit+1.96*G$se.fit) # make upper conf. int. 
FSLOW=(G$fit-1.96*G$se.fit) # make lower conf. int. 
lines(MyData$year, F, lty=1, col="darkblue")


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
library(tseries)
adf.test(co2, alternative = "stationary")
#is non stationary, has a trend 

#look at correlogram
#of autocorrelation (seasonal trend)
print(acf(data.lm$residuals))
#of partial autocorrelation (eliminates intermediate correlation between x(t) and x(t-1))
pacf(data.lm$residuals)

data.pacf=(pacf(data.lm$residuals))
data.pacf[1]
*****
  
#make gls 
library(nlme)
data.gls = gls(co2 ~ year,cor= corAR1(0.94)) #use lag one data.acf[2]

data.gls
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

#look at correlogram
#of autocorrelation (seasonal trend)

datagls.acf=(acf(data.gls$residuals))
print(acf(data.gls$residuals)
)#of partial autocorrelation (eliminates intermediate correlation between x(t) and x(t-1))
pacf(data.gls$residuals)

data.pacf=(pacf(data.gls$residuals))
datagls.acf[2]

