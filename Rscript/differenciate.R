getwd()

setwd("C:/Users/schnuri/Desktop/Neuer Ordner/Dataset/")

#get from dataset the monthly corrected values for NAs 
#see what data you have
#eliminate columns you dont need 

library(tseries)

data = co2month[,c(3,5)]
colnames(data)= c("year", "co2")

#1. visualize our data
search()
attach(data)
plot(year, co2, type="n",las=1, xlab="Year", ylab="CO2 conc. (ppm)", main="CO2 concentration in the atmosphere")
grid (NULL,NULL, lty = 6, col = "cornsilk2") 
points(year, co2, col="cornflowerblue" )
#1.1 cant see much, maybe smooth the curve 
k <- 5
lines(year,filter(co2, rep(1/k,k)),col = 'red', type="l", lwd = 3 )

#2. fit the linear model, that CO2 concentrations are dependent from time ( in months ) for time period of 1958 until 2014
data.lm = lm( co2 ~ year)

#2.1 fit predict values
MyData=data.frame(year=seq(from=(1958),to=2014, by=0.1))
G=predict(data.lm, newdata=MyData, type="response", se=T) #poission: type ="LINK", binomial: type="RESPONSE"


plot(year, co2, type="n",las=1, xlab="Year", ylab="CO2 conc. (ppm)", main="CO2 concentration in the atmosphere")
grid (NULL,NULL, lty = 6, col = "cornsilk2") 
points(year, co2, col="cornflowerblue" )
#go for confidence interval 
F=(G$fit)
FSUP=(G$fit+1.96*G$se.fit) # make upper conf. int. 
FSLOW=(G$fit-1.96*G$se.fit) # make lower conf. int. 
lines(MyData$year, F, lty=1, col="red")
#2.result: linear regression dont fit 


#check for data mistakes, ect. : visualize our data
x <- co2
op <- par(mfrow = c(1,2),
          mar = c(5,4,1,2)+.1,
          oma = c(0,0,2,0))
hist(x, freq=F,
     col = "cornsilk",
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
     col = "cornsilk",
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

#autocorrelation:
#what it should look like, if you dont have autocorr. problems:
par(mfrow=c(1,2))
n <- 200
x <- rnorm(n)
acf(x, main="ACF for white noise")
#lag 0 is always y=1, because the data is compared with the true data, which are equal

x <- co2
acf(x, main="ACF for our data")

#make a monthly time series:
ts.x=ts(x, 1958,2014,12 )
class(ts.x)
#decompose the time series into the trend, seasonal fluctiation and the random white noise
dec = decompose(ts.x)
plot(dec)

#now we can see the different parts of our future regression
par(mfrow=c(3,3))

acf(x)
pacf(x)
spectrum(x, main="")

#first diff. 
diff.x= diff(x, lag=12)
acf(diff.x)
pacf(diff.x)
spectrum(diff.x, main="")

#2nd differenciation
diff2.x = diff(diff.x)
acf(diff2.x)
pacf(diff2.x)
spectrum(diff2.x, main="")

par(mfrow=c(1,1))

#### and NOW??!?! zwei mal differenciate und alles passt aber wie gehts weiter? 


