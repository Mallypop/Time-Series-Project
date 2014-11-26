
##################

library(mgcv)
model = gam ( ts.x ~ s(time))
summary(model)
par(mfrow=c(1,1))
plot.gam(model, residuals=T, scheme=c(2,1), all.terms=T)

smoothed1.gam <- gamm(as.numeric(ts.x) ~ s(time) + I(time^2) +
                                          COS[,1]+SIN[,1]+COS[,2]+SIN[,2]+
                                          COS[,3]+SIN[,3]+COS[,4]+SIN[,4]+
                                          COS[,5]+SIN[,5]+COS[,6]+SIN[,6]
                     , cor=corARMA(p=2, q=2))

smoothed3.gam <- gamm(as.numeric(ts.x) ~ s(time)  +
                       COS[,1]+SIN[,1]+COS[,2]+SIN[,2]+
                       COS[,3]+SIN[,3]+COS[,4]+SIN[,4]+
                       COS[,5]+SIN[,5]+COS[,6]+SIN[,6]
                     , cor=corARMA(p=2, q=2))

smoothed4.gam <- gam(as.numeric(ts.x) ~ s(time)  +
                        COS[,1]+SIN[,1]+COS[,2]+SIN[,2]+
                        COS[,3]+SIN[,3]+COS[,4]+SIN[,4]+
                        COS[,5]+SIN[,5]+COS[,6]+SIN[,6]
                      , cor=corARMA(p=2, q=2))

smoothed4.gam <- gam(as.numeric(ts.x) ~ s(time, k=12)  +
                       COS[,1]+SIN[,1]+COS[,2]+SIN[,2]+
                       COS[,3]+SIN[,3]+COS[,4]+SIN[,4]+
                       COS[,5]+SIN[,5]+COS[,6]+SIN[,6]
                     , cor=corARMA(p=2, q=2))
plot.gam(smoothed.gam, residuals=T, scheme=c(2,1),all.terms=T)
summary(smoothed3.gam$lme)

anova(smoothed1.gam, smoothed2.gam, smoothed3.gam)
AIC(smoothed3.gam$lme)
smoothed3.gam
plot(smoothed4.gam)
AIC(smoothed4.gam)
############
ts.plot(Nile)
class(Nile)
timeNile = time(Nile)
timeNile
smoothed5.gam <- gam(as.numeric(Nile) ~ s(timeNile) 
                     , cor=corARMA(p=2, q=0))
plot(smoothed5.gam)
AIC(smoothed5.gam)