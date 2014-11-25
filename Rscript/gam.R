
##################

library(mgcv)
model = gam ( ts.x ~ s(time))
summary(model)
par(mfrow=c(1,1))
plot.gam(model, residuals=T, scheme=c(2,1), all.terms=T)

smoothed1.gam <- gam(as.numeric(ts.x) ~ s(time) + I(time^2) +
                                          COS[,1]+SIN[,1]+COS[,2]+SIN[,2]+
                                          COS[,3]+SIN[,3]+COS[,4]+SIN[,4]+
                                          COS[,5]+SIN[,5]+COS[,6]+SIN[,6]
                     , cor=corARMA(p=2, q=2))

smoothed3.gam <- gam(as.numeric(ts.x) ~ s(time) + s(I(time^2)) +
                       COS[,1]+SIN[,1]+COS[,2]+SIN[,2]+
                       COS[,3]+SIN[,3]+COS[,4]+SIN[,4]+
                       COS[,5]+SIN[,5]+COS[,6]+SIN[,6]
                     , cor=corARMA(p=2, q=2))

plot.gam(smoothed.gam, residuals=T, scheme=c(2,1),all.terms=T)
AIC(smoothed3.gam)

anova(smoothed1.gam, smoothed2.gam, smoothed3.gam)


