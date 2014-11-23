obj <- gam(co2 ~ s(year))
 plot(obj, shift=mean(data$co2), residuals=T, pch=1, xlab="")

# Fit an additive autoregressive model
# additive model fitting is outsourced to mgcv::gam, with splines
# Inputs: time series (x), order of autoregression (order)
# Output: fitted GAM object
aar <- function(ts,order) {
  stopifnot(require(mgcv))
  # Automatically generate a suitable data frame from the time series
  # and a formula to go along with it
  fit <- gam(as.formula(auto.formula(order)),
             data=design.matrix.from.ts(ts,order))
  return(fit)
}

aar(yourts,2)

# Generate formula for an autoregressive GAM of a specified order
# Input: order (integer)
# Output: a formula which looks like
# "lag0 ~ s(lag1) + s(lag2) + ... + s(lagorder)"
auto.formula <- function(order) {
  inputs <- paste("s(lag",1:order,")",sep="",collapse="+")
  form <- paste("lag0 ~ ",inputs)
  return(form)
}


##################


model = gam ( co2 ~ s(year))
summary(model)
plot.gam(model, residuals=T, scheme=c(2,1), all.terms=T)

smoothed.gam <- gam(as.numeric(yourts) ~ s(TIME) + s(I(TIME^2)) + V1 + V2 + V3 + V4 + V5 +V6 +V7+V8 +V9 +V10 +V11 +V12, corr=corAR1(acf(dataseason.gls$residuals)$acf[2]), data=time.df)

plot.gam(smoothed.gam, residuals=T, all.terms=T, pch=19)


model1 = gamm( co2 ~ s(year, k=5))
model2= gamm (co2 ~ s(year, k=5), correlation= corARMA ( form=~year,p=1) )
model3= gamm (co2 ~ s(year, k=5), correlation= corARMA ( form=~year,p=2) )
anova(model1$lme, model2$lme, model3$lme)
