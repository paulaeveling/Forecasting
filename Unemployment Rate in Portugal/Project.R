# UNEMPLOYMENT RATE & ECONOMIC PERSPECTIVE TIME SERIES ANALYSIS

# Import libraries that will be used in this project
#install.packages("portes")
library(tseries)
library(forecast)
library(fma)
library(ggplot2)
library(FitAR)
require(astsa)
library(nortest)
library(portes)
library(lattice)
library(tsutils)
library(TSA)

# Import dataset
library(readxl)
dataset <- read_excel("/Users/paulaeveling/Desktop/TS-Project/dataset.xlsx")

# Format dataset as time series
ds <- ts(dataset, start=c(1999,1), frequency=12)
ds

# Statistical summary of the dataset
summary(ds)

# Based on the statistical summary of the dataset, it can be observed that the average unemployment rate between 1999 and 2021 is 9.74%, 
# while the minimum observed was 4.9% and the maximum 18.65%.

# Time Series Initial Analysis
tsdisplay(ds, main = "Unemployment Rate in Portugal from 1999 until 2021")
plot(ds, main = "Unemployment Rate in Portugal from 1999 until 2021")

# Trend: The dataset presents an upward trend until 2013, when the unemployment rate drops.
# Seasonality: It is not clear in the charts if this is a seasonal time series.
# Cycle: There is no clear cicles in the dataset.
# Variance: The variance is not constant.
# Stationarity: The dataset does not present characteristics of a stationary time series.

# Time series decomposition
decompose(ds)
plot(decompose(ds))

# Seasonal Analysis
seasplot(ds, outplot=2)

# Statistical Test:
# H0: no seasonality
# H1: there is seasonality
# Since the p-value is lower than 0.05, there is enough evidence to say that this time series has seasonality.
# It can be observed on the chart that the dataset under study is seasonal because there are boxes below the line, confirming the statistical test presented on the last step.

# Seasonal Analysis
seasplot(ds, outplot=3)

# Statistical Test:
# H0: no seasonality
# H1: there is seasonality
# Since the p-value is lower than 0.05, there is enough evidence to say that this time series has seasonality.
# It can be observed on the chart that the dataset under study is seasonal because there are boxes below the line, confirming the statistical test presented on the last step.

# Stationarity Ttest

# Dickey-Fuller
adf.test(ds)
# KPSS
kpss.test(ds)
# PP
PP.test(ds) 

# The Dickey-Fuller test presented a p-value higher than 0.1, so we fail to reject the null hypothesis. This is a non-stationary time series.

# Stationarity test with one order differencing
adf.test(diff(ds))

# plot the timeseries with one differencing order
tsdisplay(diff(ds), main = "Unemployment Rate in Portugal from 1999 until 2021")

# After applying one order differencing, the p-value presented by the Dickey-Fuller test is lower than 0.01, so we do reject the null hypothesis.
# There is strong evidence that the time-series is stationary.
# It can be observed a strong seasonality in the time-series that must be treated accordingly.

# Variance treatment using lambda
BoxCox(ds, interval=c(-4,4))
lambda.est<-0.449

# Tratamento da Time Series Using 1 stationary differencing + 1 seasonal differencing + BoxCox
ds_treated<-ts(diff(diff(ds^lambda.est, lag=12, differences = 1)), frequency=12)
tsdisplay(ds_treated, lag=12, differences=1)

# Auto ARIMA Fit = ARIMA(1,2,0)(2,0,0)
fit.auto<-auto.arima(ds, stationary=FALSE, lambda=lambda.est, seasonal = TRUE)
summary(fit.auto)
tsdisplay(fit.auto$residuals)

# Fit 1 = ARIMA(1,2,0)(2,0,0)
fit1<- Arima(ds, order=c(1,2,0), seas=list(order=c(2,0,0), period=12),
             include.drift=TRUE, lambda=lambda.est)
summary(fit1)
tsdisplay(fit1$residuals)

# Fit 2 = ARIMA(1,1,0)(2,1,0)
fit2<- Arima(ds, order=c(1,1,0), seas=list(order=c(2,1,0), period=12),
             include.drift=TRUE, lambda=lambda.est)
summary(fit2)
tsdisplay(fit2$residuals)

# Fit 3 = ARIMA(1,2,1)(1,1,1)
fit3<- Arima(ds, order=c(1,2,1), seas=list(order=c(1,1,1), period=12),
             include.drift=TRUE, lambda=lambda.est)
summary(fit3)
tsdisplay(fit3$residuals)

# Test for normality
lillie.test(fit2$residuals) # white noise residuals
t.test(fit2$residuals)
LjungBox(fit2$residuals, lags=seq(4,30,2), order=3, season=1, squared.residuals=FALSE)
LiMcLeod(fit2$residuals, lags=seq(4,30,2), order=3, season=1, squared.residuals=FALSE)
BoxPierce(fit2$residuals,lags=seq(4,30,2),season=1)

# Plot adjusted model
plot(ds)
lines(fitted(fit2), col="red")

# 24-month forecasting
forecast(fit2,h=24) 
plot(forecast(fit2,h=24))
lines(fitted(fit2),col=2)


# CROSSCORRELATION (COVARIANCE) FUNCTION

# importing dataset
perspectiva <- read_excel("/Users/paulaeveling/Desktop/TS-Project/two_datasets.xlsx")

# transforming dataset into ts format
datasets <- ts(perspectiva, start=c(1999,1), frequency=12)
unemployment <- ts(perspectiva$unemployment, start=c(1999,1), frequency=12)
perspective <- ts(perspectiva$economic_perspective, start=c(1999,1), frequency=12)

# plot two time series
xyplot(datasets)
cor(datasets)
plot(decompose(perspective))

length(unemployment)
length(perspective)

# Both datasets have the same lenght, which is good.
# Bots time series must be treated accordingly. The unemployment one needs one box cox transformation to treat the variance, 
# one differencing to de-trend the series and one seasonal differencing to handle seasonality.
# The economic perspective time series on the other hand, only needs one differencing to transform the series into stationary.

ccf(x=perspective,y=unemployment)

tsdisplay(perspective)
# It approaches zero very slowly.

# Treat both datasets
treatedunemployment<-ts(diff(diff(unemployment^lambda.est, lag=12, differences = 1)), frequency=12)
treatedperspectiva<-ts(diff(perspective),frequency=12)

length(treatedunemployment)
length(treatedperspectiva)
# Because they both needed different treatments, we have lost 12 observations in the unemployment dataset and only one in the economic perspective.

treatedunemployment
treatedperspectiva

# Since both time series have different sizes, it is necessary to remove observations from the economic perspective so they both have the same size.
treatedperspectiva2<-ts((treatedperspectiva[c(-1,-2,-3,-4,-5,-6,-7,-8,-9,-10,-11,-12)]), frequency=12)
treatedunemployment
treatedperspectiva2

print(ccf(x=treatedperspectiva2,y=treatedunemployment))
abline(v=0, lty = 2)

# We want to explain the variable unemployment using the variable economic perspective.
# No clear leading or lagging pattern! And also it is clear that both series have trend. Both are non-stationary.

lag2.plot(treatedperspectiva2, treatedunemployment, 10)

perspective_cbind3<-cbind(xlag3=lag(perspective,-1))
fit.regauto1<- auto.arima(unemployment, xreg=perspective_cbind3) # modelo automático.
fit.regauto1

perspective_cbind2<-cbind(xlag3=lag(perspective,-2))
fit.regauto1<- auto.arima(unemployment, xreg=perspective_cbind2,d=1) # modelo automático.
fit.regauto1

length(xreg_perspet)
length(unemployment)

perspective_cbind2
perspective_cbind3

fit.regauto1<- auto.arima(unemployment, xreg=xreg_perspet) # modelo automático.
fit.regauto1

treatedperspectiva23<-cbind(xlag6=lag(treatedperspectiva2,-3))
auto.arima(treatedunemployment,xreg=treatedperspectiva23,d=0)

# Working with the differenced time series
tsdisplay(auto.arima(treatedunemployment,xreg=treatedperspectiva23))
tsdisplay(auto.arima(treatedunemployment,xreg=treatedperspectiva23)$residuals)
tsdisplay(auto.arima(treatedunemployment,xreg=treatedperspectiva23))

arimax(treatedunemployment,order=c(1,0,1),xreg=treatedperspectiva23)


# PERIODOGRAM

# importing dataset
#perspectiva <- read_excel("/Users/paulaeveling/Desktop/TS-Project/perspetiva.xlsx")

# importing dataset
perspectiva <- read_excel("/Users/paulaeveling/Desktop/TS-Project/two_datasets.xlsx")

# transforming dataset into ts format
datasets <- ts(perspectiva, start=c(1999,1), frequency=12)
unemployment <- ts(perspectiva$unemployment, start=c(1999,1), frequency=12)
perspective <- ts(perspectiva$economic_perspective, start=c(1999,1), frequency=12)

treatedunemployment<-ts(diff(diff(unemployment^lambda.est, lag=12, differences = 1)), frequency=12)
treatedperspectiva<-ts(diff(perspective),frequency=12)

require(TSA)
y <- treatedunemployment
length(treatedunemployment)
new = periodogram(y)

new$freq[36]

(1/new$freq[36])
(1/new$freq[36])/12 # the harmonic frequencies

treatedperspectiva2<-ts((treatedperspectiva[c(-1,-2,-3,-4,-5,-6,-7,-8,-9,-10,-11,-12)]), frequency=12)

x<-treatedperspectiva2 
length(treatedperspectiva)
new = periodogram(x)

order(new$spec) # approximate the Fourir frequencies

new$freq[67]
(1/new$freq[67])
(1/new$freq[67])/12
