rm(list = ls())
setwd("/Users/keawejohnson/Analyzing Financial Data (Wang)/Project 1 Folder")
library(psych)
library(dplyr)
library(dynlm)
library(vars)
library(forecast)
library(lubridate)

project1 <- read.csv("project1.csv")

#Let's specify which Stock ticker we are using and refine the parameters
GOOGLE <- subset(project1, select = c(Date,SPY.Close,SPY.R,GOOG.Close,GOOG.R,
                                      RF,SMB,HML))
GOOGLE$Date.num <- as.Date(GOOGLE$Date, format = "%m/%d/%Y")

#Now we're going to plot the charts by setting the S&P closing and returns against the Google closing and returns to compare
par(mfrow = c(2,1))
plot(GOOGLE$SPY.Close ~ GOOGLE$Date.num, type = "l",xlab = "Date",ylab="SPY Closing Price",
     main = "SPY Closing Price")
plot(GOOGLE$SPY.R ~ GOOGLE$Date.num, type = "l", main = "SPY Daily Returns", xlab = "Date", ylab = "Returns")
par(mfrow = c(1,1))

par(mfrow = c(2,1))
plot(GOOGLE$GOOG.Close ~ GOOGLE$Date.num, type = "l",xlab = "Date",ylab="Google Closing Price",
     main = "Google Closing Price")
plot(GOOGLE$GOOG.R ~ GOOGLE$Date.num, type = "l", main = "Google Daily Returns", xlab = "Date", ylab = "Google")
par(mfrow = c(1,1))


#start section 2

#rollapplyr funtion to make moving average

mean(GOOGLE$GOOG.Close)
sd(GOOGLE$GOOG.Close)
mean(GOOGLE$GOOG.R)
sd(GOOGLE$GOOG.R)
mean(GOOGLE$SPY.Close)
sd(GOOGLE$SPY.Close)
mean(GOOGLE$SPY.R)
sd(GOOGLE$SPY.R)


#RISK FREE
subset <- subset(GOOGLE, select = c(SPY.R, GOOG.R, RF))
pairs.panels(subset)

SPY.SR <- GOOGLE$SPY.R-GOOGLE$RF
print(SPY.SR)

GOOG.SR <- GOOGLE$GOOG.R-GOOGLE$RF
print(GOOG.SR)


reg <- lm(GOOG.SR~SPY.SR)
summary(reg)
#LESS RISK
regmulti <- lm(GOOG.SR~SPY.SR+GOOGLE$SMB+GOOGLE$HML)
summary(regmulti)
#VALUE PREMIUM UNCORROLATED
#RGB LINES 14 day avg

GOOG.ma14 <- rollapplyr(GOOGLE$GOOG.Close, 14, mean, fill = NA)
GOOG.sd14 <- rollapplyr(GOOGLE$GOOG.Close, 14, FUN = sd, fill = NA)
GOOG.sd14u <- GOOG.ma14 + 2*GOOG.sd14
GOOG.sd14l <- GOOG.ma14 - 2*GOOG.sd14 

GOOG.plot <- GOOGLE$GOOG.Close
plot(GOOG.plot~GOOGLE$Date.num,pch=19,main="Google",type="l")
lines(GOOG.ma14~GOOGLE$Date.num, col = "blue", lwd = 3)
lines(GOOG.sd14u~GOOGLE$Date.num, col = "red")
lines(GOOG.sd14l~GOOGLE$Date.num, col = "green")




GOOGLE$SPY.ma14 <- rollapplyr(GOOGLE$SPY.Close, 14, mean, fill = NA)
GOOGLE$SPY.sd14 <- rollapplyr(GOOGLE$SPY.Close, 14, FUN = sd, fill = NA)
GOOGLE$SPY.sd14u <- GOOGLE$SPY.ma14 + 2*GOOGLE$SPY.sd14
GOOGLE$SPY.sd14l <- GOOGLE$SPY.ma14 - 2*GOOGLE$SPY.sd14

SPY.plot <- GOOGLE$SPY.Close
plot(SPY.plot~GOOGLE$Date.num,pch=19,main="SPY",type="l", ylim = c(400, 500))
lines(GOOGLE$SPY.ma14~GOOGLE$Date.num, col = "blue", lwd = 3)
lines(GOOGLE$SPY.sd14u~GOOGLE$Date.num, col = "red")
lines(GOOGLE$SPY.sd14l~GOOGLE$Date.num, col = "green")

#AUTOCORROLATIONS
#Autocorrelation of vanguard
acf(GOOGLE$GOOGLE.Close,main="Autocorrelation Function of Google.Close")
acf(GOOGLE$GOOGLE.R,main="Autocorrelation Function of Google.Returns")
#Autocorrelated up to 20 lags at 20%
#RETURNS NO AUTOCORROLATION
#Autocorrelation of SPY
acf(GOOGLE$SPY.Close,main="Autocorrelation Function of SPY.Close")
acf(GOOGLE$SPY.R,main="Autocorrelation Function of SPY.Returns")

#NATURAL LOG USE TS DATA 

GOOGLOG <- log(GOOGLE$GOOG.Close)

GOOGLOG.df <- ur.df(GOOGLOG, lags = 0, type = "drift")
summary(GOOGLOG.df)
# By the way, the AR(1) model can be estimated on TSLA close price as follows:
ar1Google <- dynlm(GOOGLOG ~ L(GOOGLOG,1) )
summary(ar1Google)

#START LOG

LOGspy <- log(GOOGLE$SPY.Close) 

LOGspy.df <- ur.df(LOGspy, lags = 0, type = "drift")
summary(LOGspy.df)
# By the way, the AR(1) model can be estimated on TSLA close price as follows:
ar1spy <- dynlm(LOGspy ~ L(LOGspy,1) )
summary(ar1spy)


#ARIMA

GOOGts <- ts(GOOGLE$GOOG.Close)
plot(GOOGts, xlab="MONTHS")
# Fitting and AR(4) model
ar4 <- dynlm(GOOGts ~ L(GOOGts,1) + L(GOOGts,2) + L(GOOGts,3) + L(GOOGts,4))
summary(ar4)
# Next, we can use VARselect() in the “vars” package
# "const" in VARselect denotes model with a constant(drift) only
lselect <- VARselect(GOOGts, type = "const", lag.max = 12)
print(lselect)
# Plot the AIC criteria for a better look at the optimal lag selection
plot(lselect$criteria[1,],type = "l",ylab="AIC", xlab = "Numb. of Lags")
# Augmented Dickey-Fuller for AR(4), note this has 3 first differenced lags!
ar4.df <- ur.df(GOOGts, lags = 3, type = "drift")
summary(ar4.df)
#ARIMA Autofit and Predictions using the “forecast” package for 16 quarters (4 years)
fit <- auto.arima(GOOGts)
plot(forecast(fit, h = 16))
# Let's do a similar thing for U.S. consumption
GOOG1ts <- ts(GOOGLE$GOOG.R, frequency = (12))
# Note here the fit is a seasonal ARIMA! Just remember that it is one ARIMA for the entire period,
# stacked on top of an ARIMA for each year (4 quarters each, so 16 quarters)
fit.i <- auto.arima(GOOG1ts)
plot(forecast(fit.i, h = 16))
# Alternatively, you can always force auto.arima to not fit seasonal ARIMA’s:


#ARIMA FOR SPY

SPYts <- ts(GOOGLE$SPY.Close, frequency = (12))
plot(GOOGts, xlab="MONTHS")
# Fitting and AR(4) model, COME BACK TO 154; NEED HELP
arS4 <- dynlm(GOOGts ~ L(SPYts,1) + L(SPYts,2) + L(SPYts,3) + L(SPYts,4))
summary(ar4)
# Next, we can use VARselect() in the “vars” package
# "const" in VARselect denotes model with a constant(drift) only
Yselect <- VARselect(SPYts, type = "const", lag.max = 12)
print(Yselect)
# Plot the AIC criteria for a better look at the optimal lag selection
plot(Yselect$criteria[1,],type = "l",ylab="AIC", xlab = "Numb. of Lags")
# Augmented Dickey-Fuller for AR(4), note this has 3 first differenced lags!
arS4.df <- ur.df(SPYts, lags = 3, type = "drift")
summary(arS4.df)
#ARIMA Autofit and Predictions using the “forecast” package for 16 quarters (4 years)
fit <- auto.arima(SPYts)
plot(forecast(fit, h = 16))
# Let's do a similar thing for U.S. consumption
SPY1ts <- ts(GOOGLE$SPY.R, frequency = (12))
# Note here the fit is a seasonal ARIMA! Just remember that it is one ARIMA for the entire period,
# stacked on top of an ARIMA for each year (4 quarters)
fit1.i <- auto.arima(SPY1ts)
plot(forecast(fit1.i, h = 16))
# Alternatively, you can always force auto.arima to not fit seasonal ARIMA’s:





