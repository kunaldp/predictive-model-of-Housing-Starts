housing=read.csv("cansim-0270001-prep.csv")
names(housing)

library(lubridate)
housing$date_year=year(housing$Date)
housing$date_months=month(housing$Date)

head(housing[,1:5]);
housing$date_year[1:5]

#print the data types in dataframe
sapply(housing, typeof)

#create time series
thous_k.ts<- ts(housing, start=c(1948,1), end= c(2011,12), frequency=12)

#plot the seasonality graph using correllogram.

#Auto-Correlation Function
acf(thous_k.ts[,132], lag.max = 36)
x_k<-acf(thous_k.ts[,132], lag.max = 36); x_k$acf[1]<-NA
plot(x_k, main="ACF of Housing Starts in Toronto")

#Partial Auto-Correlation Function
acf(thous_k.ts[,132], lag.max = 36, type = "p", main="Partial ACF of Housing Starts of Toronto")

#linear model using 1 and 2 month lag

library(dyn)

mod1_k<-dyn$lm(TO.HS.To ~ lag(TO.HS.To,-1), data=thous_k.ts)
summary(mod1_k)

mod2_k<-dyn$lm(TO.HS.To ~ lag(TO.HS.To,-1) + lag(TO.HS.To,-2) + factor(date_year), data=thous_k.ts)
summary(mod2_k) 

mod3_k<-dyn$lm(TO.HS.To ~ lag(TO.HS.To,-1) + lag(TO.HS.To,-2) + factor(date_year) + 
               factor(date_months), data=thous_k.ts, na.action=na.omit)
summary(mod3_k)

library(stargazer)
stargazer(mod1, mod2, mod3, type="text", no.space=TRUE, align=TRUE,
          dep.var.labels=c("Lagged only","Lag+years", "Lag+years+months"))


#___________ADF tests 
#plotting the residuals and correlation
#this test determines the strength of the 
install.packages("tseries")
library(tseries)
library(urca)
library(dyn)
#mod 3
res.starts_k <-residuals.dyn(mod3_k,thous.ts)
plot(res.starts_k)

#mod2
res.starts_k_2 <-residuals.dyn(mod2_k,thous.ts)
plot(res.starts_k_2)

#adf test begins
adf.test(res.starts_k, k=12)
summary(ur.df(res.starts_k, type = "trend", lags = 12))

library(fUnitRoots)
#mod 3
urersTest(res.starts_k, type = c("DF-GLS", "P-test"), model = c("constant", "trend"),
          lag.max = 12, doplot = TRUE)

urersTest(res.starts_k, type = c("DF-GLS", "P-test"), model = c("constant"),
          lag.max = 12, doplot = TRUE)

urersTest(res.starts_k, type = c("DF-GLS", "P-test"), model = c("constant", "trend"),
          lag.max = 12)

#mod 2
urersTest(res.starts_k_2, type = c("DF-GLS", "P-test"), model = c("constant", "trend"),
          lag.max = 12, doplot = TRUE)
#-------------------------------------------------------------------------
head(thous_k.ts[, 1:5]); tail(thous_k.ts[, 1:5])

names(as.data.frame((thous_k.ts)))

names(as.data.frame((thous.ts)))

pred_k.starts <-fitted.dyn(mod3_k,thous_k.ts)
starts_k <- thous_k.ts[,132]
OLS_pred_k<-cbind(starts_k,pred_k.starts)

ts.plot(OLS_pred_k, lty=c(1,2), xlab="")
title(main="Predicted Versus Actual Starts", ylab="starts",
      sub = "Comparing actual and forecasted housing starts")
abline(h=c(seq(1000,5000, by=1000)))
legend(1993,5500, c("starts", "forecast"),
       lty = c(1,3))

#var model- Ignore, does not capture the non linearity.

library(vars)
vardata_k<-window(thous_k.ts[,c(132,134)], start=c(1967,1), end=c(2011,12))
thous_k.ts[,c(132,134)]
VARselect(vardata_k, lag.max = 8, type = "both")
varmod_k <- VAR(vardata_k, p = 3, type = "both")
varmod <- VAR(vardata, p = 3)
summary(varmod)

#test on existing data from Jan 2012 to Nov 2016

pred_k.var<-fitted(varmod_k, data=thous_k.ts)
p.var1_k<-predict(varmod_k,n.ahead=12)
p.var1_k

names(p.var1_k)

p.var2_k<-(p.var1_k$fcst$TO.HS.To[,1])
p.var2_k

starts_k <-window(thous.ts[,c(132)], start=c(2012,1))
as.numeric(starts_k)

pred.starts2_k <-window(pred_k.starts, start=c(2012,1))
pred.starts2_k

var_forcast_k<-cbind(starts=starts_k,f.var=p.var2_k, pred.starts2_k)
var_forcast_k

#
# # Arima Models______________________________________________
# just the test. Cleaned version is coming.
library(stargazer)
arma100_k <- arima(housing$TO.HS.To, order = c(1, 0, 0))
arma101_k <- arima(housing$TO.HS.To, order = c(1, 0, 1))
arma200_k <- arima(housing$TO.HS.To, order = c(2, 0, 0))
arma201_k <- arima(housing$TO.HS.To, order = c(2, 0, 1))

stargazer(arma100_k, arma101_k, arma200_k, arma201_k, type="text", align=TRUE)

ts.plot(housing[,132], col="red", xlab="", ylab="")
lines(housing$TO.HS.To-arma100_k$residuals, col="blue")
lines(housing$TO.HS.To-arma101_k$residuals, col="green")
lines(housing$TO.HS.To-arma200_k$residuals, col="pink")
lines(housing$TO.HS.To-arma201_k$residuals, col="dark gray")

title(main="Predicted Versus Actual Starts", ylab="Actual & Predicted Starts",
      xlab="Time index",
      sub = "Predicted and actual starts -- ARIMA models")
abline(h=c(seq(1000,5000, by=1000)))

legend(75,5500, c("starts","arma100", "arma101", "arma200", "arma201"), lty=c(1),
       col= c("red","blue", "green", "pink", "dark gray"))

# Cleaned version++++++++++++++++++++++++

ts_arma_k<- ts(housing[,132], start=c(1948,1), end= c(2016,11), frequency=12)
ts.plot(ts_arma_k)

# Final version of ARIMA models
ts.plot(ts_arma_k, col="red", ylab="",xlab="")
#ts.plot(ts(housing$TO.HS.To-arma100_k$residuals, start=c(1967,1), end= c(2018,12), frequency=12),col="blue")
#lines(ts_arma_k, col="red", ylab="",xlab="")
lines(ts(housing$TO.HS.To-arma100_k$residuals, start=c(1948,1), end= c(2018,12), frequency=12),col="blue")
lines(ts(housing$TO.HS.To-arma101_k$residuals, start=c(1948,1), end= c(2018,12), frequency=12),col="green")
lines(ts(housing$TO.HS.To-arma200_k$residuals, start=c(1948,1), end= c(2018,12), frequency=12),col="pink")
lines(ts(housing$TO.HS.To-arma201_k$residuals, start=c(1948,1), end= c(2018,12), frequency=12),col="dark gray")

title(main="Predicted Versus Actual Starts", ylab="Actual & Predicted Starts", ylim=c(0,10000),
      xlab="Years",
      sub = "Predicted and actual starts -- ARIMA models")
#abline(h=c(seq(1000,5000, by=1000)))
legend(1994,4500, c("starts","arma100", "arma101", "arma200", "arma201"), lty=c(1),
       col= c("red","blue", "green", "pink", "dark gray"), cex=.8)

#  Test data and prediction
wts_arma_k <-window(ts_arma_k, start=c(2005,1), end=c(2016,11))
warma100_k <- window(ts((2*housing$TO.HS.To-arma100_k$residuals), start=c(2005,1), end= c(2018,12), frequency=12))
                
warma101_k <- window(ts((2*housing$TO.HS.To-arma101_k$residuals), start=c(2005,1), end= c(2018,12), frequency=12))
                   #start=c(2012,1), end=c(2018,12))
warma200_k <- window(ts((2*housing$TO.HS.To-arma200_k$residuals), start=c(2005,1), end= c(2018,12), frequency=12))
                   #start=c(2012,1), end=c(2018,12))
warma201_k <- window(ts((2*housing$TO.HS.To-arma201_k$residuals), start=c(2005,1), end= c(2018,12), frequency=12))
                   #start=c(2012,1), end=c(2018,12))

#ts.plot(wts_arma_k, col="red", ylab="",xlab="",lty=1)
ts.plot(warma100_k, col="blue", ylab="",xlab="",ylim=c(0,7000), lty=1)
lines(wts_arma_k,col="red", lty=2)
lines(warma101_k,col="dark green",lty=3)
lines(warma200_k,col="pink",lty=4)
lines(warma201_k,col="dark gray",lty=5)

title(main="Predicted Versus Actual Starts", ylab="Actual & Predicted Starts",
      xlab="Years",
      sub = "Predicted and actual starts -- ARIMA models, 2005.01 - 2018.12")
#abline(h=c(seq(1000,4000, by=1000)))
legend(2000.7,2800, c("starts","arma100", "arma101", "arma200", "arma201"), lty=c(1:5),
       col= c("red","blue", "dark green", "pink", "dark gray"), cex=0.8)

#Prediction upto Dec 2018

wts_arma_k_p <-window(ts_arma_k, start=c(2015,1), end=c(2016,11))
warma100_k_p <- window(ts((4*housing$TO.HS.To-arma100_k$residuals), start=c(2015,1), end= c(2018,12), frequency=12))

warma101_k_p <- window(ts((4*housing$TO.HS.To-arma101_k$residuals), start=c(2015,1), end= c(2018,12), frequency=12))
#start=c(2012,1), end=c(2018,12))
warma200_k_p <- window(ts((4*housing$TO.HS.To-arma200_k$residuals), start=c(2015,1), end= c(2018,12), frequency=12))
#start=c(2012,1), end=c(2018,12))
warma201_k_p <- window(ts((4*housing$TO.HS.To-arma201_k$residuals), start=c(2015,1), end= c(2018,12), frequency=12))
#start=c(2012,1), end=c(2018,12))

#ts.plot(wts_arma_k, col="red", ylab="",xlab="",lty=1)
ts.plot(warma100_k_p, col="blue", ylab="",xlab="",ylim=c(0,10000), lty=1)
lines(wts_arma_k_p,col="red", lty=2)
lines(warma101_k_p,col="dark green",lty=3)
lines(warma200_k_p,col="pink",lty=4)
lines(warma201_k_p,col="dark gray",lty=5)

title(main="Predicted Versus Actual Starts", ylab="Actual & Predicted Starts",
      xlab="Years",
      sub = "Predicted and actual starts -- ARIMA models, 2015.01 - 2018.12")
#abline(h=c(seq(1000,4000, by=1000)))
legend(2000.7,2800, c("starts","arma100", "arma101", "arma200", "arma201"), lty=c(1:5),
       col= c("red","blue", "dark green", "pink", "dark gray"), cex=0.8)

#--------------------------------------------------------------------------------------------
