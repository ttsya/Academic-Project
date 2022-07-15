setwd("C:/R data")
source("EACF.R")
library(ggplot2)
library(tidyr)
library(dplyr)
library(forecast)
library(DMwR2)
library(zoo)
library(tsutils)
library(tseries)
library(depmixS4)
library(quantmod)
library(lubridate)

#data preparation
data(sp500)  
c=sp500$Close
# convert numeric into ts 
ts= ts(as.numeric(c),frequency = 12, start=decimal_date(ymd("1950-02-28")))
head(ts)

ts%>%ggtsdisplay(xlab="monthly",main="SP500")
library(urca)
diff= diff(ts)
diff%>%ggtsdisplay(xlab="month",main="sp500")

adf.test(ts)
## Q1
library(forecast)
# box cox transformation, first differencing
best_lambda=BoxCox.lambda(ts)
BoxCox(ts, lambda = best_lambda)-> ts.l 
diff.l= diff(ts.l)
diff.l%>%ggtsdisplay(xlab="month",main="sp500")
stlFit = stl(ts.l, s.window = "periodic")
autoplot(stlFit)+xlab("month")

# function to calculate RMSE
getrmse.arima= function(x=ts,h=24,...)
{
  train.end =time(x)[length(x)-h]
  test.start = time(x)[length(x)-h+1]
  train = window(x,end=train.end)
  test = window(x,start=test.start)
  fit = Arima(train,...)
  fc = forecast(fit,h=h)
  return(accuracy(fc,test)[2,"RMSE"])
  
}

getrmse.ets=function(x=ts,h=24,...)
{
  train.end = time(x)[length(x)-h]
  test.start = time(x)[length(x)-h+1]
  train = window(x,end=train.end)
  test = window(x,start=test.start)
  fit = ets(train,...)
  fc = forecast(fit,h=h)
  return(accuracy(fc,test)[2,"RMSE"])
}



## arima
arima.auto=auto.arima(ts)
arima.auto
source("EACF.R")
EACF(diff.l,p=12,q=12)
arima1= Arima(ts, order=c(0,1,0), seasonal=c(1,0,1), lambda= 0,include.constant = TRUE)
arima1
r1=getrmse.arima(ts,order=c(1,1,1), seasonal=c(0,0,1), include.constant = TRUE)
r2= getrmse.arima(ts,order=c(0,1,0), seasonal=c(1,0,1), lambda= 0,include.constant = TRUE)

Model= c("Auto","Manual")
rmse= round(c(r1,r2),2)
cbind(Model,rmse)%>%as.data.frame()
AIC=round(c(arima.auto$aic,arima1$aic),2)
cbind(Model,AIC)%>%as.data.frame()

# ets
#additive error, additive trend and additive seasonality
ets1=ets(ts, model="AAA", lambda = 0, damped = T)

E1= getrmse.ets(ts, model="AAA", lambda = 0, damped = T) 


###neutral network
train.end = time(ts)[length(ts)-24]
test.start = time(ts)[length(ts)-24+1]
train = window(ts,end=train.end)
test = window(ts,start=test.start)

fit1<- nnetar(train,lambda =best_lambda,k=1)
fc = forecast(fit1,h=24)
k1= accuracy(fc,test)[2,"RMSE"]

fit2= nnetar(train,lambda = best_lambda,k=2)
fc = forecast(fit2,h=24)
k2= accuracy(fc,test)[2,"RMSE"]

fit3= nnetar(train,lambda = best_lambda,k=3)
fc = forecast(fit3,h=24)
k3= accuracy(fc,test)[2,"RMSE"]

fit4= nnetar(train,lambda = best_lambda,k=4)
fc = forecast(fit4,h=24)
k4= accuracy(fc,test)[2,"RMSE"] 

RMSE= round(c(k1,k2,k3,k4),1)
K= round(c(1,2,3,4),2)
cbind(K,RMSE)%>% as.data.frame()

autoplot(forecast(fit,h=24)) 
nnetar(y=train, lambda = 0,k=2)

#model selection
RMSE= round(c(r2,E1,k1),2)
Model= c("ARIMA", "ETS", "NNETAR")
cbind(Model,RMSE)%>%as.data.frame()


checkresiduals(arima1)
Box.test(arima1$residuals,lag=12,type= "Ljung")

true= c(1365.68	,1408.47	,1397.91	,1310.33	,1362.16	,1379.32	,1406.58	, 1440.67	, 1412.16	,1416.18, 1426.19	)
true= ts(true, start= 2012.159, frequency=12)
f1= forecast(arima1,h=11)
autoplot(f1)+autolayer(true)
