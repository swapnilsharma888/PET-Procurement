setwd("C:\\Users\\SWAPNIL\\Desktop\\abiprocurementhackathonproblemstatement")
library(dplyr)
demand<-read.csv("Dataset 1.csv",header = T, na.strings = c("","-","NA"))
pet<-read.csv("Dataset 2.csv",header = T, na.strings = c("","-","NA"))
demand<-demand[-c(8:20)]
demand<-demand[c(1:2036),]
summary(demand)
summary(pet)
pet$sno<-c(1:221)
pet<-arrange(pet,-sno)
ts_pet<-pet[,2]
ts_pet<-ts(ts_pet,start = c(2000,1), end = c(2018,5), frequency = 12)
plot.ts(ts_pet)
boxplot(ts_pet ~ cycle(ts_pet))
dec_pet<-decompose(ts_pet)
plot(dec_pet)
unique(demand$Country)
library(tseries)
#petdiff1<-diff(ts_pet,differences = 1)
#plot(petdiff1)
#adf.test(petdiff1)




#Acf(diff(petdiff1,lag.max=20))
#pacf(diff(petdiff1,lag.max=20))
# P =0
# q =1

library(forecast)
#arima_ts_pet<-arima(ts_pet,order=c(0,1,1))
#pet_forecast<-forecast(arima_ts_pet,h=19)
#plot(pet_forecast)
#pet_forecast

#checkresiduals(pet_forecast)
#acf(pet_forecast$residuals)
#summary(pet_forecast)

#demand$PET_Price<-1179.133
ts_pet1<-window(ts_pet,2009)

holt_pet<- HoltWinters(ts_pet1)
plot(holt_pet)
forecast_pet <- forecast(holt_pet, h=19)
plot(forecast_pet)
Box.test(forecast_pet$residuals, lag=20,type="Ljung-Box")
checkresiduals(forecast_pet)
acf(forecast_pet$residuals)
summary(forecast_pet)

library(lubridate)
data.class(demand$ï..Date_requirement)
demand$ï..Date_requirement<-dmy(demand$ï..Date_requirement)
demand$month<-month(demand$ï..Date_requirement)

a<-as.data.frame(forecast_pet)
b<-a[-c(2:5)]
b<-b[-c(1:7),]
b<-as.data.frame(b)
b$month<-c(1:12)
forecast<-b
demand_pet<-merge(x=demand,y=forecast,by="month",all.x = T)

write.csv(demand_pet,"demand_pet.csv")