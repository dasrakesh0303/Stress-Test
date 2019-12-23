macro <- read.csv('SupervisorySeverelyAdverseDomestic.csv')
library(readxl)
bankincome <- read_xlsx('Income_Hist_Data.xlsx')
citiindex <- which(bankincome$institution.name == 'Citigroup Inc.')
citidata <- bankincome[citiindex,]
citidata <- ts(citidata[,3:9],frequency = 4,start = c(1999,1))

library("dynlm")
library("forecast")
library("tseries")
library("quantmod")
library("stats")
library("urca")
source(file="intord.R")
plot.ts(citidata)

macro <- ts(macro[,-1],frequency = 4,start = c(1976,1))
tail(macro)
intord(macro[,1]) #real gdp growth stationary
realgdpgrowth <- macro[,1]
intord(macro[,2]) # nominal gdp growth stationary
nominalgdpgrowth <- macro[,2]
intord(macro[,3]) # real disposable income growth stationary
realdisposableincomegrowth <- macro[,3]
intord(macro[,4]) #nominal disposable income growth stationary
nominaldisposableincomegrowth <- macro[,4]
intord(macro[,5]) # SD for level data of unemployment rate drops more than half
unemploymentratediff<- diff(macro[,5])

intord(macro[,6]) #cpi inflation rate is stationary
inflationrate<- macro[,6]
intord(macro[,7]) #3 month treasury nonstationary
x3monthtreasurydiff <- diff(macro[,7])
intord(macro[,8]) #5yr treasury rate nonstationary
x5yrtreasurydiff <- diff(macro[,8])
intord(macro[,9]) #10yr treasury rate nonstationary
x10yrtreasurydiff <- diff(macro[,9])
intord(macro[,10]) #BBBcorp nonstationary
bbbdiff<- diff(macro[,10])
intord(macro[,11]) #mortgage rate nonstationary
mortgagediff <- diff(macro[,11])

intord(macro[,12]) #prime rate non stationary
primediff <- diff(macro[,12])

dowjones <- macro[,13]
dowjones <- na.omit(dowjones)

intord(dowjones)
dowjonesdiff<- diff(dowjones)

intord(macro[,14]) #house price index non stationary
housepricediff <- diff(macro[,14])
intord(macro[,15]) #commercial real estate non stationary
realestatediff<- diff(macro[,15])

volatilityindex <- macro[,16]
volatilityindex <- na.omit(volatilityindex)
intord(volatilityindex) #volatility stationary

citiinterestincome <- citidata[,1]
intord(citiinterestincome) #nonstationary
citiinterestincomediff <- diff(citiinterestincome)
citiinterestexpense <- citidata[,2]
intord(citiinterestexpense) #nonstationary
citiinterestexpensediff <- diff(citiinterestexpense)
citinoninterestincome <- citidata[,3] #stationary
intord(citinoninterestincome)
citinoninterestexpense <- citidata[,4] #stationary
intord(citinoninterestexpense)
citiprovisionlosses <- citidata[,5] #nonstationary
intord(citiprovisionlosses)
citiprovisionlossesdiff <- diff(citiprovisionlosses)



library(dplyr)
Interestincomedataset <- cbind(realgdpgrowth, realdisposableincomegrowth, inflationrate, unemploymentratediff, x3monthtreasurydiff, x5yrtreasurydiff,
          x10yrtreasurydiff, bbbdiff, mortgagediff, primediff, housepricediff,
          realestatediff, dowjonesdiff, volatilityindex, citiinterestincomediff)
trainsetintincome <- as.data.frame(window(Interestincomedataset,c(1999,1), c(2014,4)))
testsetintincome <- as.data.frame(window(Interestincomedataset,2015, c(2015,4)))

library(caret)


intincome <- dynlm(citiinterestincomediff~., data = trainsetintincome)
summary(intincome)
intincome1<- dynlm(citiinterestincomediff~ mortgagediff + housepricediff + realgdpgrowth , data = trainsetintincome)
summary(intincome1)
intincome2<- dynlm(citiinterestincomediff ~ housepricediff + realgdpgrowth , data = trainsetintincome )
summary(intincome2)
intincome3<- dynlm(citiinterestincomediff ~ housepricediff , data = trainsetintincome )
summary(intincome3)

forecastintincome <- forecast(intincome3, testsetintincome )
autoplot(forecastintincome)

cbind(forecastintincome, testsetintincome$citiinterestincomediff)
