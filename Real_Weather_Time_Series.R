library(astsa)
library(dplyr)
#setwd("C:/Users/nbafl/OneDrive/gitstuff/HackDavis20")

weather = read.csv("davis010117011720weather.csv", header = TRUE)
head(weather)
#created new column
weather1 <- weather %>% mutate(
  avg_temp = (Air.max + min)/2
)
#pointed to new dataframe with the additional new column
weather1 <- weather1 %>% select_if(~is.numeric(.))
weather1 = weather1[-dim(weather1)[1], ]

#plugged in my guy
tsplot(weather1[,10], main = "Daily avg temperature Jan 1,2017 - Jan 17,2020", ylab = "Xt")
tsplot(diff(weather1[,10]), main = "Daily avg temperature Jan 1,2017 - Jan 17,2020", ylab = expression(nabla~X[~t]))

#checking normality
qqnorm(weather1[,10])
qqline(weather1[,10], col = 2)
qqnorm(diff(weather1[,10]))
qqline(diff(weather1[,10]), col = 2)

#sample acf and pacf for avg temperature
par(mfrow = c(2,1))
acf(diff(weather1[,10]))
pacf(diff(weather1[,10]))
par(mfrow = c(1,1))
#acf cuts off at lag 4, pacf tails off, which suggests a ARMA(0,4) model

#check possible models
m1 = sarima(weather1[,10], p=0, d=1, q=4)
m2 = sarima(weather1[,10], p=1, d=1, q=4)
m3 = sarima(weather1[,10], p=2, d=1, q=4)
#ACF of residuals for ARMA(2,4) is mostly within confidence levels and p-values for Ljung-Box statistic is 
#all over 0.05 confidence level, so it seems to fit the model the best.

#model selection
c(m1$AIC, m1$AICc, m1$BIC)
c(m2$AIC, m2$AICc, m2$BIC)
c(m3$AIC, m3$AICc, m3$BIC)
#Comparing AIC between ARMA(2,4) with ARMA(1,4) and ARMA(0,4), ARMA(2,4) has the lower AIC, so it indeed
#is the best model

#forcasting next 5 avg temperature
s1 = sarima.for(weather1[,10], n.ahead = 5, p=2, d=1, q=4)
s1$pred
