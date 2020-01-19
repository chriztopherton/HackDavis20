library(astsa)
library(dplyr)

davisweather$avg.temp =  (davisweather[,6] + davisweather[,7])/2

weather = read.csv("davisweather.csv", header = TRUE)
avg.temp = (weather[,6] + weather[,7])/2 
#avg temperature found by averaging the max and min temp for each day

tsplot(weather[,6], main = "Daily avg temperature Jan 1,2017 - Jan 17,2020", ylab = "Xt")
tsplot(diff(weather[,6]), main = "Daily max temperature", ylab = expression(nabla~X[~t]))

qqnorm(weather[,6])
qqline(weather[,6], col = 2)
qqnorm(diff(weather[,6]))
qqline(diff(weather[,6]), col = 2)

par(mfrow = c(2,1))
acf(diff(weather))



library(astsa)
library(dplyr)

weather = read.csv("davisweather.csv", header = TRUE)
tsplot(weather[,6], main = "Daily avg temperature Jan 1,2017 - Jan 17,2020", ylab = "Xt")

AvgTmp = (weather[, 6] + weather[, 7])/2
AvgTmp_df = data.frame(AvgTmp)
weather = cbind(weather, AvgTmp_df)

weather['AvgTmp'] = AvgTmp

f
tsplot(weather['AvgTmp'])

typeof(weather['AvgTmp'][1])
typeof(weather[, 6][1])
typeof(weather[, 7][1])


typeof((weather[, 6] + weather[, 7])/2)
