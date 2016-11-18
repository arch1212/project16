#--------------------Importing the data
library(xlsx)
opm <- read.xlsx("UK Outward Passengers Movement.xls", sheetIndex = 1, startRow = 6)

#--------------------Preparing the data
colnames(opm) <- c("Year", "Quarter", "Ireland", "Other EU not Ireland", "Rest of the World and Med",
                   "Rest of the World", "Total")

library(dplyr)
opm <- opm %>% mutate(Quarter = paste(Year, Quarter, sep = "-")) %>% select(Quarter, Total)

opm <- opm[complete.cases(opm),]

#---------------------Time Series Data
opm.ts <- ts(opm$Total, start = c(1996,1), end = c(2005,4), frequency = 4)
opm.ts

plot(opm.ts)

#--------------------Multiplicative Seasonal Decomposition 
fit1 <- decompose(opm.ts, type = c("multiplicative"))
fit1
plot(fit1)

library(forecast)
forecast(fit1$trend, 4)
accuracy(fit1$x, opm.ts)

#--------------------Smoothening techniques

# Single Smoothening
fit2 <- HoltWinters(opm.ts, beta=FALSE, gamma=FALSE)
accuracy(fit2$fitted, opm.ts)
# predict next 4 observations
forecast(fit2, 4)
# MAPE = 18.11
# no seasonality component in the predictions

# Double Smoothening
fit3 <- HoltWinters(opm.ts, gamma=FALSE)
accuracy(fit3$fitted, opm.ts)
# predict next 4 observations
forecast(fit3, 4)
# MAPE = 23.51, it has gone up
# very little seasonality component in the predictions

# Triple Smoothening
fit4 <- HoltWinters(opm.ts)
accuracy(fit4$fitted, opm.ts)
# predict next 4 observations
forecast(fit4, 4)
# MAPE = 2.91, it has gone down significantly
# There is seasonality component in the predictions

# Triple smoothening with ets()
fit5<-ets(opm.ts)
accuracy(fit5$fitted, opm.ts)

# predict next 4 observations
forecast(fit5, 4)
#MAPE = 1.94, it has gone down further
# and there is seasonality component in the forecast

#-----------------------------------Using ARIMA model
fit6 <- auto.arima(opm.ts)
ls(fit6)
fit6$model
fit6$series
summary(fit6)

# predictive accuracy
accuracy(fit6)
# MAPE = 2.42

# predict next 4 observations
library(forecast)
forecast(fit6, 4)
# There is seasonality component in the forecast

plot(forecast(fit6, 4))
