# Exercise 12 - 16 January 2025 ----

# Load Required Libraries ----
library(forecast)
library(tseries)
library(dplyr)
library(ggplot2)
library(readr)
library(urca)

# Oilgold Data ----

gold <- read_csv("~/Desktop/Uni Oldenburg/WiSe24:25/Forecasting Methods/ForecastingMethodsExercises/ForecastingMethodsExercises/datasets/oilgold.csv")

gold <- gold %>% 
  mutate(mydate = as.Date(date,format="%d%b%Y"))

# Plot ----

ggplot(gold, aes(x = mydate, y = goldprice)) +
  geom_line(color = "blue") +
  labs(title = "Gold Time Series", x = "Time", y = "Gold") +
  theme_minimal()

# Stationarity Test ADF Test ----
adf_1 <- adf.test(gold$goldprice)
adf_1

# unitroot_kpss
# It's not stationary because p-value is larger than 0.05. We cannot reject the hypotesis that the series is not stationary
 
# ADFC Test ----

# Sword Criteria for Lag Selection ----
# 12 * (N/100)^0.25

adf_2 <- adf.test(gold$goldprice, k = 33)
adf_2

# Using ur.df test ----

ur_adf1 <- ur.df(gold$goldprice, lags = 33, type = "drift")
summary(ur_adf1)

# First Differencing ----

diff_gold <- diff(gold$goldprice)

gold <- gold %>% 
  mutate(diff_gold = c(NA, diff(goldprice)))

# Plot ----



ggplot(gold, aes(x = mydate, y = diff_gold)) +
  geom_line(color = "blue") +
  labs(title = "First Difference of Gold", x = "Date", y = "Gold") +
  theme_minimal()


# ADF Test for 1st Difference of Gold ----

diff_adf <- ur.df(diff_gold, lags = 33)
summary(diff_adf)



# ACF and PACF Plots for the Differenced Gold ----

par(mfrow = c(1,2))
Acf(gold$diff_gold, main = "Differenced Gold", na.action = na.pass)
Pacf(gold$diff_gold, main = "Differenced Gold", na.action = na.pass)

# Which model should we estimate?
# ARIMA(1,1,1)

arima111 <- Arima(gold$goldprice, order = c(1,1,1), include.drift=TRUE)
summary(arima111)

# ARIMA 3,1,3

arima313 <- Arima(gold$goldprice, order = c(3,1,3), include.drift=TRUE)
summary(arima313)

# ARIMA 3,1,1

arima311 <- Arima(gold$goldprice, order = c(3,1,1), include.drift=TRUE)
summary(arima311)
# ARIMA 1,1,3

arima113 <- Arima(gold$goldprice, order = c(1,1,3), include.drift=TRUE)
summary(arima113)
# ARIMA 13,1,13

# arima13113 <- Arima(gold$goldprice, order = c(13,1,13), include.drift=TRUE)
# summary(arima13113)



AIC(arima111,arima313,arima311,arima113)


# Best Model is ARIMA(3,1,3) with a drift.

# Residual check ----
checkresiduals(arima313)

auto_best <- auto.arima(gold$goldprice, trace = TRUE, approximation = FALSE, stepwise = FALSE)
# 
