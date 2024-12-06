# Exercise 6 - 05 December 2024 ----

# Import Libraries ----

library("haven") 
library("Hmisc") 
library("expss")
library("dplyr") 
library("ggplot2")
library("tidyverse") 
library("readr")
library("forecast")
library("lubridate")
library("zoo")
library("xts")
library("gridExtra")
library("estimatr")
library("modelsummary")
library("lmtest")
library("tseries")
library("fpp2")
library("generics")

# Exponential Smoothing ----

# Load Data Set ----

aus <- austa
aus
autoplot(aus)

# Explain the plot regards to components of time series and suggest the method.

# There is a clear trend. Simple expnontial smoothing.
# There is a clear trend and seasonality. Holt-Winters method. Which one? Additive or Multiplicative?
# Holt-Winters Linear Trend Model

# Holt Linear Trend Estimator ----

aus_holt <- holt(aus, h = 5)

summary(aus_holt)

# We have two components: alpha and beta. Alpha is the smoothing parameter for the level and beta is the smoothing parameter for the trend.

autoplot(aus_holt)

checkresiduals(aus_holt)

# The residuals are not normally distributed. The residuals are not independent. The residuals are not homoscedastic.
# Residuals are from the dataset. Actual datapoint - its predicted value.

# Ljung-Box test
# 
# data:  Residuals from Holt's method
# Q* = 4.8886, df = 7, p-value = 0.6736
# 
# Model df: 0.   Total lags used: 7

# Mean is fluctuating around 0. 
# ACF plot shows that there is no significatant spikes. That means model is not good enough.

# Import Electricity Dataset ----

elec <- read.csv("~/Desktop/Uni Oldenburg/WiSe24:25/Forecasting Methods/ForecastingMethodsExercises/ForecastingMethodsExercises/datasets/electricity-usa.csv")


# Data Preprocessing ----

elec <- elec %>% 
  mutate(time = as.Date(as.yearmon(year, format ="%Y %B")),
         month = month(time),
         ID = row_number()
         )

# Plot the data ----


ggplot(elec, aes(x = time, y = eletpus)) + 
  geom_line(color = "blue") + 
  labs(title = "Electricity Demand in USA", x = "Time", y = "Electricity")

# It has Trend and Seasonality, Which model? 
# Addidtive or Multiplicative?
# Mupliticative because spikes are increasing by time
# Holt-Winters Multiplicative Seasonality Model 

# Convert to TS object ----

elec_ts <- ts(elec$eletpus, start = c(1973, 1), frequency = 12)

# Splitting the Dataset ----

elec_train <- window(elec_ts, start = c(1973, 1), end = c(2009, 12))
elec_test <- window(elec_ts, start = c(2010, 1), end = c(2010, 11))

elec <- elec %>% 
  mutate(train_test = ifelse(row_number()<= length(elec_train), "train", "test"))

# OLS Regression ----

reg_ols <- lm_robust(eletpus ~ID + as.factor(month),
                     data = filter(elec, train_test =="train"))


summary(reg_ols)

# Insample Prediction ----

elec <- elec %>% 
  mutate(forecast=predict(reg_ols, newdata =.))
  

# Compute the errors ----

elec <- elec %>% 
  mutate(
    residual = eletpus - forecast, 
    error = ifelse(train_test == "test", residual, NA_real_)
  )


# Accuracy measures fıor the Test set ----

test_metrics <- elec %>% 
  filter(train_test == "test") %>% 
  summarize(
    MAE = mean(abs(error), na.rm = TRUE),
    RMSE = sqrt(mean(error^2, na.rm = TRUE)),
    MAPE = mean(abs(error/eletpus), na.rm = TRUE)*100
  )

test_metrics


# Plot Forecast vs. Actual ----

library(ggplot2)

ggplot(elec, aes(x = time)) + 
  geom_line(aes(y = eletpus, color = "Actual")) + 
  geom_line(aes(y = forecast, color = "Forecast")) + 
  scale_color_manual(values = c("Actual" = "blue", "Forecast" = "red")) +
  labs(title = "Electricity Demand vs Forecast", 
       x = "Time", 
       y = "Electricity",
       color = "Legend")

  
  
# Holt-Winters Multiplicative Model ----

hw_m <- hw(elec_train, seasonal = "multiplicative", h = 11)
summary(hw_m)  


autoplot(hw_m)+autolayer(fitted(hw_m))

generics::accuracy(hw_m, elec_test)



# Holt-Winters Additive Model ----

hw_a <- hw(elec_train, seasonal = "additive", h = 11)
summary(hw_a)  


autoplot(hw_m)+autolayer(fitted(hw_a))

generics::accuracy(hw_a, elec_test)


# Logirthms to stabilize the vvarience of series and then we can apply a model

# Additive Seasonal with Log ----

elec <- elec %>% 
  mutate(
    log_eletpus = log(eletpus)
    )

elec_train_log <- ts(elec$log_eletpus[1:444], start = c(1973, 1), frequency = 12)
elec_test_log <- ts(elec$log_eletpus[445:455], start = c(2010, 1), frequency = 12)
hw_log <- hw(elec_train_log, seasonal = "additive", h = 11)

summary(hw_log)

autoplot(hw_log)+autolayer(fitted(hw_log))

generics::accuracy(hw_log, elec_test_log)

# Back-transform ----

bck <- exp(hw_log$mean)

# Compute accuracy measure for log ----

MAE_log = mean(abs(elec_test-bck), na.rm = TRUE)
RMSE_log = sqrt(mean((elec_test-bck)^2, na.rm = TRUE))
MAPE_log = mean(abs((elec_test-bck)/elec_test), na.rm = TRUE)*100


generics::accuracy(hw_a, elec_test)
generics::accuracy(hw_m, elec_test)
test_metrics
MAE_log
RMSE_log
MAPE_log  





