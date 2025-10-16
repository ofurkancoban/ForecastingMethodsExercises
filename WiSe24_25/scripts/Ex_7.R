# Exercise 7 - 12 December 2024 ----

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

# Load Data Set ----

waren <- read_csv("~/Desktop/Uni Oldenburg/WiSe24:25/Forecasting Methods/ForecastingMethodsExercises/ForecastingMethodsExercises/datasets/waren.csv")

# Date Format ----
waren <- waren %>% 
  mutate(year=str_sub(time,1, 4),
         month=str_sub(time, 6)
  ) %>% 
  mutate(date = as.Date(paste("01", month,year, sep="-"),format="%d-%m-%Y"))


# Plot TS ----

ggplot(waren, aes(x = date, y = waren)) +
  geom_line(color = "blue") +
  labs(title = "Waren Time Series", x = "Time", y = "Waren") +
  theme_minimal()


# TS Conversion ----
waren_ts <- ts(waren$waren, start = c(1998, 1), frequency = 12)


# Train Test Split ----
# Train set end last date in 2009
# Remaining is test set
train_ts <- window(waren_ts, end = c(2009, 12))
test_ts <- window(waren_ts, start = c(2010, 1))

# Regression linear trend and seasonal dummies ----
time <- 1:length(train_ts)
season <- factor(cycle(train_ts))
model <- lm(train_ts ~ time + season)
summary(model)

# Predict ----
pred_train <- fitted(model)
pred_test <- predict(model, newdata = data.frame(time = length(train_ts) + 1:length(test_ts), 
                                                 season = factor(cycle(test_ts), levels = levels(season))))

# Forecast ----
forecast_horizon <- 12
forecast_time <- length(train_ts) + length(test_ts) + 1:(forecast_horizon)
forecast_season <- factor(rep(1:12, length.out = forecast_horizon), levels = levels(season))
forecast_values <- predict(model, newdata = data.frame(time = forecast_time, season = forecast_season))


# Plot Original, Predicted and Forecasted ----
forecast_df <- data.frame(
  time = seq.Date(as.Date("2011-01-01"), by = "month", length.out = forecast_horizon),
  forecast = forecast_values
)

train_df <- data.frame(
  time = as.Date(time(train_ts), origin = "1998-01-01"),
  original = as.numeric(train_ts),
  predicted = pred_train
)

test_df <- data.frame(
  time = as.Date(time(test_ts), origin = "1998-01-01"),
  original = as.numeric(test_ts),
  predicted = pred_test
)

ggplot() +
  geom_line(data = train_df, aes(x = time, y = original), color = "blue") +
  geom_line(data = train_df, aes(x = time, y = predicted), color = "green", linetype = "dashed") +
  geom_line(data = test_df, aes(x = time, y = original), color = "blue") +
  geom_line(data = test_df, aes(x = time, y = predicted), color = "orange", linetype = "dashed") +
  geom_line(data = forecast_df, aes(x = time, y = forecast), color = "red", linetype = "dotted") +
  labs(title = "Original, Predicted, and Forecasted Waren", x = "Time", y = "Waren") +
  theme_minimal()


# Holt-Winters ----
# Make 12 head forecast ----

# Apply Holt-Winters method for forecasting
holt_model_mult <- HoltWinters(train_ts, seasonal = "multiplicative")
holt_model_add <- HoltWinters(train_ts, seasonal = "additive")
holt_model_mult
holt_model_add
# Forecasting for the next 12 months using Holt-Winters
holt_forecast_mult <- forecast(holt_model_mult, h = 12)
holt_forecast_add <- forecast(holt_model_add, h = 12)

# Plot the Holt-Winters forecast
plot(holt_forecast_mult, main = "Holt-Winters Forecast Multiplicative", xlab = "Time", ylab = "Waren")
lines(test_ts, col = "red", lty = 2)
legend("topright", legend = c("Test Data", "Holt-Winters Forecast Multiplicative"), col = c("red", "black"), lty = c(2, 1))

plot(holt_forecast_add, main = "Holt-Winters Forecast Additive", xlab = "Time", ylab = "Waren")
lines(test_ts, col = "red", lty = 2)
legend("topright", legend = c("Test Data", "Holt-Winters Forecast Additive"), col = c("red", "black"), lty = c(2, 1))



# Simple Exponential Smoothing ----
# Apply Simple Exponential Smoothing
ses_model <- ses(train_ts, h = 12)

# Plot the SES forecast
autoplot(ses_model) +
  autolayer(test_ts, series = "Test Data", PI = FALSE) +
  labs(title = "Simple Exponential Smoothing Forecast",
       x = "Time", y = "Waren") +
  theme_minimal()

# Residual Analysis ----
# ????

# Model Selection ----
# Compare models using accuracy metrics
holt_accuracy_mult<- accuracy(holt_forecast_mult, test_ts)
holt_accuracy_add <- accuracy(holt_forecast_add, test_ts)
ses_accuracy <- accuracy(ses_model, test_ts)

# Display accuracy metrics
model_comparison <- data.frame(
  Model = c("Holt-Winters", "Simple Exponential Smoothing"),
  RMSE_mult = c(holt_accuracy_mult["Test set", "RMSE"], ses_accuracy["Test set", "RMSE"]),
  RMSE_add = c(holt_accuracy_add["Test set", "RMSE"], ses_accuracy["Test set", "RMSE"]),
  MAE_mult = c(holt_accuracy_mult["Test set", "MAE"], ses_accuracy["Test set", "MAE"]),
  MAE_add = c(holt_accuracy_add["Test set", "MAE"], ses_accuracy["Test set", "MAE"])
)
print(model_comparison)

