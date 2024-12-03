# Exercise 6 - 28 November 2024 ----

# Exponantial Smoothing ----

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

# Ensure the file path is correct or use the proper file path
station <- read_csv("~/Desktop/Uni Oldenburg/WiSe24:25/Forecasting Methods/ForecastingMethodsExercises/ForecastingMethodsExercises/datasets/station5108.csv")

station <- station %>% 
  filter(complete.cases(p)) %>% mutate(p = p*100, 
  mydate=as.Date(mydate,"%d%b%Y"),
  year = year(mydate),
  quarter = quarter(mydate))

# Split data into training and test sets ----

station_train <- station[1:1593,]
station_test <- station[1594:1824,]

# why did we split the data exactly like this?



# You can not randomly split time series data

# Plot data ----

ggplot(station, aes(x = mydate, y = p)) +
  geom_line(color = "blue") +
  labs(title = "Station 5108",
       x = "Date",
       y = "Value") +
  theme_minimal()

#Interpret the plot

# First of all take whole series as a whole
# Linear trend ? no
# Cycles ? No

# Which model to use ? Exponential Smoothing 
# Which exponential smoothing model to use ? Holt-Winters

# Convert to TS object ----

station_train_ts <- ts(station_train$p, start = c(2014, 1), frequency = 365.25)
station_test_ts <- ts(station_test$p, start = c(2018, 135), frequency = 365.25)

#as.POSIXlt(x="2014-05-15")$yday # show the which day of the day. 134th day of the year
#as.POSIXlt(x="2014-01-01")$yday


# Simple Exponential Smoothing ----

# We need to choose and optimal alpha value. To do that we need sum of squared errors for each alpha value.

alpha <- seq(0.01, 0.99, by = 0.01)

sse_mat <- sapply(alpha, function(a)
  ets(station_train_ts, model = "ANN", alpha = a)$mse)


sse <- tibble(alpha = alpha, sse = sse_mat) %>% 
  mutate(ln_sse=log(sse))

# Which alpha level gives us minimum?


# Plotting ----

ggplot(sse, aes(x = alpha, y = ln_sse)) +
  geom_line(color = "red") +
  labs(title = "SSE vs Alpha",
       x = "Alpha",
       y = "Log SSE") +
  theme_minimal()

opt_alpha <- sse %>% 
  filter(sse == min(sse)) %>% 
  pull(alpha)

# Much simpler way to find the optimal alpha value

sse_model <- ets(station_train_ts, model = "ANN")
summary(sse_model)

# Smoothing parameters: alpha = 0.5407

# Forecasting ----

# Exam question!!!! 

fore_ses <- forecast(sse_model, h = 10) # 10 days ahead forecast

# Plot Original vs Fitted Values ----

fitted_values <- fitted(sse_model)
plot_data <- data.frame(
  Time = time(station_train_ts),
  Original = as.numeric(station_train_ts),
  Fitted = as.numeric(fitted_values)
)

ggplot(plot_data, aes(x = Time)) + 
  geom_line(aes(y = Original), color = "green") +
  geom_line(aes(y = Fitted), color = "red") +
  labs(
    title = "Original vs Fitted Values",
    x = "Time",
    y = "Values"
  ) +
  theme_minimal()

# Plot Forecast ----

autoplot(fore_ses) +
  autolayer(fore_ses$fitted, series = "Fitted", color = "red") +
  xlim(2018.2, 2018.4) +
  labs(
    title = "Forecast with Exponential Smoothing",
    x = "Time",
    y = "Values"
  ) +
  theme_minimal()

# Accuracy measures ----

fore_error <- function(actual, predicted) {
  tibble(
    ME = mean(actual - predicted), # Mean Error
    MAE = mean(abs(actual - predicted)), # Mean Absolute Error
    MAPE = mean(abs((actual - predicted) / actual)), # Mean Absolute Percentage Error
    RMSE = sqrt(mean((actual - predicted)^2)), # Root Mean Squared Error
  )
}
      


errors <- fore_error(station_test_ts[1:10], fore_ses$mean)   
errors         


# Do it for Rain(rr) dataset for 30 day forecast . Assignment










