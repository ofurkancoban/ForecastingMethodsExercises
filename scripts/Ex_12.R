# Exercise 12 - 16 January 2025 ----

# Load Required Libraries ----
library(forecast)
library(tseries)
library(dplyr)
library(ggplot2)
library(readr)

# Oilgold Data ----

gold <- read_csv("~/Desktop/Uni Oldenburg/WiSe24:25/Forecasting Methods/ForecastingMethodsExercises/ForecastingMethodsExercises/datasets/oilgold.csv")

gold <- gold %>% 
  mutate(date = as.Date(date,format="%d%b%Y"))