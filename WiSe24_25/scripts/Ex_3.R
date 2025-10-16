# Exercise 3 - 07 November 2024 ----

## Import Libraries ----

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

air_usa <- read_csv("~/Desktop/Uni Oldenburg/WiSe24:25/Forecasting Methods/ForecastingMethodsExercises/ForecastingMethodsExercises/datasets/air-usa.csv")

air_usa <- air_usa %>% 
  expss::apply_labels(pas = "Passengers")

is.Date(air_usa$time)

# Date transformations ----

air_usa <- air_usa %>% 
  mutate(year=str_sub(time,1, 4),
         month=str_sub(time, 6)
  ) %>% 
  mutate(date = as.Date(paste("01", month,year, sep="-"),format="%d-%m-%Y"))

is.Date(air_usa$date)

# Time Series ----

is.ts(air_usa)

# Convert to timeseries ----

air_usa_ts <- ts(air_usa$pas, start = c(1996, 1), frequency = 12) #year, month, freq

is.ts(air_usa_ts)

# Time series plot in R ----

plot(
  air_usa$date,
  air_usa$pas,
  xlab = "time",
  type="l",
  ylab = "Passenger",
  col = "blue",
  main = "Number of Passengers")

ts.plot(air_usa_ts) # another method

plot_raw <- ggplot(air_usa, aes(x=date, y=pas))+
  geom_line()+
  labs(title="Number of Passengers",
       x="Date",
       y="Passengers"
  )

plot_raw

# Saving the plot ----
ggsave("passenger.png")

# Multiplicative seasonality !


# Add a trendline ----

plot_tl <- ggplot(air_usa, aes(x=date, y=pas))+
  geom_line()+
  geom_smooth(
    aes(color="Trendline"),
    method= "lm_robust",
    se=FALSE,
    linewidth=0.5
  ) +
  labs(title="Number of Passengers",
       x="Date",
       y="Passengers"
  )

plot_tl


# Other Techniques ----
layout(1:2)
plot(aggregate(air_usa_ts))
boxplot(air_usa_ts ~ cycle(air_usa_ts))

# R-Decomposition ----

air_decom <- decompose(air_usa_ts, type = "multiplicative")

plot(air_decom)


# Regression Models ----

# Creating Time Index ----

air_usa <-air_usa %>% 
  mutate(trend=row_number())

# OLS Regression ----

reg_ols <- lm_robust(pas ~ trend, data = air_usa)

summary(reg_ols)


# Number of passenger increase 84 unit.
# Statisticaly significant.

msummary(reg_ols,
         stars=c("*"=0.1,"**"=0.05,"***"=0.01),
         fmt ="%.3f")


















