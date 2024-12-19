# Exercise 8 - 19 December 2024 ----

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
library("mFilter")

# Importing and Preparing Data ----
# Load GDP dataset and add time components 


gdp <- read_csv("~/Desktop/Uni Oldenburg/WiSe24:25/Forecasting Methods/ForecastingMethodsExercises/ForecastingMethodsExercises/datasets/gdp.csv") 

gdp <- gdp %>% 
  mutate(year = as.numeric(str_sub(time,1, 4)),
         quarter = as.numeric(str_sub(time, 6)),
         ID = row_number(),
         log_gdp = log(gdp)
  )

# Convert TS object ----
gdp_ts <- ts(gdp$gdp, start = c(1991, 1), frequency = 4) #year, quarter, freq

log_gdp_ts <- ts(gdp$log_gdp, start = c(1991, 1), frequency = 4) #year, quarter, freq


# Plot

plot(gdp_ts)

# Linear Detrending ----

lin.mod <- lm_robust(log_gdp ~ID, data = gdp)

# Extract Fitted Values and Make it TS object ----

lin.trend <- ts(lin.mod$fitted.values, start = c(1991, 1), frequency = 4)

# Cycle Component ----

lin.cycle <- log_gdp_ts - lin.trend

plot(lin.cycle)

# Hotrick _ Proscott(HP) Filter ----

# Lambda value for cycle = 1600 (quarterly data)

hp.decomp <- hpfilter(log_gdp_ts, freq = 1600)
hp_trend <- hp.decomp$trend
hp_cycle <- hp.decomp$cycle

# Combining Outputs df ----

HP_out <- cbind(hp.decomp$x, hp.decomp$trend, hp.decomp$cycle)
colnames(HP_out) <- c("x", "trend", "cycle")
HP_out <- as.data.frame((HP_out))

# Easier way to plot ----

plot(hpfilter(log_gdp_ts,freq = 1600))
# monthly lambda = 14400
# annual = 10

# Growth ----

gdp <- gdp %>% 
  mutate(gr_log_gdp_ly = log_gdp-lag(log_gdp, 4),
  gr_log_gdp_lq = log_gdp-lag(log_gdp, 1)
  )

# Plot ----

par(mfrow = c(1,1))

plot(gdp$gr_log_gdp_ly, type = "l", col = "darkblue")
lines(gdp$gr_log_gdp_lq, type= "l", col = "darkgreen")
lines(HP_out$cycle, type = "l", col = "darkred")
legend("bottomleft",
       legend= c("1 Year Growth", "1 Quarter Growth", "HP Cycle"),
       col = c("darkblue", "darkgreen", "darkred"),
       lty = 1,
       bty = "n")
abline(h=0, col = "red")

       
# The difference between 1 Year growth, 1 quarter growth and HP Cycle?


# Both series show same fluctuations (movements)
# Growth rate of first quarter shows smaller deviations than 1 year growth rate
#
# HP filters are combined 1 year and 1 quarter so it's between them






