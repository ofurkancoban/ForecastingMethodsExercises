# Exercise 9 - 19 December 2024 ----

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
library("urca")
library("tseries")

# Simulate Data ----

set.seed(2)
  
df <- tibble(
    t = 1:500,
    uuid = rnorm(500, mean = 0, sd = 1)  # Generate random noise
  ) %>%
    mutate(
      y = case_when(
        t == 1 ~ 1,  # Set y = 1 when t == 1
        TRUE ~ NA_real_  # Set y = NA for all other cases
      ),
      dt = 0.9 + t + uuid  # Generate dt as a function of t and random noise
    )
  

# Plot ----

ggplot(df, aes(x=t))+geom_line(aes(y=dt))


# Random walk ----

df <- df %>% 
  mutate(rw = cumsum(uuid))

# Random Walk with Drift ----

df$st <- df$y 

for (i in 2:nrow(df)) {
  df$st[i] <- 0.9 + df$st[i-1] + df$uuid[i]
}

# RW without drift ----

df$rw_nd <- df$y 

for (i in 2:nrow(df)) {
  df$rw_nd[i] <- df$rw_nd[i-1] + df$uuid[i]
} 

# Plot them ----

par(mfrow = c(1,3))
plot(df$dt, type = 'l', main = 'DT')
plot(df$st, type = 'l', main = 'RW with Drift')
plot(df$rw_nd, type = 'l', main = 'RW without Drift')



