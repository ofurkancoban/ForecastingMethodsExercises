# Exercise 10 - 9 January 2025 ----

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

# Stationary Test ----

## ADF Test with adf.test ----

# Ho: Dataset has unit roots/ is not stationary
# Decision: P-Values should be less than 0.05 to reject the null hypothesis(Ho)
# So we cn not reject the null hypothesis 


adf.test(df$st)
adf.test(df$rw_nd)

# ADF test with unitroot_kpss ----

# Ho: ts has no unit root/ is stationary
# Decision: P-Values should be less than 0.05 to reject the null hypothesis(Ho)

unitroot_kpss(df$st)
unitroot_kpss(df$rw_nd)

# Generate AR and MA Process ----

## AR(1) Process ----
# y_t = c + \phi_1 y_{t-1} + \epsilon_t

df$ar1 <- df$y

for (i in 2:nrow(df)) {
  df$ar1[i] <- 0.9 + 0.6*df$ar1[i-1] + df$uuid[i]
}

par(mfrow = c(1,1))
plot(df$ar1, type= "l")


# ACF and PACF Plot for ARI ----
par(mfrow = c(1,2))
Acf(df$ar1, plot = TRUE,na.action = na.pass)
Pacf(df$ar1, plot = TRUE,na.action = na.pass)


## MA(1) Process ----
# y_t = c + \epsilon_t + \theta_1 \epsilon_{t-1}

df$ma1 <- df$y

df <- df %>%  mutate(ma1=5 + uuid + lag(0.85*uuid))

plot(df$ma1,type = "l")

# ACF and PACF Plot for MA(1) ----

par(mfrow = c(1,2))
Acf(df$ma1, plot = TRUE,na.action = na.pass)
Pacf(df$ma1, plot = TRUE,na.action = na.pass)




# Simulate MA(5) Process ----
# y_t = c + ε_t + θ_1 * ε_{t-1} + θ_2 * ε_{t-2} + ... + θ_5 * ε_{t-5}

ma5_sim <- arima.sim(model=list(ma =c(0.55,0.4,0.3,0.25,0.2)), n = 500,sd = 10)

# Plot ACF and PACF ----


