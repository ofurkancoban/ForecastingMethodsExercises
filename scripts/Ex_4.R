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

# Exercise 4 - 14 November 2024 ----

air_usa <- air_usa %>% 
  mutate(pas_pre = predict(reg_ols, newdata=air_usa))

head(air_usa$pas_pre)
head(air_usa$pas) # Comparing prediction and real data, we get the errors. 
# When we subtract the real data from the prediction, we get the errors, we get residuals.
# If you use new data points, you get forecast errors, they are diffent from residuals.

# We use linear models to estimate the trend.
# Polinomial models can be used to estimate the trend.


# Generate polynomial trend ----
# Create polynomial terms in the air_usa dataset
air_usa <- air_usa %>% 
  mutate(
    t = trend,
    t_2 = t^2,
    t_3 = t^3,
    t_4 = t^4,
    t_5 = t^5)

# Polynomial Regression
models <- list()  # Initialize the list correctly

models[["Poly 2"]] <- lm_robust(pas ~ t + t_2, data = air_usa)
models[["Poly 3"]] <- lm_robust(pas ~ t + t_2 + t_3, data = air_usa)
models[["Poly 4"]] <- lm_robust(pas ~ t + t_2 + t_3 + t_4, data = air_usa)
models[["Poly 5"]] <- lm_robust(pas ~ t + t_2 + t_3 + t_4 + t_5, data = air_usa)

poly_2 <- lm_robust(pas ~ t + t_2, data = air_usa)
poly_3 <- lm_robust(pas ~ t + t_2 + t_3, data = air_usa)
poly_4 <- lm_robust(pas ~ t + t_2 + t_3 + t_4, data = air_usa)
poly_5 <- lm_robust(pas ~ t + t_2 + t_3 + t_4 + t_5, data = air_usa)


# Display summary of models with significance stars
msummary(models, stars = c("*" = 0.1, "**" = 0.05, "***" = 0.01), fmt = "%.3f")


# R^2 is low. But why do you think R^2 is low? Because we have seasonality in data. 




# Predictions for Polynomial Models ----

air_usa <- air_usa %>% 
  mutate(
    pas_pre2 = predict(poly_2, newdata = air_usa),
    pas_pre3 = predict(poly_3, newdata = air_usa),
    pas_pre4 = predict(poly_4, newdata = air_usa),
    pas_pre5 = predict(poly_5, newdata = air_usa)
  )


# Graphing ----
plot_poly <- ggplot(air_usa, aes(x=t))+
  geom_line(aes(y=pas, color="blue"))+
  geom_line(aes(y=pas_pre2, color="red"))+
  geom_line(aes(y=pas_pre3, color="yellow"))+
  geom_line(aes(y=pas_pre4, color="green"))+
  geom_line(aes(y=pas_pre5, color="purple"))+
  labs(title="Polynomial Models",
       x="Time",
       y="Number of Passengers",
  )

plot_poly

# Exponential Smoothing ----
# Exponential Regression ----

# First take log of number of passengers ----
air_usa <- air_usa %>% 
  mutate(
    log_pas = log(pas)
  )

# Plot log of passengers using ggplot ----

plot_log <- ggplot(air_usa, aes(x=t, y=log_pas))+
  geom_line()+
  labs(title="Log of Number of Passengers",
       x="Time",
       y="Log of Passengers"
  )

plot_log


# Run the regression ----

exp_reg <- lm_robust(log_pas ~ t, data = air_usa)

# Predict the values ----

air_usa <- air_usa %>% 
  mutate(
    log_pas_pre = predict(exp_reg, newdata = air_usa),
    pas_pre_exp = exp(log_pas_pre)
  )

# Plot the predictions ----

plot_exp <- ggplot(air_usa, aes(x=t))+
  geom_line(aes(y=pas, color="red"))+
  geom_line(aes(y=pas_pre_exp, color="blue"))+
  labs(title="Exponential Regression",
       x="Time",
       y="Number of Passengers"
  )

plot_exp

# The difference between log and linear model 
# is that the linear model is not able to capture the exponential growth in the data.
# The exponential model is able to capture the exponential growth in the data.


# Generate Residuals For Polynomial Models ----
air_usa <- air_usa %>% 
  mutate(res = pas - pas_pre,
         res_2 = pas - pas_pre2,
         res_3 = pas - pas_pre3,
         res_4 = pas - pas_pre4,
         res_5 = pas - pas_pre5
  )

# Are the residuals mistakes? or errors? -----
# Mistakes are the difference between the actual value and the predicted value.
# Errors are the difference between the actual value and the forecasted value.
# they are unexplained part of ...


# Plot the residuals ----
plot_res <- ggplot(air_usa, aes(x=t))+
  geom_line(aes(y=res, color="blue"))+
  geom_line(aes(y=res_2, color="red"))+
  geom_line(aes(y=res_3, color="yellow"))+
  geom_line(aes(y=res_4, color="green"))+
  geom_line(aes(y=res_5, color="purple"))+
  labs(title="Residuals",
       x="Time",
       y="Residuals"
  )

plot_res

# Do it individually ----

plot_raw <- ggplot(air_usa, aes(x=t, y=res))+
  geom_line()+
  labs(title="Residuals",
       x="Time",
       y="Residuals",
       color="blue"
  )

plot_res_2 <- ggplot(air_usa, aes(x=t, y=res_2))+
  geom_line()+
  labs(title="Residuals",
       x="Time",
       y="Residuals",
       color="red"
  )
plot_res_3 <- ggplot(air_usa, aes(x=t, y=res_3))+
  geom_line()+
  labs(title="Residuals",
       x="Time",
       y="Residuals",
       color="yellow"
  )
plot_res_4 <- ggplot(air_usa, aes(x=t, y=res_4))+
  geom_line()+
  labs(title="Residuals",
       x="Time",
       y="Residuals",
       color="green"
  )

plot_res_5 <- ggplot(air_usa, aes(x=t, y=res_5))+
  geom_line()+
  labs(title="Residuals",
       x="Time",
       y="Residuals",
       color="purple"
       
  )


gridExtra::grid.arrange(plot_raw, plot_res_2, plot_res_3, plot_res_4, plot_res_5, ncol=2)


summary(air_usa$res) 

# Standardization of Residuals ----

st <- function(x) {
  return(x - mean(x, na.rm=TRUE)) / sd(x, na.rm = TRUE)
}


res_st <- st(air_usa$res)
res_st  

# Using dplyr ----

air_usa <- air_usa %>% 
  mutate(st_res = (res-mean(res,na.rm=TRUE))/sd(res,na.rm=TRUE),
         st_res_2 = (res_2-mean(res_2,na.rm=TRUE))/sd(res_2,na.rm=TRUE),
         st_res_3 = (res_3-mean(res_3,na.rm=TRUE))/sd(res_3,na.rm=TRUE),
         st_res_4 = (res_4-mean(res_4,na.rm=TRUE))/sd(res_4,na.rm=TRUE),
         st_res_5 = (res_5-mean(res_5,na.rm=TRUE))/sd(res_5,na.rm=TRUE))


# Histogram Plot ----

# Histogram Plot ----

air_long <- air_usa %>% 
  dplyr::select(starts_with("st_")) %>% 
  tidyr::pivot_longer(cols = starts_with("st_"),
                      names_to = "Distribution",
                      values_to = "Values")  

ggplot(air_long, aes(x = Values)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, fill = "green", color = "black") +
  stat_function(fun = dnorm, args = list(mean = 0, sd = 1), color = "blue", linewidth = 1) +
  facet_wrap(~ Distribution, scales = "free") +
  scale_colour_manual(values = c("darkblue", "darkred", "darkorange")) +
  theme_minimal()







