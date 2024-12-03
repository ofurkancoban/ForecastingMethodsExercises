# Exercise 5 - 21 November 2024 ----

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
dax <- read_csv("~/Desktop/Uni Oldenburg/WiSe24:25/Forecasting Methods/ForecastingMethodsExercises/ForecastingMethodsExercises/datasets/DAX.csv")

# Rename and arrange the data
dax <- dax %>% 
  dplyr::rename(adj_close = `Adj Close`) %>% # Fixing the space issue in column name
  dplyr::mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>% # Ensure Date is correctly formatted
  dplyr::arrange(Date)

# Add a label for adj_close (optional)
label(dax$adj_close) <- "Adj Close"

# Check class of Date
class(dax$Date) # Should return "Date"

# Subset data for adj_close
dax_ts <- dax %>% 
  dplyr::select(adj_close)

# Create a time series object for adj_close
# Adjust frequency based on daily data with leap year consideration
dax_ts <- ts(dax_ts$adj_close, start = c(2009, 1), frequency = 365.25)

# Plotting the time series using ggplot2 ----

# Ensure column names match for ggplot
plot_ts <- ggplot(dax, aes(x = Date, y = adj_close)) +
  geom_line(color = "blue") +
  labs(title = "Adjusted Closings",
       x = "Date",
       y = "Value") +
  theme_minimal()

# Display the plot
print(plot_ts)


# Generate a new column ----

dax_ts <- dax %>% 
  dplyr::mutate(shares ="dax")

# Save dataset ---

write_csv(dax_ts, "~/Desktop/Uni Oldenburg/WiSe24:25/Forecasting Methods/ForecastingMethodsExercises/ForecastingMethodsExercises/datasets/dax_1.csv")

# Second dataset Vw ----

# Load Data Set ----

vw <- read_csv("~/Desktop/Uni Oldenburg/WiSe24:25/Forecasting Methods/ForecastingMethodsExercises/ForecastingMethodsExercises/datasets/VW.csv")

# TS object for VW ---- 

# Rename and arrange the data
vw <- vw %>% 
  dplyr::rename(adj_close = `Adj Close`) %>% # Fixing the space issue in column name
  dplyr::mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>% # Ensure Date is correctly formatted
  dplyr::arrange(Date)

vw_ts <- vw %>% 
  dplyr::select(adj_close)

# Create a time series object for adj_close
# Adjust frequency based on daily data with leap year consideration
vw_ts <- ts(vw_ts$adj_close, start = c(2009, 1), frequency = 365.25)

# Plotting the time series using ggplot2 ----
plot_ts <- ggplot(vw, aes(x = Date, y = adj_close)) +
  geom_line(color = "blue") +
  labs(title = "Adjusted Closings",
       x = "Date",
       y = "Value") +
  theme_minimal()

plot_ts
# Additive seasonality but not strong.

# Generate a new column ----
vw_ts <- vw %>% 
  dplyr::mutate(shares ="vw")

# Save dataset ---

write_csv(vw_ts, "~/Desktop/Uni Oldenburg/WiSe24:25/Forecasting Methods/ForecastingMethodsExercises/ForecastingMethodsExercises/datasets/vw_1.csv")

# Third dataset BASF.csv ----

bf <- read_csv("~/Desktop/Uni Oldenburg/WiSe24:25/Forecasting Methods/ForecastingMethodsExercises/ForecastingMethodsExercises/datasets/BASF.csv")

# TS object for BASF ----

bf <- bf %>% 
  dplyr::rename(adj_close = `Adj Close`) %>% # Fixing the space issue in column name
  dplyr::mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>% # Ensure Date is correctly formatted
  dplyr::arrange(Date)

bf_ts <- bf %>%
  dplyr::select(adj_close)

# Create a time series object for adj_close
# Adjust frequency based on daily data with leap year consideration
bf_ts <- ts(bf_ts$adj_close, start = c(2010, 1), frequency = 365.25)

# Plotting the time series using ggplot2 ----

plot_ts <- ggplot(bf, aes(x = Date, y = adj_close)) +
  geom_line(color = "blue") +
  labs(title = "Adjusted Closings",
       x = "Date",
       y = "Value") +
  theme_minimal()

plot_ts

    

# Generate a new column ----

bf_ts <- bf %>% 
  dplyr::mutate(shares ="basf")


# Appending 3 datasets ----

stock <- rbind(dax_ts, vw_ts, bf_ts)

stock <- stock %>% 
  arrange(shares, Date)

stock <- stock %>% 
  dplyr::group_by(shares) %>%
  dplyr::mutate(ID=dplyr::cur_group_id()) %>% 
  dplyr::ungroup()


# CMA ----

sm <- function(x, n=5) {
  stats::filter(x, rep(1/n, n), sides=2)
}


# Wrong!!!!

stock$sm_5 <- sm(stock$adj_close)

## Corrected ----

stock <- stock %>% 
  dplyr::group_by(shares) %>% 
  dplyr::mutate(sm_5c = sm(adj_close)) %>% 
  dplyr::ungroup()



stock <- stock %>% 
  dplyr::group_by(shares) %>% 
  dplyr::mutate(sm_5_t1 = zoo::rollmean(adj_close, k = 5, fill = NA, align = "center")) %>% 
  dplyr::ungroup()


stock <- stock %>% 
  dplyr::group_by(shares) %>% 
  dplyr::mutate(sm_5_t3 = ma(adj_close, order = 4 , centre=TRUE)) %>% 
  dplyr::ungroup()


# TMA 15, 38, 200 ----

stock_dax <- stock %>% 
  dplyr::filter(shares=="dax")


stock_dax <- stock_dax %>%
  mutate(ma_dax15 = rollmean(Open, k=15, align = "right", fill = NA),
         ma_dax38 = rollmean(Open, k=38, align = "right", fill = NA),
         ma_dax200 = rollmean(Open, k=200, align = "right", fill = NA))

# Plotting the time series using ggplot2 ----

ggplot(data = stock_dax, aes(x = Date)) +
  geom_line(aes(y = Open, color = "Open")) +
  geom_line(aes(y = ma_dax15, color = "ma_dax15")) +
  geom_line(aes(y = ma_dax38, color = "ma_dax38")) +
  geom_line(aes(y = ma_dax200, color = "ma_dax200")) +
  labs(title = "DAX",
       x = "Date",
       y = "Value") +
  scale_color_manual(values = c("Open" = "blue", "ma_dax15" = "red", "ma_dax38" = "green", "ma_dax200" = "purple")) +
  theme_minimal()


# VW 



