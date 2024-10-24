# Exercise 2 - 24 October 2024 ----

# Data Manipulations ----

## Import Libraries ----

library("haven") 
library("Hmisc") 
library("expss")
library("dplyr") 
library("ggplot2")
library("tidyverse") 
library("readr")


# Importing Data ----

STAN <- read_dta("~/Desktop/Uni Oldenburg/WiSe24:25/Forecasting Methods/ForecastingMethodsExercises/ForecastingMethodsExercises/datasets/STAN_ALL.dta")
View(stan)


STAN <- read_csv("~/Desktop/Uni Oldenburg/WiSe24:25/Forecasting Methods/ForecastingMethodsExercises/ForecastingMethodsExercises/datasets/STAN_ALL.csv")
View(stan_csv)

# Data Manipulations ----

# Generating Variables ----
STAN <- STAN %>% 
  mutate(net_export = EXPO - IMPO)


# Select and Drop Variables ----

STAN_red <- STAN %>% 
  dplyr::select(EXPO, IMPO, net_export) 


STAN_red <- STAN_red %>% 
  select(-net_export)

STAN <- STAN %>% 
  select(-net_export) %>% 
  mutate(NX = EXPO - IMPO)


# Labeling of Variables ----

STAN <- STAN %>% 
  expss::apply_labels(NX = "Net Exports")

# Renaming Variables ----

STAN <- STAN %>% 
  dplyr::rename(capital_stock=GFCF)

STAN <- STAN %>% 
  dplyr::rename(inter_input=INTI)

# Keep Relevant Variables : No overwriting the original data ----

STAN_red <- STAN %>%
  dplyr::select(COUNTRY, YEAR, ISICREV3, PROD)
  

# Drop Observations with Condition ----

# Dropping Germany and Austria
STAN_red <- STAN_red %>% 
  dplyr::filter(!(COUNTRY=="DEU"|COUNTRY=="AUS"))


unique(STAN_red$COUNTRY)

# Keep Observations
# USA

STAN_red <- STAN_red %>% 
  dplyr::filter(COUNTRY=="USA")

# Keep Observations Using Sector Coding 31,32,33 ----

STAN_red <- STAN_red %>% 
  dplyr::filter(ISICREV3=="31"|ISICREV3=="32"|ISICREV3=="33")

unique(STAN_red$ISICREV3)

# Drop Missing Values(NA) ----

STAN_red <- na.omit(STAN_red)

STAN_red2 <- STAN %>% 
  tidyr::drop_na()

is.na(STAN_red2)

unique((is.na(STAN_red2)))


# Grouping Data (Variables) ---- 

STAN_red <- STAN_red %>% 
  dplyr::arrange(YEAR) %>% 
  dplyr::group_by(COUNTRY, ISICREV3) %>% 
  dplyr:: mutate(prod_lag = dplyr::lag(PROD, n=1, default =NA)) %>% 
  ungroup()

# Generate Growth Rate ----

STAN_red <- STAN_red %>% 
  dplyr:: arrange(YEAR) %>% 
  dplyr:: group_by(COUNTRY, ISICREV3) %>% 
  dplyr:: mutate(prod_gwr = (PROD-prod_lag)/prod_lag) %>% 
  ungroup()

# Simple Graphs ----

# Scatter Plots ----

STAN_red <- STAN_red %>% 
  dplyr::arrange(COUNTRY, YEAR, ISICREV3)

# Plot
plot(STAN_red$YEAR, STAN_red$PROD, main="My First R Plot", xlab = "Year" , ylab = "Production")

# Keep Industry 31 and Plot

STAN_red31 <- STAN_red %>% 
  dplyr::filter(ISICREV3=="31")


# Plot
plot(STAN_red$YEAR, STAN_red$PROD, type ="l", main="For 31", xlab = "Year" , ylab = "Production")
 
# Save as a PNG

png("Lineplot_31.png") # Opens the png extension
plot(STAN_red$YEAR, STAN_red$PROD, type ="l", main="For 31", xlab = "Year" , ylab = "Production")
dev.off()  # Close the PNG file

# Save as a PDF


pdf("Lineplot_31.pdf") # Opens the pdf extension
plot(STAN_red$YEAR, STAN_red$PROD, type ="l", main="For 31", xlab = "Year" , ylab = "Production")
dev.off()  # Close the PNG file


# Merging Datasets ----


















