# Exercise 1 - 17 October 2024 ----

## Import Libraries ----

library("haven") 
library("Hmisc") 
library("expss")
library("dplyr") 
library("ggplot2")
library("tidyverse") 
library("readr")

## First Coding ----

## Set Working Directory ----

getwd()
#setwd("")



## Basics ----
# Basic Operations ----

5+4
7-4
8 / 2
10 * 2
8/2

# Basic Functions ----

log2(4)
sqrt(25)
abs(-1000)

# Working with Characters ----

print("hi Furkan")

# Logical Operations ----

96>100

3>=2

"Anna" == "Lukas"

# Vectors in R ----

age <- c(25, 9, 30, 60)

age1 <- list(3, 10, 12,15)

names <- c("Nikki", "Tom", "Jerry", "Abigail")

df <- cbind(age, names) # Combines lists

# Don'ts in R ----

# 111 <- c(8.7)
# $ <- c(7,9)
# 1c1 <- c(8,1)

# Create a numeric vector ----

age <-c(25,32,55,NA)

min(age)
max(age)
mean(age) # Mean of age is NA !

mean(age, na.rm=TRUE)

# Importing Data ----

stan <- read_dta("~/Desktop/Uni Oldenburg/WiSe24:25/Forecasting Methods/ForecastingMethodsExercises/ForecastingMethodsExercises/datasets/STAN_ALL.dta")
View(stan)


stan_csv <- read_csv("~/Desktop/Uni Oldenburg/WiSe24:25/Forecasting Methods/ForecastingMethodsExercises/ForecastingMethodsExercises/datasets/STAN_ALL.csv")
View(stan_csv)


# Gather Information on the Data set ----

str(stan_csv)

# Show Variable Names ----

names(stan_csv)

# Show More Information on Variables ----

str(stan_csv)

# Summary Statistics for All Variables ----
summary(stan_csv)

# Summary Statistics for One Variable ----

summary(stan_csv$PROD)

# Show Characteristics of a string variable ----

summary(factor(stan_csv$COUNTRY))

unique(stan_csv$COUNTRY)

stan_1 <- stan_csv %>% 
  arrange(desc(COUNTRY))


## Sort the dataframe in R using multiple variable with Dplyr ----

stan_2 <- stan_csv %>% 
  dplyr::arrange(COUNTRY, OECDCODE, YEAR)

stan_3 <- stan_csv %>% 
  arrange(desc(COUNTRY))

stan_4 <- stan_csv %>% 
  arrange(desc(OECDCODE))

stan_5 <- stan_csv %>% 
  arrange(desc(YEAR))

## Sort Data: Multiple Varibles ----

stan_3 <- stan_csv %>% 
  arrange(desc(COUNTRY)) %>% 
  arrange(desc(OECDCODE)) %>% 
  arrange(desc(YEAR))


# Exercise 2 - 24 October 2024 ----

# Data Manipulations ----


















































