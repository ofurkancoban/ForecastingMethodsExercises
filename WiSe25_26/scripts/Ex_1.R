# Exercise 1 - 16 October 2025 ----

# Install Packages ----

## Installing ----

# install.packages("dplyr") # For data manipulation
# install.packages("Hmisc") # Variable labeling
# install.packages("expss") # Labeling and tables
# install.packages("ggplot2") # For data visualization
# install.packages("haven") # Importing SPSS, Stata, and SAS files
# install.packages("readr") # For reading rectangular text data CSV, TSV, and FWF
# install.packages("tidyverse") # Collection of R packages for data science

## Importing Libraries ----

library("dplyr") # For data manipulation
library("Hmisc") # Variable labeling
library("expss") # Labeling and tables
library("ggplot2") # For data visualization
library("haven") # Importing SPSS, Stata, and SAS files
library("readr") # For reading rectangular text data CSV, TSV, and FWF
library("tidyverse") # Collection of R packages for data science




# Basic Operations --------------------------------------------------------

5+2 


## Basic Functions ---------------------------------------------------------

log2(4)
log(4,2)

abs(-9)

sqrt(16)

# Working with characters -------------------------------------------------

"guten morgen"

# Logical Operations ------------------------------------------------------

96>100

3>=2

"Anna" == "Lukas"



# Vectors in R ------------------------------------------------------------

height <- c(170, 180, 175, 160)

names <- c("Nikki", "Tom", "Jerry", "Abigail")

df <- cbind(height, names) # Column bind
df

fruits <- list("apple", "banana", "orange")
vegetables <- list("carrot", "broccoli", "spinach")

df_fruits <- cbind(fruits, vegetables)


# Don'ts of R -------------------------------------------------------------

111 <- c(8.7) # Don't start variable names with numbers

! <- c(8.7) # Don't use special characters like !, @, #, $, %, ^, &, *, (, ), -, +, =, {, }, [, ], |, \, :, ;, ", ', <, >, ,, ., ?, / in variable names

1v <- c(8.7) # Don't start variable names with numbers

b1 <- c(8.7) # Variable names should not contain spaces

b_1 <- c(8.7) # Use underscores instead of spaces in variable names


ls() # List all objects in the environment
?ls # Get help on the ls function


# Importing Dataser -------------------------------------------------------

getwd() # Get current working directory

setwd("C:/Users/furka/OneDrive/Dokumente/GitHub/BA") # Set working directory
## Stata Dataset ----

stan <-haven::read_dta("WiSe25_26/datasets/STAN_ALL.dta")


stan_csv <- readr::read_csv("WiSe25_26/datasets/STAN_ALL.csv")

# Gather Information on the Data set --------------------------------------

names(stan) # Show variable names)
str(stan) # Show more information on variables
summary(stan) # Summary statistics for all variables
str(stan_csv)
summary(stan$EXPO))


ages <- c(2,8,NA)
mean(ages, na.rm=TRUE)

#$ Sorting Data ------------------------------------------------------------

stan <- stan |> 
  dplyr::arrange(OECDCODE)
stan
