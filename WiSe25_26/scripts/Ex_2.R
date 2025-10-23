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


stan <- readr::read_csv("WiSe25_26/datasets/STAN_ALL.csv")

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

# sort the data by country, OECDCODE and year
stan <- stan |> 
  dplyr::arrange(COUNTRY, OECDCODE, YEAR)


# sort in by year in a descending order
stan <- stan |> 
  dplyr::arrange(desc(YEAR))
View(stan)


## Generating Variables ----------------------------------------------------

stan <- stan %>% 
  mutate(netexpo = EXPO - IMPO)


#Drop  / Select Variables -----------------------------------------------------

##Dromping a variable

stan_1 <- stan |> 
  dplyr::select(-netexpo)

# Select Counrty, OECDCODE, YEAR and EXPO variables only

stan_2 <- stan |> 
  dplyr::select(COUNTRY,OECDCODE,YEAR,EXPO)



# Applying Labels ---------------------------------------------------------

stan <- stan |>  
  expss::apply_labels(netexpo = "Net Exports",EXPO = "Exports", IMPO = "Imports", SELF = "Self Employed")


# Renaming Columns -------------------------------------------------------

stan <- stan |>  
  dplyr::rename(Production=PROD)


# Keep Reduced Dataset ----------------------------------------------------

stan_rd <- stan |> 
  dplyr::select(YEAR, COUNTRY, ISICREV3, Production)
View(stan_rd)  



# Filtering Observations --------------------------------------------------

stan_rd <- stan_rd |> 
  dplyr::filter(!(COUNTRY=="DEU"|COUNTRY=="AUS"))


unique(stan_rd$COUNTRY)

# Keep Observations for USA only

stan_rd <- stan_rd |> 
  dplyr::filter(COUNTRY=="USA")
unique(stan_rd$COUNTRY)


# Keep Observations Using Sector Coding 31,32,33 ----
stan_rd <- stan_rd |> 
  dplyr::filter(ISICREV3 == "31" | ISICREV3 == "32" | ISICREV3 == "33")
unique(stan_rd$ISICREV3)



# Drop missing data points ------------------------------------------------

is.na(stan_rd)

stan_rd <- stan_rd |> 
  tidyr::drop_na()

# Also this works
stan_rd <- na.omit(stan_rd)




# Falsely generating growth_rate ------------------------------------------

stan_rd <- stan_rd |> 
  dplyr::mutate(
    prod_gw = (Production - dplyr::lag(Production)) / dplyr::lag(Production),
    n = 1,
    default = NA
  )
View(stan_rd)
# False because it's ot ordered according to year also indusry


# Correctly generating growth_rate -----------------------------------------

stan_rd <- stan_rd |> 
  dplyr::arrange(YEAR) |> 
  dplyr::group_by(COUNTRY, ISICREV3) |> 
  dplyr::mutate(
    prod_gw = (Production - dplyr::lag(Production)) / dplyr::lag(Production),
    n = 1,
    default = NA
  ) |> 
  dplyr::ungroup()
View(stan_rd)



# Plotting ----------------------------------------------------------------
## Scatter Plot -----

plot(stan_rd$YEAR, stan_rd$Production, main="Production over Years", xlab = "Year" , ylab = "Production")


# Filter Industry 31 and create a scatter Plot

stan_rd31 <- stan_rd |> 
  dplyr::filter(ISICREV3=="31")
plot(stan_rd31$YEAR, stan_rd31$Production, main="Production over Years for Industry 31", xlab = "Year" , ylab = "Production")


# Line Plot
plot(stan_rd31$YEAR, stan_rd31$Production, type="l", main="Production over Years for Industry 31", xlab = "Year" , ylab = "Production")



# Save plots or graphics --------------------------------------------------
# Step 1: Open the file device
png("Lineplot_Industry31.png") # Opens the png extension
# Step 2: Create the plot
plot(stan_rd31$YEAR, stan_rd31$Production, type="l", main="Production over Years for Industry 31", xlab = "Year" , ylab = "Production")
# Step 3: Close the file device
dev.off()  # Close the PNG file


# Similarly for PDF files
pdf("Lineplot_Industry31.pdf") # Opens the pdf extension
# Step 2: Create the plot
plot(stan_rd31$YEAR, stan_rd31$Production, type="l", main="Production over Years for Industry 31", xlab = "Year" , ylab = "Production")
# Step 3: Close the file device
dev.off()  # Close the PDF file

