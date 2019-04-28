library(tidyverse)
library(knitr)
library(ggplot2)
library(dplyr)
library(purrr)
library(repurrrsive)
library(stats)

# Question 1
data("iris")

q1_loop_output <- vector(mode = "double", length = ncol(iris))
for (i in unique(iris)){
print(length(unique(i)))
}

map_int(iris, ~ length(unique(.)))


#Question 2 

x <- 1:30
x


 


q2_sqx <- vector(mode = "double")
for (i in range(x)) {
  q2_sqx <- (x^2)
}
q2_sqx

map_dbl(1:30, ~(.)^2)


# Question 3 
pythagorean <- function(side1, side2, hypo) {
  if(side1 == FALSE || side2 == FALSE || hypo == FALSE) {
    stop("ERROR: Invalid Number of Inputs")
  }
  if(side1 == TRUE || side2 == FALSE || hypo == FALSE) {
    stop("ERROR: Invalid Number of Inputs")
  }
  if(side1 == FALSE || side2 == TRUE || hypo == FALSE) {
    stop("ERROR: Invalid Number of Inputs")
  }
  if(side1 == FALSE || side2 == FALSE || hypo == TRUE) {
    stop("ERROR: Invalid Number of Inputs")
  }
  if (side1 == TRUE || side2 == TRUE || hypo == TRUE) {
    stop("ERROR: Invalid Number of Inputs")
  }
  if (side1 != as.numeric()) {
    stop("Invalid Input Type")
  }
  if (side2 != as.numeric()) {
    stop("Invalid Input Type")
  }
  if (hypo != as.numeric()) {
    stop("Invalid Input Type")
  }
if (side1 == TRUE| side2 == TRUE| hypo == FALSE) {
hypotenuse <- sqrt(side1^2 + side2^2)
return(hypotenuse)
} else if (side1 == TRUE | hypo == TRUE | side2 == FALSE) {
sidetwo <- sqrt(hypo^2 - side1^2)
return(sidetwo)
} else if (side2 == TRUE| hypo == TRUE| side1 == FALSE) {
sideone <- sqrt(hypo^2 - side2^2)
return(sideone)
} else{
return(NULL) 
}
}

pythagorean( hypo = 2, side2 = 1, side1 = FALSE)

pythagorean2 <- function(side1, side2, hypo) {

if (side1 != as.numeric(side1)) {
  stop("Invalid Input Type")
}
if (side2 != as.numeric(side2)) {
  stop("Invalid Input Type")
}
if (hypo != as.numeric(hypo)) {
  stop("Invalid Input Type")
}
  if (side1 == TRUE| side2 == TRUE| hypo == FALSE) {
    hypotenuse <- sqrt(side1^2 + side2^2)
    return(hypotenuse)
  } else if (side1 == TRUE | hypo == TRUE | side2 == FALSE) {
    sidetwo <- sqrt(hypo^2 - side1^2)
    return(sidetwo)
  } else if (side2 == TRUE| hypo == TRUE| side1 == FALSE) {
    sideone <- sqrt(hypo^2 - side2^2)
    return(sideone)
  } else{
    return("Invalid Inputs") 
  }
}
pythagorean2( hypo = 2, side2 = 1, side1 = FALSE)




#Part 2 
#read files and skip first 4 rows
import_wb_data <- function(file) { 
  
  # Read *.csv files, skipping rows 1:4
  read_csv(file, skip = 4) %>% 
    
    select(-"Indicator Code", -X62) %>%
    rename(country = "Country Name",
           country_code = "Country Code",
           variable = "Indicator Name"
    ) %>%
    
    
    gather(key = "year", 
           value = "value", c("1960":"2016")) %>% 
    
    spread(key = variable, 
           value = "value") %>%
    
    rename(inc_per_capita = "Adjusted net national income per capita (current US$)",
           female_households = "Female headed households (% of households with a female head)",
           out_of_school_female = "Adolescents out of school, female (% of female lower secondary school age)",
           out_of_school_male = "Adolescents out of school, male (% of male lower secondary school age)",
           safety_net = "Adequacy of social safety net programs (% of total welfare of beneficiary households)") %>%
    select(country, country_code, inc_per_capita, female_households, out_of_school_female, out_of_school_male, safety_net)
}

allcountries <- list.files(path = "data_world_bank", pattern = "*.csv", full.names = TRUE)

wb_data <- vector(mode = "list", length(allcountries))
for(i in seq_along(allcountries)){
  wb_data[[i]] <- import_wb_data(allcountries[[i]])
}

wb_data <- bind_rows(wb_data)
wb_data


#Analysis
#Out of School Female Variance
out_of_school_fe_variance = var( x = wb_data$out_of_school_female, use = "complete.obs")
(out_of_school_fe_variance)

#Out of School Male Variance
out_of_school_ma_variance = var( x = wb_data$out_of_school_male, use = "complete.obs")
(out_of_school_ma_variance)

#Out of School Covariance
cov(x = wb_data$out_of_school_male, y = wb_data$out_of_school_female, use = "complete.obs",
    method = c("pearson"))

#Out of School Correlation
cor(x = wb_data$out_of_school_male, y = wb_data$out_of_school_female, use = "complete.obs",
    method = c("pearson"))