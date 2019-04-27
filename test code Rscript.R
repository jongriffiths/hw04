library(tidyverse)
library(knitr)
library(ggplot2)
library(dplyr)
library(purrr)
library(repurrrsive)

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
pythagorean <- function(side1, side2, hypo) {
  if (side1 == TRUE| side2 == TRUE| hypo == is.null()) {
    hypotenuse <- sqrt(side1^2 + side2^2)
    return(hypotenuse)
  } else if (side1 == TRUE | hypo == TRUE | side2 == is.null()) {
    sidetwo <- sqrt(hypo^2 - side1^2)
    return(sidetwo)
  } else if (side2 == TRUE| hypo == TRUE| side1 == is.null()) {
    sideone <- sqrt(hypo^2 - side2^2)
    return(sideone)
  } else{
    return(NULL) 
  }
}


 


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


pythagorean3 <- function(side1, side2, hypo) {
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
pythagorean3(side1 = 1, hypo = 2, side2 = FALSE)
