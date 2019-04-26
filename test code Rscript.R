library(tidyverse)
library(knitr)
library(ggplot2)
library(dplyr)

data("iris")

q1_loop_output <- vector(mode = "double", length = ncol(iris))
for (i in unique(iris)){
print(length(unique(i)))
}


