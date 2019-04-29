Programming Exercises
================
Jon Griffiths
April 29, 2019

Load necessary libraries
------------------------

``` r
library(tidyverse)
```

    ## -- Attaching packages -------------------------------------------- tidyverse 1.2.1 --

    ## v ggplot2 3.1.1       v purrr   0.3.2  
    ## v tibble  2.1.1       v dplyr   0.8.0.1
    ## v tidyr   0.8.3       v stringr 1.4.0  
    ## v readr   1.3.1       v forcats 0.4.0

    ## -- Conflicts ----------------------------------------------- tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(knitr)
library(dplyr)
library(ggplot2)
library(purrr)
library(repurrrsive)
```

Compute the number of unique values in each column of `iris`
------------------------------------------------------------

### Using a `for` loop

``` r
q1_loop_output <- vector(mode = "double", length = ncol(iris))
for (i in unique(iris)){
print(length(unique(i)))
}
```

    ## [1] 35
    ## [1] 23
    ## [1] 43
    ## [1] 22
    ## [1] 3

### Using a `map` function

``` r
map_int(iris, ~ length(unique(.)))
```

    ## Sepal.Length  Sepal.Width Petal.Length  Petal.Width      Species 
    ##           35           23           43           22            3

Calculate the square of each element in vector `x`
--------------------------------------------------

``` r
x <- 1:30
x
```

    ##  [1]  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23
    ## [24] 24 25 26 27 28 29 30

### Using a `for` loop

``` r
q2_sqx <- vector(mode = "double")
for (i in range(x)) {
  q2_sqx <- (x^2)
}
q2_sqx
```

    ##  [1]   1   4   9  16  25  36  49  64  81 100 121 144 169 196 225 256 289
    ## [18] 324 361 400 441 484 529 576 625 676 729 784 841 900

### Using a `map` function

``` r
square_function <- function(x){
  x^2
}
x %>%
map_dbl(square_function)
```

    ##  [1]   1   4   9  16  25  36  49  64  81 100 121 144 169 196 225 256 289
    ## [18] 324 361 400 441 484 529 576 625 676 729 784 841 900

Write a function to calculate length of sides in a right-triangle using the Pythagorean Theorem
-----------------------------------------------------------------------------------------------

``` r
#defines the 3 sides of the triangle
pythagorean <- function(side1, side2, hypo) {

#stops the function if any of the sides are not imputed as numeric
  
if (side1 != as.numeric(side1)) {
  stop("Invalid Input Type")
}
if (side2 != as.numeric(side2)) {
  stop("Invalid Input Type")
}
if (hypo != as.numeric(hypo)) {
  stop("Invalid Input Type")
}
  #indicates what sides must be present and what is returned under those circumstances
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
    #This is what is returned if there is a different number of inputs than specified above
    return("Invalid Number of Inputs") 
  }
}
```

Session info
------------

``` r
devtools::session_info()
```

    ## - Session info ----------------------------------------------------------
    ##  setting  value                       
    ##  version  R version 3.5.3 (2019-03-11)
    ##  os       Windows 10 x64              
    ##  system   x86_64, mingw32             
    ##  ui       RTerm                       
    ##  language (EN)                        
    ##  collate  English_United States.1252  
    ##  ctype    English_United States.1252  
    ##  tz       America/Chicago             
    ##  date     2019-04-28                  
    ## 
    ## - Packages --------------------------------------------------------------
    ##  package     * version date       lib source        
    ##  assertthat    0.2.1   2019-03-21 [1] CRAN (R 3.5.3)
    ##  backports     1.1.3   2018-12-14 [1] CRAN (R 3.5.2)
    ##  broom         0.5.2   2019-04-07 [1] CRAN (R 3.5.3)
    ##  callr         3.2.0   2019-03-15 [1] CRAN (R 3.5.3)
    ##  cellranger    1.1.0   2016-07-27 [1] CRAN (R 3.5.2)
    ##  cli           1.1.0   2019-03-19 [1] CRAN (R 3.5.3)
    ##  colorspace    1.4-1   2019-03-18 [1] CRAN (R 3.5.3)
    ##  crayon        1.3.4   2017-09-16 [1] CRAN (R 3.5.2)
    ##  desc          1.2.0   2018-05-01 [1] CRAN (R 3.5.3)
    ##  devtools      2.0.1   2018-10-26 [1] CRAN (R 3.5.3)
    ##  digest        0.6.18  2018-10-10 [1] CRAN (R 3.5.2)
    ##  dplyr       * 0.8.0.1 2019-02-15 [1] CRAN (R 3.5.3)
    ##  evaluate      0.13    2019-02-12 [1] CRAN (R 3.5.3)
    ##  forcats     * 0.4.0   2019-02-17 [1] CRAN (R 3.5.3)
    ##  fs            1.2.7   2019-03-19 [1] CRAN (R 3.5.3)
    ##  generics      0.0.2   2018-11-29 [1] CRAN (R 3.5.2)
    ##  ggplot2     * 3.1.1   2019-04-07 [1] CRAN (R 3.5.3)
    ##  glue          1.3.1   2019-03-12 [1] CRAN (R 3.5.3)
    ##  gtable        0.3.0   2019-03-25 [1] CRAN (R 3.5.3)
    ##  haven         2.1.0   2019-02-19 [1] CRAN (R 3.5.3)
    ##  hms           0.4.2   2018-03-10 [1] CRAN (R 3.5.2)
    ##  htmltools     0.3.6   2017-04-28 [1] CRAN (R 3.5.2)
    ##  httr          1.4.0   2018-12-11 [1] CRAN (R 3.5.2)
    ##  jsonlite      1.6     2018-12-07 [1] CRAN (R 3.5.2)
    ##  knitr       * 1.22    2019-03-08 [1] CRAN (R 3.5.3)
    ##  lattice       0.20-38 2018-11-04 [2] CRAN (R 3.5.3)
    ##  lazyeval      0.2.2   2019-03-15 [1] CRAN (R 3.5.3)
    ##  lubridate     1.7.4   2018-04-11 [1] CRAN (R 3.5.2)
    ##  magrittr      1.5     2014-11-22 [1] CRAN (R 3.5.2)
    ##  memoise       1.1.0   2017-04-21 [1] CRAN (R 3.5.3)
    ##  modelr        0.1.4   2019-02-18 [1] CRAN (R 3.5.3)
    ##  munsell       0.5.0   2018-06-12 [1] CRAN (R 3.5.2)
    ##  nlme          3.1-137 2018-04-07 [2] CRAN (R 3.5.3)
    ##  pillar        1.3.1   2018-12-15 [1] CRAN (R 3.5.2)
    ##  pkgbuild      1.0.3   2019-03-20 [1] CRAN (R 3.5.3)
    ##  pkgconfig     2.0.2   2018-08-16 [1] CRAN (R 3.5.2)
    ##  pkgload       1.0.2   2018-10-29 [1] CRAN (R 3.5.3)
    ##  plyr          1.8.4   2016-06-08 [1] CRAN (R 3.5.2)
    ##  prettyunits   1.0.2   2015-07-13 [1] CRAN (R 3.5.2)
    ##  processx      3.3.0   2019-03-10 [1] CRAN (R 3.5.3)
    ##  ps            1.3.0   2018-12-21 [1] CRAN (R 3.5.2)
    ##  purrr       * 0.3.2   2019-03-15 [1] CRAN (R 3.5.3)
    ##  R6            2.4.0   2019-02-14 [1] CRAN (R 3.5.3)
    ##  Rcpp          1.0.1   2019-03-17 [1] CRAN (R 3.5.3)
    ##  readr       * 1.3.1   2018-12-21 [1] CRAN (R 3.5.2)
    ##  readxl        1.3.1   2019-03-13 [1] CRAN (R 3.5.3)
    ##  remotes       2.0.2   2018-10-30 [1] CRAN (R 3.5.3)
    ##  repurrrsive * 0.1.0   2017-09-08 [1] CRAN (R 3.5.3)
    ##  rlang         0.3.4   2019-04-07 [1] CRAN (R 3.5.3)
    ##  rmarkdown     1.12    2019-03-14 [1] CRAN (R 3.5.3)
    ##  rprojroot     1.3-2   2018-01-03 [1] CRAN (R 3.5.3)
    ##  rstudioapi    0.10    2019-03-19 [1] CRAN (R 3.5.3)
    ##  rvest         0.3.2   2016-06-17 [1] CRAN (R 3.5.2)
    ##  scales        1.0.0   2018-08-09 [1] CRAN (R 3.5.2)
    ##  sessioninfo   1.1.1   2018-11-05 [1] CRAN (R 3.5.3)
    ##  stringi       1.4.3   2019-03-12 [1] CRAN (R 3.5.3)
    ##  stringr     * 1.4.0   2019-02-10 [1] CRAN (R 3.5.3)
    ##  tibble      * 2.1.1   2019-03-16 [1] CRAN (R 3.5.3)
    ##  tidyr       * 0.8.3   2019-03-01 [1] CRAN (R 3.5.3)
    ##  tidyselect    0.2.5   2018-10-11 [1] CRAN (R 3.5.2)
    ##  tidyverse   * 1.2.1   2017-11-14 [1] CRAN (R 3.5.3)
    ##  usethis       1.5.0   2019-04-07 [1] CRAN (R 3.5.3)
    ##  withr         2.1.2   2018-03-15 [1] CRAN (R 3.5.2)
    ##  xfun          0.6     2019-04-02 [1] CRAN (R 3.5.3)
    ##  xml2          1.2.0   2018-01-24 [1] CRAN (R 3.5.2)
    ##  yaml          2.2.0   2018-07-25 [1] CRAN (R 3.5.2)
    ## 
    ## [1] C:/Users/User/Documents/R/win-library/3.5
    ## [2] C:/Program Files/R/R-3.5.3/library
