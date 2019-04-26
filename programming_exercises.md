Programming Exercises
================
Your name
October 18, 2017

Load necessary libraries
------------------------

``` r
library(tidyverse)
```

    ## -- Attaching packages ------------------------------------------------------------------------ tidyverse 1.2.1 --

    ## v ggplot2 3.1.1       v purrr   0.3.2  
    ## v tibble  2.1.1       v dplyr   0.8.0.1
    ## v tidyr   0.8.3       v stringr 1.4.0  
    ## v readr   1.3.1       v forcats 0.4.0

    ## -- Conflicts --------------------------------------------------------------------------- tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(knitr)
library(dplyr)
library(ggplot2)
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

Calculate the square of each element in vector `x`
--------------------------------------------------

``` r
x <- 1:30
x
```

    ##  [1]  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23
    ## [24] 24 25 26 27 28 29 30

### Using a `for` loop

### Using a `map` function

Write a function to calculate length of sides in a right-triangle using the Pythagorean Theorem
-----------------------------------------------------------------------------------------------

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
    ##  date     2019-04-26                  
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
