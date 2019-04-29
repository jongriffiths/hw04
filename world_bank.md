Exploring World Bank Data
================
Jon Griffiths
April 29, 2019

Load necessary libraries
------------------------

``` r
library(tidyverse)
```

    ## -- Attaching packages --------------------------------------------------------------------------------------------------------------------------------------- tidyverse 1.2.1 --

    ## v ggplot2 3.1.1       v purrr   0.3.2  
    ## v tibble  2.1.1       v dplyr   0.8.0.1
    ## v tidyr   0.8.3       v stringr 1.4.0  
    ## v readr   1.3.1       v forcats 0.4.0

    ## -- Conflicts ------------------------------------------------------------------------------------------------------------------------------------------ tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(knitr)
library(ggplot2)
library(dplyr)
library(purrr)
library(repurrrsive)
library(stats)
```

Write a function to import the data files
-----------------------------------------

``` r
#read files, skip first 4 rows, and edit data to increase ease of use
import_wb_data <- function(file) { 
  read_csv(file, skip = 4) %>% 
    select(-"Indicator Code", -X62) %>%
    rename(country = "Country Name",
           country_code = "Country Code",
           indicator = "Indicator Name"
    ) %>%
    
  #reposition variables for analysis  
    gather(key = "year", 
           value = "value", c("1960":"2016")) %>% 
    
    spread(key = indicator, 
           value = "value") %>%
    
    #rename the variables I will be using
    rename(inc_per_capita = "Adjusted net national income per capita (current US$)",
           female_households = "Female headed households (% of households with a female head)",
           out_of_school_female = "Adolescents out of school, female (% of female lower secondary school age)",
           out_of_school_male = "Adolescents out of school, male (% of male lower secondary school age)") %>%
    
    #Remove unnecessary variables
    select(country, country_code, inc_per_capita, female_households, out_of_school_female, out_of_school_male)
}
```

Import the data
---------------

``` r
allcountries <- list.files(path = "data_world_bank", pattern = "*.csv", full.names = TRUE)

#wb_data = world bank data
wb_data <- vector(mode = "list", length(allcountries))
for(i in seq_along(allcountries)){
  wb_data[[i]] <- import_wb_data(allcountries[[i]])
}
```

    ## Warning: Missing column names filled in: 'X62' [62]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X62 = col_logical()
    ## )

    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X62' [62]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X62 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X62' [62]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X62 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X62' [62]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X62 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X62' [62]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X62 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X62' [62]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X62 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X62' [62]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X62 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X62' [62]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X62 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X62' [62]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X62 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X62' [62]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X62 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X62' [62]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X62 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X62' [62]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X62 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X62' [62]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X62 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X62' [62]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X62 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X62' [62]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X62 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

``` r
wb_data <- bind_rows(wb_data)
wb_data
```

    ## # A tibble: 855 x 6
    ##    country country_code inc_per_capita female_househol~ out_of_school_f~
    ##    <chr>   <chr>                 <dbl>            <dbl>            <dbl>
    ##  1 Angola  AGO                      NA               NA               NA
    ##  2 Angola  AGO                      NA               NA               NA
    ##  3 Angola  AGO                      NA               NA               NA
    ##  4 Angola  AGO                      NA               NA               NA
    ##  5 Angola  AGO                      NA               NA               NA
    ##  6 Angola  AGO                      NA               NA               NA
    ##  7 Angola  AGO                      NA               NA               NA
    ##  8 Angola  AGO                      NA               NA               NA
    ##  9 Angola  AGO                      NA               NA               NA
    ## 10 Angola  AGO                      NA               NA               NA
    ## # ... with 845 more rows, and 1 more variable: out_of_school_male <dbl>

Explore the data
----------------

### Out of School Female Variance

``` r
out_of_school_fe_variance = var( x = wb_data$out_of_school_female, use = "complete.obs")
(out_of_school_fe_variance)
```

    ## [1] 141.8563

### Out of School Male Variance

``` r
out_of_school_ma_variance = var( x = wb_data$out_of_school_male, use = "complete.obs")
(out_of_school_ma_variance)
```

    ## [1] 104.8845

### Out of School Covariance

``` r
covariance <- cov(x = wb_data$out_of_school_male, y = wb_data$out_of_school_female, use = "complete.obs",
    method = c("pearson"))
(covariance)
```

    ## [1] 113.7076

### Out of School Correlation

``` r
correlation <- cor(x = wb_data$out_of_school_male, y = wb_data$out_of_school_female, use = "complete.obs",
    method = c("pearson"))
(correlation)
```

    ## [1] 0.9322006

### Female Out of School Regression

``` r
female_regression <- lm(out_of_school_female ~ female_households + inc_per_capita, data = wb_data)
(summary(female_regression))
```

    ## 
    ## Call:
    ## lm(formula = out_of_school_female ~ female_households + inc_per_capita, 
    ##     data = wb_data)
    ## 
    ## Residuals:
    ##     158     169     616     617     675 
    ## -23.087   7.545   8.974  12.042  -5.474 
    ## 
    ## Coefficients:
    ##                   Estimate Std. Error t value Pr(>|t|)  
    ## (Intercept)       64.02622   21.87993   2.926   0.0996 .
    ## female_households  0.07917    0.71364   0.111   0.9218  
    ## inc_per_capita    -0.02380    0.01036  -2.298   0.1483  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 20.56 on 2 degrees of freedom
    ##   (850 observations deleted due to missingness)
    ## Multiple R-squared:  0.7364, Adjusted R-squared:  0.4728 
    ## F-statistic: 2.793 on 2 and 2 DF,  p-value: 0.2636

### Male Out of School Regression

``` r
male_regression <- lm(out_of_school_male ~ female_households + inc_per_capita, data = wb_data)
(summary(male_regression))
```

    ## 
    ## Call:
    ## lm(formula = out_of_school_male ~ female_households + inc_per_capita, 
    ##     data = wb_data)
    ## 
    ## Residuals:
    ##     158     169     616     617     675 
    ## -21.870   7.117   7.836  12.079  -5.162 
    ## 
    ## Coefficients:
    ##                    Estimate Std. Error t value Pr(>|t|)
    ## (Intercept)       55.047019  20.784693   2.648    0.118
    ## female_households  0.047128   0.677916   0.070    0.951
    ## inc_per_capita    -0.019535   0.009838  -1.986    0.185
    ## 
    ## Residual standard error: 19.53 on 2 degrees of freedom
    ##   (850 observations deleted due to missingness)
    ## Multiple R-squared:  0.6772, Adjusted R-squared:  0.3545 
    ## F-statistic: 2.098 on 2 and 2 DF,  p-value: 0.3228

\`\`\`

For this assignment, I decided to look at the rates at which male and female adolescents do not attend school. I first examined the variances in these data. The overall variance is larger for females than it is for males. This means that there is a larger spread amongst the data between countries with high female enrollment and low female enrollment.

I also examined the covariance and correlation between male and female youth unenrollment. As expected, there was a high degree of correlation between the variables. Correlation falls on a scale of 0 to 1 where 0 is no correlation and 1 is perfect correlation. Between the unenrollment of the two sexes, the correlation coefficient is r = .93, which is close to a perfect correlation. This means, as is to be expected, countries with high male unenrollment also have high female unenrollment and vice versa.

Since the data were replete with missing values, the chosen variables were not conducive to graphical interpretation. As such, I employ two multiple regression models which have each sex's respective unenrollment rate as the outcome variables. These variables are measured as a function the percent of a nation's households that are headed by women and the income per capita. While the percent of women-headed households and income per capita appear to have a smaller effect on the rate which boys attend school, the results from both models are statistically insignificant at a p &lt; .05. As such, I cannot draw any substantive conclusions about the relationships between the variables.

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
    ##  fansi         0.4.0   2018-10-05 [1] CRAN (R 3.5.2)
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
    ##  utf8          1.1.4   2018-05-24 [1] CRAN (R 3.5.2)
    ##  withr         2.1.2   2018-03-15 [1] CRAN (R 3.5.2)
    ##  xfun          0.6     2019-04-02 [1] CRAN (R 3.5.3)
    ##  xml2          1.2.0   2018-01-24 [1] CRAN (R 3.5.2)
    ##  yaml          2.2.0   2018-07-25 [1] CRAN (R 3.5.2)
    ## 
    ## [1] C:/Users/User/Documents/R/win-library/3.5
    ## [2] C:/Program Files/R/R-3.5.3/library
