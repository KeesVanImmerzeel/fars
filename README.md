
# fars

<!-- badges: start -->
[![Travis build status](https://travis-ci.com/KVIsweco/fars.svg?branch=master)](https://travis-ci.com/KVIsweco/fars)
<!-- badges: end -->

The goal of the package fars is to explore the Fatality Analysis Reporting System (FARS).
The data is limited to the years 2013, 2014 or 2015.

## Installation

You can install the released version of fars from [CRAN](https://CRAN.R-project.org) with:

``` r
devtools::install_github("KVIsweco/fars")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(fars)
## basic example code to return the number of fatal accidents per month.
fars_read_years(c("2013","2014"))
```


