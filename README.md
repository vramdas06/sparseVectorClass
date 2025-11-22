
<!-- README.md is generated from README.Rmd. Please edit that file -->

# sparseVector

<!-- badges: start -->

[![R-CMD-check.yaml](https://github.com/vramdas06/sparseVectorClass/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/vramdas06/sparseVectorClass/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of sparseVector is to define an S4 class that represents sparse
numeric vectors. The methods in this package perform much more
efficiently than typical dense vector operations when the vector
contains a lot of 0s. This package includes coersion methods between
sparse and dense vectors, simple arithmetic operations on sparse
vectors, and more advanced capabilities like plotting sparse vector
overlap and computing the mean, norm, and the standardized vector.

## Installation

You can install the development version of sparseVector like so:

``` r
devtools::install_github("vramdas06/sparseVectorClass")
```

## Example

This is a basic example which shows you how to use sparseVector:

``` r
library(sparseVector)
#> 
#> Attaching package: 'sparseVector'
#> The following object is masked from 'package:base':
#> 
#>     norm
x <- c(1, 0, 3, 0, 5, 0, 0)
sparse_x <- as(x, "sparse_numeric")
sparse_x
#> Sparse numeric vector of length 7 
#> Non-zero elements:
#>   pos value
#> 1   1     1
#> 2   3     3
#> 3   5     5

x <- as(c(1, 0, 3, 0, 5), "sparse_numeric")
y <- as(c(0, 2, 3, 0, 1), "sparse_numeric")

# Add two sparse vectors
x + y
#> Sparse numeric vector of length 5 
#> Non-zero elements:
#>   pos value
#> 1   1     1
#> 2   2     2
#> 3   3     6
#> 4   5     6

# Subtract
x - y
#> Sparse numeric vector of length 5 
#> Non-zero elements:
#>   pos value
#> 1   1     1
#> 2   2    -2
#> 3   5     4

# Multiply (element-wise)
x * y
#> Sparse numeric vector of length 5 
#> Non-zero elements:
#>   pos value
#> 1   3     9
#> 2   5     5

# Plot
plot(x, y)
```

<img src="man/figures/README-example-1.png" width="100%" />
