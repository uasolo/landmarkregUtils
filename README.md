
<!-- README.md is generated from README.Rmd. Please edit that file -->

# landmarkregUtils

<!-- badges: start -->
<!-- badges: end -->

The goal of landmarkregUtils is to provide a flexible version of the
`fda::landmarkreg` function. Essentially, the computation of time
warping functions
![h(t)](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;h%28t%29 "h(t)")
and their use to register curves are decoupled. Wrapper functions allow
to either apply landmark registration directly on curve samples, thus
hiding the `fda` details to the user, or on `fd` objects. In the latter
case, the log rate of the warping function,
i.e. ![r(t) = - \log(\frac{dh}{dt})](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;r%28t%29%20%3D%20-%20%5Clog%28%5Cfrac%7Bdh%7D%7Bdt%7D%29 "r(t) = - \log(\frac{dh}{dt})"),
is also returned.

## Installation

You can install the development version of landmarkregUtils from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("uasolo/landmarkregUtils")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(landmarkregUtils)
## basic example code
```
