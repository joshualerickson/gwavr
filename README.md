
<!-- README.md is generated from README.Rmd. Please edit that file -->

# gwavr

<!-- badges: start -->

[![R-CMD-check](https://github.com/joshualerickson/gwavr/workflows/R-CMD-check/badge.svg)](https://github.com/joshualerickson/gwavr/actions)
[![CRAN
status](https://www.r-pkg.org/badges/version/gwavr)](https://CRAN.R-project.org/package=gwavr)
[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
<!-- badges: end -->

The goal of gwavr (Gee-waver) is to Get Water Attributes Visually in R
(gwavr). This allows the user to point and click on areas within the
United States and get back hydrological data, e.g. flowlines,
catchments, basin boundaries, comids, etc. Most of the heavy lifting is
done by the [{nhdplusTools}](https://github.com/USGS-R/nhdplusTools)
(much thanks) as well as the
[{leaflet}](https://github.com/rstudio/leaflet) extensions and all this
package does is wrapp them into a app. Enjoy!

## Installation

You can install `gwavr` from CRAN:

``` r
install.packages('gwavr')
```

To install the development version of the `gwavr` package, you can
install directly from [GitHub](https://github.com/):

``` r
# install.packages("devtools")
devtools::install_github("joshualerickson/gwavr")
```

## Contributions

Contributions are welcome!

## Example

This is a basic example which shows you how to solve a common problem:
getting water data without knowing the exact lat, lon.

``` r
library(gwavr)
## basic example code

nhdplus_data <- get_nhdplus_interactively()

## or for NLDI

nldi_data <- get_nldi_interactively()
```

<center>

## Video

<img src='inst/www/huc12.gif' class = 'center'>

</center>
