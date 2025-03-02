
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
done by the [{nhdplusTools}](https://github.com/DOI-USGS/nhdplusTools)
(much thanks) as well as the
[{leaflet}](https://github.com/rstudio/leaflet) extensions and all this
package does is wrap them into a app. Enjoy!

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

## Current Functionality

There are a handful of functions right now that can help with getting
water data visually: `get_nhdplus_interactively()`,
`get_nldi_interactively()`, `get_basin_interactively()`,
`get_streamnetwork_interactively()`, `get_usgs_iv_interactively()`,
`get_usgs_dv_interactively()`, `get_noaatlas_interactively()`. Please
read the manual for more information on how to use these functions.

## Example

This is a basic example which shows you how to solve a common problem:
getting water data without knowing the exact lat, lon via a shiny
application.

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
