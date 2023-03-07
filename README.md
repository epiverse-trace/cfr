
<!-- README.md is generated from README.Rmd. Please edit that file -->

# *datadelay*: Estimating disease severity and under-reporting

<!-- badges: start -->

[![License:
MIT](https://img.shields.io/badge/License-MIT-blue.svg)](https://opensource.org/licenses/MIT)
[![R-CMD-check](https://github.com/epiverse-trace/datadelay/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/epiverse-trace/datadelay/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/epiverse-trace/datadelay/branch/main/graph/badge.svg)](https://app.codecov.io/gh/epiverse-trace/datadelay?branch=main)
[![CRAN
status](https://www.r-pkg.org/badges/version/datadelay)](https://CRAN.R-project.org/package=datadelay)
<!-- badges: end -->

The goal of datadelay is to provide simple, fast methods for estimation
of disease severity and under-reporting in real-time, accounting for
delays in epidemic timeseries.

## Installation

You can install the development version of datadelay from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("epiverse-trace/datadelay")

# Also install epiparameter for epidemiological parameter values
devtools::install_github("epiverse-trace/epiparameter")
```

## Quick start

### Ebola 1976

This example of basic usage shows how to use *datadelay* to estimate
case fatality ratios from the 1976 Ebola outbreak.

``` r
# Load package
library(datadelay)

# Load the Ebola 1976 data provided with the package
data("ebola1976")

# create an epidist for EVD onset to death distribution
# taken from parameters in 10.1016/S0140-6736(18)31387-4
onset_to_death_ebola <- epiparameter::epidist(
  disease = "Ebola virus disease",
  pathogen = "Ebolavirus",
  epi_dist = "onset_to_death",
  prob_distribution = "gamma",
  prob_distribution_params = c(
    shape = 2.4, scale = 3.333
  )
)
#> Citation cannot be created as either author, year or DOI is missing

# Calculate the static naive and corrected CFRs
ncfr <- static_cfr(ebola1976, correct_for_delays = FALSE)
ccfr <- static_cfr(ebola1976, correct_for_delays = TRUE, onset_to_death_ebola)

# Print nicely formatted case fatality rate estimates
format_cfr_neatly(ncfr)
#> [1] "CFR: 0.96% (95% CI: 0.92% - 0.98%)"
format_cfr_neatly(ccfr)
#> [1] "CFR: 0.96% (95% CI: 0.84% - 1.00%)"
```

Calculate and plot real-time CFR estimates up to a given point in time

``` r
# Calculate naive and corrected static CFRs up to a given point in time
df_ncfr <- rolling_cfr(ebola1976, correct_for_delays = FALSE)
df_ccfr <- rolling_cfr(
  ebola1976,
  correct_for_delays = TRUE,
  onset_to_death_ebola
)

# Plotting case and death data along with CFRs
plot_data_and_cfr(df_ncfr, df_ccfr)
#> Warning: Removed 1 row containing missing values (`geom_line()`).
```

<img src="man/figures/README-example-ebola-plot-1.png" width="100%" />

This package is currently a *concept*, as defined by the [RECON software
lifecycle](https://www.reconverse.org/lifecycle.html). This means that
essential features and mechanisms are still being developed, and the
package is not ready for use outside of the development team.
