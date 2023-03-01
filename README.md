
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

The goal of datadelay is to provide simple, fast methods for estimation of disease severity and under-reporting in real-time, accounting for delays in epidemic timeseries.

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

# Get the onset to death distribution for ebola virus disease
# from the {epiparameter} package
onset_to_death_ebola <- epiparameter::epidist(
  pathogen = "ebola",
  delay_dist = "onset_to_death"
)

# Access the probability mass function
delay_pmf <- onset_to_death_ebola$pmf

# Calculate the static naive and corrected CFRs
ncfr <- static_cfr(ebola1976, correct_for_delays = FALSE)
ccfr <- static_cfr(ebola1976, correct_for_delays = TRUE, delay_pmf)

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
  delay_pmf
)

# Plotting case and death data along with CFRs
plot_data_and_cfr(df_ncfr, df_ccfr)
```

<img src="man/figures/README-example-ebola-plot-1.png" width="100%" />

## Development

This package is currently a *concept*, as defined by the [RECON software
lifecycle](https://www.reconverse.org/lifecycle.html). This means that
essential features and mechanisms are still being developed, and the
package is not ready for use outside of the development team.
