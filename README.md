
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

# assign a location
ebola1976$location <- "DRC"

# read epidist for EVD onset to death from {epiparameter}
# accesses parameters reported in https://doi.org/10.1016/S0140-6736(18)31387-4
onset_to_death_ebola <- epiparameter::epidist_db(
  disease = "Ebola Virus Disease",
  epi_dist = "onset_to_death",
  author = "Barry_etal"
)
#> Using Barry et al. (2018) <10.1016/S0140-6736(18)31387-4> PMID: 30047375. 
#> To retrieve the short citation use the 'get_citation' function

# Calculate the static naive and corrected CFRs
ncfr <- estimate_static(
  df_in = ebola1976, correct_for_delays = FALSE, location = "location"
)
ccfr <- estimate_static(
  df_in = ebola1976,
  correct_for_delays = TRUE,
  epi_dist = onset_to_death_ebola,
  location = "location"
)

# Print nicely formatted case fatality rate estimates
format_output(ncfr, estimate_type = "severity")
#>   Location                         Estimate
#> 1      DRC 95.51% (95% CI: 92.11% - 97.74%)
format_output(ccfr, estimate_type = "severity")
#>   Location                          Estimate
#> 1      DRC 95.90% (95% CI: 84.20% - 100.00%)
```

Calculate and plot real-time CFR estimates up to a given point in time

``` r
# Calculate naive and corrected static CFRs up to a given point in time
df_ncfr <- estimate_time_varying(
  df_in = ebola1976, correct_for_delays = FALSE,
  burn_in_value = 7
)

df_ccfr <- estimate_time_varying(
  ebola1976,
  correct_for_delays = TRUE,
  epi_dist = onset_to_death_ebola,
  burn_in_value = 7
)

# plot case and death data
plot_case_data(df_ccfr)
```

<img src="man/figures/README-example-ebola-plot-1.png" width="100%" />

``` r
plot_death_data(df_ccfr)
```

<img src="man/figures/README-example-ebola-plot-2.png" width="100%" />

``` r

# Plotting case and death data along with CFRs
plot_time_varying(df_ncfr, lower = 0, upper = 100)
```

<img src="man/figures/README-example-ebola-plot-3.png" width="100%" />

This package is currently a *concept*, as defined by the [RECON software
lifecycle](https://www.reconverse.org/lifecycle.html). This means that
essential features and mechanisms are still being developed, and the
package is not ready for use outside of the development team.
