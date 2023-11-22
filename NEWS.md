# cfr (development version)

# cfr 0.1.0

Initial CRAN submission of _cfr_, an R package to estimate the severity of a disease and ascertainment of cases while correcting for delays in outcomes of reported cases being known.

This release includes:

1. Functions for the overall severity of an outbreak, the overall severity of an outbreak estimated with an expanding time series of data, and the time-varying severity of an outbreak,
2. A function to estimate the number of outcomes to be expected from a given number of cases assuming a user-specified distribution of delays between cases and outcomes being known,
3. A function to estimate the overall (static) ascertainment of cases in an outbreak by comparing the relevant severity measures against a user-specified baseline severity,
4. A data preparation generic with an S3 method for the `<incidence2>` class from the _incidence2_ package,
5. Example daily case and death data from the 1976 Ebola Virus Disease outbreak as reported in Camacho et al. (2014). <https://doi.org/10.1016/j.epidem.2014.09.003>,
6. Example daily case and death data from the Covid-19 pandemic over the range 2020-01-01 to 2022-12-31 from the 19 countries with over 100,00 deaths over this period, as taken from the _covidregionaldata_ package which is no longer on CRAN,
7. Vignettes describing how to get started with severity estimation, and more detailed workflows on different kinds of severity estimation,
8. A vignette on working with data from the _incidence2_ package, and a vignette on working with delay distributions,
9. 100% code coverage,
10. Workflows to render the vignettes and README as a website.

## Correction

_cfr_ v0.1.0 only includes functionality for static ascertainment calculations. The functionality for time-varying ascertainment is expected to be included in future versions, and an older implementation was removed just prior to release. The news section for _cfr_ v0.1.0 has been updated to reflect this.
