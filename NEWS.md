# cfr (development version)

## Functions

1. `cfr_static()`:

    - Has more informative checks on intermediate values passed to `.estimate_severity()`.

    - Prints a message when CFR value cannot be determined or are unreliable.

2. `cfr_rolling()`:

    - Prints a message explaining that this is a convenience function.

    - Has improved input checking.

    - Uses new `.estimate_severity()` functionality based on outbreak size and initial expectation of CFR.

    - Prints a message when some rolling CFR values cannot be determined or are unreliable.

3. `.estimate_severity()`:

    - Renamed with `.` prefix to indicate internal function.

    - Added parameter `p_mid` for initial severity estimate, which is used to determine the likelihood approximation method.

    - Selects from among Binomial, Poisson, and Normal approximation of the likelihood depending on the outbreak size and `p_mid` using the function `.select_fun_likelihood()`; prints a message with the selected method.

    - Lowest possible severity estimate is reduced to $10^{-4}$.

    - Severity estimates and confidence intervals stored as named vectors rather than a `<data.frame>`.

4. `.select_func_likelihood()`: Internal function added that chooses likelihood approximation function based on outbreak size and `p_mid`.

    - Binomial approximation used for small outbreaks with cumulative cases lower than the Poisson threshold.
    
    - Poisson approximation used for outbreaks above the Poisson threshold and with `p_mid` < 0.05.

    - Normal approximation used for outbreaks above the Poisson threshold and with `p_mid` > 0.05.

4. `test_fn_req_args()` is updated to use `Reduce(f = "+")` and `Map()` rather than `sum(mapply())`.

## Documentation

1. Added package level documentation.

2. Updated Readme with lifecycle (stable) and RepoStatus (Active) badges, added DPG badge, and updated the related projects section; corrected figure labelling.

3. Updated `_pkgdown.yaml` with reference sections, and added a software permissions vignette.

4. Updated `WORDLIST`.

5. Updated all function documentation files.

6. Added section in distributions vignette on when it is acceptable to use continuous rather than discrete distributions.

## Tests

1. All snapshots are updated with severity values from new likelihood functions.

2. Added session global state checker script and setup options script.

3. All tests are updated to reflect that functions will sometimes throw informative messages.

## Package

1. Added GitHub Actions workflows for dependency changes, linting, updating the citation file, and updating the license year.

2. Updated other GHA workflows and infrastructure files to match the latest versions on `epiverse-trace/packagetemplate`.

3. Normalised `DESCRIPTION` file.

4. Added `tools/check.env` from `epiverse-trace/packagetemplate` to suppress specific checks on package size, Rd cross references, GNU Make requirement, and non-ASCII strings.

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
