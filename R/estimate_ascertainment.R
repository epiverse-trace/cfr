#' Estimate the ascertainment ratio of a disease
#'
#' @description Estimates the proportion of cases or infections that have been
#' ascertained, given a time-series of cases and deaths, a delay distribution
#' and a baseline severity estimate. The resulting ascertainment estimate is
#' calculated as the ratio of the baseline severity estimate, which is assumed
#' to be the 'true' disease severity, and the delay-adjusted severity estimate.
#'
#' @inheritParams cfr_static
#'
#' @param severity_baseline A single number in the range 0.0 -- 1.0 for the
#' assumed true baseline severity estimate used to estimate the overall
#' ascertainment ratio. Missing by default, which causes the function to error;
#' must be supplied by the user.
#'
#' @return A `<data.frame>` containing the maximum likelihood estimate estimate
#' and 95% confidence interval of the corrected severity, named
#' "ascertainment_estimate" (for the central estimate), and "ascertainment_low"
#' and "ascertainment_high" for the lower and upper interval limits.
#'
#' @details
#' `estimate_ascertainment()` uses [cfr_static()] internally to obtain a
#' severity estimate that is compared against the user-specified baseline
#' severity. The profile likelihood method used to obtain the severity estimate
#' is decided by the internal function `.estimate_severity()` as used in
#' [cfr_static()], when delay correction is applied. See the [cfr_static()]
#' documentation for an explanation of the methods used depending on outbreak
#' size and initial severity guess.
#'
#' @export
#'
#' @examples
#' # get data pre-loaded with the package
#' data("covid_data")
#' df_covid_uk <- covid_data[covid_data$country == "United Kingdom", ]
#'
#' df_covid_uk_subset <- subset(df_covid_uk, date <= "2020-05-31")
#'
#' # use a severity baseline of 1.4% (0.014) taken from Verity et al. (2020)
#' # Lancet Infectious Diseases: <https://doi.org/10.1016/S1473-3099(20)30243-7>
#'
#' # use onset-to-death distribution from Linton et al. (2020)
#' # J. Clinical Medicine: <https://doi.org/10.3390/jcm9020538>
#'
#' # subset data until 30th June 2020
#' data <- df_covid_uk[df_covid_uk$date <= "2020-06-30", ]
#' estimate_ascertainment(
#'   data = data,
#'   delay_density = function(x) dlnorm(x, meanlog = 2.577, sdlog = 0.440),
#'   severity_baseline = 0.014
#' )
#'
estimate_ascertainment <- function(data,
                                   severity_baseline,
                                   delay_density = NULL) {
  # input checking
  # expect rows more than burn in value
  checkmate::assert_data_frame(data, min.cols = 3, min.rows = 1)
  # check that input `<data.frame>` has columns date, cases, and deaths
  checkmate::assert_names(
    colnames(data),
    must.include = c("date", "cases", "deaths")
  )
  # check for any NAs among data
  checkmate::assert_data_frame(
    data[, c("date", "cases", "deaths")],
    any.missing = FALSE
  )

  checkmate::assert_number(
    severity_baseline,
    lower = 0.0, upper = 1.0, finite = TRUE
  )

  # NOTE: delay_density is checked in estimate_outcomes() if passed and not NULL

  # switch the output based on user specified type
  df_severity <- cfr_static(
    data,
    delay_density = delay_density
  )

  # data.frame for exports, first scale values by the 1/severity baseline
  # then ensure maximum is 1.0
  df_ascertainment <- severity_baseline / df_severity
  # throw a warning for ascertainment ration > 1.0
  if (any(df_ascertainment > 1.0)) {
    warning(
      "Ascertainment ratios > 1.0 detected, setting these values to 1.0"
    )
  }
  df_ascertainment[df_ascertainment > 1.0] <- 1.0

  # reset row numbers, which might be confusing
  rownames(df_ascertainment) <- NULL

  # re-convert to data.frame from list
  # here, the estimate called "severity_estimate" translates to
  # "ascertainment_estimate"
  # and the estimate "severity_high" translates to "ascertainment_lo"
  colnames(df_ascertainment) <- c(
    "ascertainment_estimate", "ascertainment_high", "ascertainment_low"
  )

  # return data with columns in correct order
  df_ascertainment[, c(
    "ascertainment_estimate", "ascertainment_low", "ascertainment_high"
  )]
}
