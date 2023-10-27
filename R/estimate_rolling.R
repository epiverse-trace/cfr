#' Estimate static severity for an expanding time series
#'
#' @description Calculates the CFR at each time point in the case and death time
#' series supplied, using an expanding window of time. The static CFR is
#' calculated for each time point, using the time series from the start to each
#' time point, and increasing the number of time points included by one in each
#' iteration.
#'
#' @details When delay correction is applied by passing a delay distribution
#' density function to `delay_density`, the internal function
#' [estimate_severity()] is used to calculate the rolling severity.
#'
#' Note that in the naive method the severity estimate and confidence intervals
#' cannot be calculated for days on which the cumulative number of cases since
#' the start of the time-series, and for days on which the cumulative number of
#' deaths reported exceeds the cumulative reported cases, and is returned as
#' `NA`.
#'
#' @inheritParams cfr_static
#'
#' @return A `<data.frame>` with the date, maximum likelihood estimate and 95%
#' confidence interval of the daily severity estimates, named
#' "severity_mean", "severity_low", and "severity_high", with one row for each
#' day in the original data.frame.
#' @export
#'
#' @examples
#' # load package data
#' data("ebola1976")
#'
#' # estimate severity without correcting for delays
#' cfr_static(ebola1976)
#'
#' # estimate severity for each day while correcting for delays
#' # obtain onset-to-death delay distribution parameters from Barry et al. 2018
#' # view only the first values
#' estimate <- cfr_rolling(
#'   ebola1976,
#'   delay_density = function(x) dgamma(x, shape = 2.40, scale = 3.33)
#' )
#'
#' head(estimate)
#'
cfr_rolling <- function(data,
                        delay_density = NULL,
                        poisson_threshold = 100) {
  # input checking
  # input checking
  checkmate::assert_data_frame(
    data,
    min.rows = 1, min.cols = 3
  )
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
  # check that data$date is a date column
  checkmate::assert_date(data$date, any.missing = FALSE, all.missing = FALSE)
  # check for excessive missing date and throw an error
  # also check delay_density
  stopifnot(
    "Input data must have sequential dates with none missing or duplicated" =
      identical(unique(diff(data$date)), 1), # use numeric 1, not integer
    # this solution works when df$date is `Date`
    # this may need more thought for dates that are integers, POSIXct,
    # or other units; consider the units package
    "`delay_density` must be a function evaluating distribution density at a
    vector of values and returning a numeric vector of the same length.
    E.g. function(x) stats::dgamma(x = x, shape = 5, scale = 1)" =
      (checkmate::test_function(delay_density) &&
        checkmate::test_numeric(delay_density(seq(10)),
          lower = 0,
          any.missing = FALSE, finite = TRUE, len = 10L
        )) || is.null(delay_density)
  )
  checkmate::assert_count(poisson_threshold)

  # prepare cumulative sums
  cumulative_cases <- cumsum(data$cases)
  cumulative_deaths <- cumsum(data$deaths)

  if (!is.null(delay_density)) {
    # calculating the total number of cases and deaths after correcting for
    # the number of cases with estimated outcomes and using this estimate as the
    # of deaths
    data <- estimate_outcomes(
      data = data,
      delay_density = delay_density
    )

    cumulative_outcomes <- cumsum(data$estimated_outcomes)

    # generate series of CFR estimates with expanding time window
    severity_estimates <- Map(
      cumulative_cases, cumulative_deaths, cumulative_outcomes,
      f = estimate_severity, poisson_threshold = poisson_threshold
    )

    # bind list elements together
    severity_estimates <- do.call(rbind, severity_estimates)
  } else {
    # check for good indices
    indices <- which(
      cumulative_deaths <= cumulative_cases &
        cumulative_cases > 0
    )

    # subset the good cumulative data
    cumulative_cases <- cumulative_cases[indices]
    cumulative_deaths <- cumulative_deaths[indices]

    # prepare holding matrix
    severity_estimates <- matrix(NA_real_, nrow = nrow(data), ncol = 3)
    colnames(severity_estimates) <- c(
      "severity_mean", "severity_low", "severity_high"
    )

    # calculating the uncorrected CFR rolling over all days
    severity_estimates[indices, "severity_mean"] <- cumulative_deaths /
      cumulative_cases

    cfr_lims <- Map(
      cumulative_deaths, cumulative_cases,
      f = stats::binom.test, p = 1
    )
    # bind list elements together
    cfr_lims <- lapply(cfr_lims, `[[`, "conf.int")
    cfr_lims <- do.call(rbind, cfr_lims)

    # assign to matrix
    severity_estimates[indices, c("severity_low", "severity_high")] <- cfr_lims

    # process into a data.frame and return
    # bind single row data.frames and return, convert to data.frame when
    # matrix is returned from no delay correction
    severity_estimates <- as.data.frame(severity_estimates)
  }

  # add date
  severity_estimates$date <- data$date

  # return severity estimate with names in correct order
  severity_estimates[, c(
    "date", "severity_mean", "severity_low", "severity_high"
  )]
}
