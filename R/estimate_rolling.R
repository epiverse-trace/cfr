#' Estimate static severity for an expanding time series
#'
#' @description Calculates the CFR at each time point in the case and death time
#' series supplied, using an expanding window of time. The static CFR is
#' calculated for each time point, using the time series from the start to each
#' time point, and increasing the number of time points included by one in each
#' iteration.
#'
#' @details When delay correction is applied by passing an `<epidist>`, the
#' internal function [estimate_severity()] is used to calculate the rolling
#' severity.
#'
#' @inheritParams cfr_static
#'
#' @return A `<data.frame>` with the MLE and 95% confidence interval of the
#' daily severity estimates, named "severity_mean", "severity_low", and
#' "severity_high", with one row for each day in the original data.frame.
#' @export
#'
#' @examples
#' # load package data
#' data("ebola1976")
#'
#' # get an onset to death distribution from the {epiparameter} package
#' onset_to_death_ebola <- epiparameter::epidist_db(
#'   disease = "Ebola Virus Disease",
#'   epi_dist = "onset_to_death",
#'   author = "Barry_etal",
#'   single_epidist = TRUE
#' )
#'
#' # estimate severity without correcting for delays
#' cfr_static(ebola1976)
#'
#' # estimate severity for each day while correcting for delays
#' # view only the first values
#' estimate <- cfr_rolling(
#'   ebola1976,
#'   epidist = onset_to_death_ebola
#' )
#'
#' head(estimate)
#'
cfr_rolling <- function(data,
                        epidist,
                        poisson_threshold = 100) {

  # input checking
  checkmate::assert_data_frame(data)
  # check that input data.frame has columns date, cases, and deaths
  checkmate::assert_names(
    colnames(data),
    must.include = c("date", "cases", "deaths")
  )
  # check that data$date is a date column
  checkmate::assert_date(data$date, any.missing = FALSE, all.missing = FALSE)
  # check for excessive missing date and throw an error
  stopifnot(
    "Input data must have sequential dates with none missing or duplicated" =
      identical(unique(diff(data$date)), 1) # use numeric 1, not integer
    # this solution works when df$date is `Date`
    # this may need more thought for dates that are integers, POSIXct,
    # or other units; consider the units package
  )
  checkmate::assert_count(poisson_threshold)

  # prepare cumulative sums
  cumulative_cases <- cumsum(data$cases)
  cumulative_deaths <- cumsum(data$deaths)

  if (!missing(epidist)) {
    # check epidists
    checkmate::assert_class(epidist, "epidist")

    # calculating the total number of cases and deaths after correcting for
    # the number of cases with known outcomes and using this estimate as the
    # of deaths
    data <- known_outcomes(
      data = data,
      epidist = epidist
    )

    cumulative_outcomes <- cumsum(data$known_outcomes)

    # generate series of CFR estimates with expanding time window
    cfr_estimate <- Map(
      cumulative_cases, cumulative_deaths, cumulative_outcomes,
      f = estimate_severity, poisson_threshold = poisson_threshold
    )

    # bind list elements together
    cfr_estimate <- do.call(rbind, cfr_estimate)
  } else {
    # calculating the uncorrected CFR rolling over all days
    cfr_me <- cumulative_deaths / cumulative_cases

    cfr_lims <- Map(
      cumulative_deaths, cumulative_cases,
      f = stats::binom.test, p = 1
    )
    cfr_estimate <- Map(
      cfr_lims, cfr_me,
      f = function(bintest, me) {
        c(me, bintest[["conf.int"]])
      }
    )

    # bind list elements together
    cfr_estimate <- do.call(rbind, cfr_estimate)
    # process into a data.frame and return
    # bind single row data.frames and return, convert to data.frame when
    # matrix is returned from no delay correction
    cfr_estimate <- as.data.frame(cfr_estimate)
    # fix column names in the case of no delay correction
    colnames(cfr_estimate) <- c(
      "severity_mean", "severity_low", "severity_high"
    )
  }

  # return cfr estimate
  cfr_estimate
}
