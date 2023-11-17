#' @title Estimate a severity measure that varies over time
#'
#' @description Calculates how the severity of a disease changes over time
#' while optionally correcting for reporting delays using an epidemiological
#' delay distribution of the time between symptom onset and outcome
#' (e.g. onset-to-death for the fatality risk).
#'
#' @inheritParams cfr_static
#' @param burn_in A single integer-like value for the number of time-points
#' (typically days) to disregard at the start of the time-series, if a burn-in
#' period is desired.
#'
#' Defaults to 7, which is a sensible default value that disregards the first
#' week of cases and deaths, assuming daily data.
#'
#' To consider all case data including the start of the time-series, set this
#' argument to 0.
#'
#' @param smoothing_window An _odd_ number determining the smoothing window size
#' to use when smoothing the case and death time-series, using a rolling median
#' procedure (as the `k` argument to [stats::runmed()]) before calculating the
#' time-varying severity.
#'
#' The default behaviour is to apply no smoothing. The minimum value of this
#' argument is 1.
#'
#' @return A `<data.frame>` with the date, maximum likelihood estimate and 95%
#' confidence interval of the daily severity estimates, named
#' "severity_mean", "severity_low", and "severity_high", with one row for each
#' day in the original data.frame.
#'
#' @details
#' # Details: Adjusting for delays between two time series
#'
#' This function estimates the number of cases which have a known outcome over
#' time, following Nishiura et al. (2009).
#' The function calculates a quantity \eqn{k_t} for each day within the input
#' data, which represents the number of cases estimated to have a known outcome,
#' on day \eqn{t}. \eqn{k_t} is calculated in the following way:
#' \deqn{k_t = \sum_{j = 0}^t c_t f_{j - t}}
#'
#' We then assume that the severity measure, for example CFR, of interest is
#' binomially distributed, in the following way:
#'
#' \deqn{d_t \sim {\sf Binomial}(k_t, \theta_t)}
#'
#' We use maximum likelihood estimation to determine the value of \eqn{\theta_t}
#' for each \eqn{t}, where \eqn{\theta} represents the severity measure of
#' interest.
#'
#' The epidemiological delay distribution passed to `epidist` is used to obtain
#' a probability mass function parameterised by time; i.e. \eqn{f(t)} which
#' gives the probability a case has a known outcomes (usually, death) at time
#' \eqn{t}, parameterised with disease-specific parameters before it is supplied
#' here.
#'
#' @references
#' Nishiura, H., Klinkenberg, D., Roberts, M., & Heesterbeek, J. A. P. (2009).
#' Early Epidemiological Assessment of the Virulence of Emerging Infectious
#' Diseases: A Case Study of an Influenza Pandemic. PLOS ONE, 4(8), e6852.
#' \doi{10.1371/journal.pone.0006852}
#'
#' @export
#'
#' @examples
#' # get data pre-loaded with the package
#' data("covid_data")
#' df_covid_uk <- covid_data[covid_data$country == "United Kingdom", ]
#'
#' # estimate time varying severity without correcting for delays
#' cfr_time_varying <- cfr_time_varying(
#'   data = df_covid_uk,
#'   burn_in = 7L
#' )
#' # View
#' tail(cfr_time_varying)
#'
#' # estimate time varying severity while correcting for delays
#' # obtain onset-to-death delay distribution parameters from Linton et al. 2020
#' # J. Clinical Medicine: <https://doi.org/10.3390/jcm9020538>
#' # view only the first values
#' cfr_time_varying <- cfr_time_varying(
#'   data = df_covid_uk,
#'   delay_density = function(x) dlnorm(x, meanlog = 2.577, sdlog = 0.440),
#'   burn_in = 7L
#' )
#' tail(cfr_time_varying)
#'
cfr_time_varying <- function(data,
                             delay_density = NULL,
                             burn_in = 7,
                             smoothing_window = NULL) {
  # input checking
  # zero count allowed to include all data
  checkmate::assert_count(burn_in)

  # expect rows more than burn in value
  checkmate::assert_data_frame(data, min.cols = 3, min.rows = burn_in + 1)
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
  checkmate::assert_count(smoothing_window, null.ok = TRUE)

  stopifnot(
    "Input data must have sequential dates with none missing or duplicated" =
      identical(unique(diff(data$date)), 1), # use numeric 1, not integer
    "`smoothing_window` must be an odd number greater than 0" =
      (smoothing_window %% 2 != 0),
    "`delay_density` must be a function with a single required argument,
    and evaluating distribution density at a vector of values and returning a
    numeric vector of the same length.
    E.g. function(x) stats::dgamma(x = x, shape = 5, scale = 1)" =
      (test_fn_req_args(delay_density) &&
        test_fn_num_out(delay_density)) || is.null(delay_density)
  )

  # prepare a new dataframe with smoothed columns if requested
  # all temporary operations are performed on df_temp,
  # data is returned with only three new columns added, and no other changes
  df_temp <- data
  # smooth cases if requested
  if (is.null(smoothing_window)) {
    # set to 0 for internal use only --- see below
    smoothing_window <- 0
  } else {
    # smooth data if requested
    df_temp$cases <- stats::runmed(
      data$cases,
      k = smoothing_window,
      endrule = "keep"
    )

    df_temp$deaths <- stats::runmed(
      data$deaths,
      k = smoothing_window,
      endrule = "keep"
    )
  }

  cases <- df_temp$cases

  case_times <- as.numeric(df_temp$date - min(df_temp$date, na.rm = TRUE),
    units = "days"
  ) + 1

  case_length <- length(case_times)

  ##### prepare matrix for severity estimation ####
  # when not correcting for delays, set estimated no. of known outcomes to cases
  # this is to avoid if-else ladders
  df_temp$estimated_outcomes <- df_temp$cases

  # assign columns for severity estimate and intervals
  severity_estimates <- matrix(
    data = NA_real_, nrow = nrow(data), ncol = 3,
    dimnames = list(NULL, sprintf("severity_%s", c("mean", "low", "high")))
  )

  # calculation of indices to modify
  # start from the final index to be smoothed
  # end at the first row after the burn-in number of rows (days)
  # TODO: check if modifying start point is necessary for runmed "keep" endrule
  indices <- seq(case_length - smoothing_window, burn_in + 1, -1)
  if (!is.null(delay_density)) {
    pmf_vals <- delay_density(seq(from = 0, to = nrow(data) - 1L))

    df_temp[indices, "estimated_outcomes"] <- round(.calc_expected_outcomes(
        cases = df_temp$cases, pmf_vals = pmf_vals, offset = burn_in,
        indices = indices
      )
    )
  }

  #### Get severity estimates ####
  # handle case where deaths are fewer than non-zero estimated (known) outcomes
  # and select indices which are not smoothed or excluded by burn in
  # this reduces the number of indices over which to run the binomial test
  # estimated_outcomes are not allowed to be NA
  indices <- intersect(
    indices,
    which(df_temp$deaths <= df_temp$estimated_outcomes &
      df_temp$estimated_outcomes > 0)
  )

  # binomial test at indices
  estimates_tmp <- vapply(indices, FUN = function(i) {
    severity_estimate <- stats::binom.test(
      df_temp$deaths[i],
      df_temp$estimated_outcomes[i]
    )

    # return a vector
    c(
      severity_estimate$estimate[[1]],
      severity_estimate$conf.int[[1]],
      severity_estimate$conf.int[[2]]
    )
  }, numeric(3))

  # replace the values at indices
  severity_estimates[indices, ] <- t(estimates_tmp)

  # create data frame
  severity_estimates <- as.data.frame(severity_estimates)
  # add date and return
  severity_estimates$date <- data$date

  # return severity estimate with names in correct order
  severity_estimates[, c(
    "date", "severity_mean", "severity_low", "severity_high"
  )]
}
