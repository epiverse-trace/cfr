#' @title Estimate a severity measure that varies over time
#'
#' @description Calculates how the severity of a disease changes over time,
#' corrected for a user-specified delay. If cases are supplied, and the delay
#' distribution representing the delay between case detection and death, then
#' a case fatality risk over time is estimated.
#'
#' @inheritParams estimate_static
#' @param burn_in_value A single integer value for the number of time-points
#' (typically days) to disregard at the start of the time-series, if a burn-in
#' period is desired.
#' The default value is set to the mean of the distribution passed to the
#' `epidist` argument. This assumes that the temporal resolution is daily, and
#' the `<epidist>` passed is parameterised
#' (see [epiparameter::is_parameterised()]).
#' Defaults to 7 if no `<epidist>` is provided, or if the `<epidist>` is not
#' parameterised. This is a sensible default value that disregards the first
#' week of cases and deaths, assuming daily data.
#'
#' To consider all case data including the start of the time-series, set this
#' argument to 1.
#'
#' @param smooth_inputs A single logical value for whether the case and death
#' time-series data should be smoothed, using a rolling median procedure
#' before calculating the time-varying severity. This may be useful for noisy
#' time-series or time-series with strong reporting (e.g., weekend) effects.
#'
#' @param smoothing_window An _odd_ number determining the smoothing window size
#' to use when smoothing the case and death time-series, using a rolling median
#' procedure (as the `k` argument to [stats::runmed()]) before calculating the
#' time-varying severity.
#' The default value is 1 for _no smoothing_. Values > 1 apply smoothing.
#'
#' @return A `<data.frame>` containing the MLE estimate and 95% confidence
#' interval of the corrected severity.
#'
#' @details
#' # Details: Adjusting for delays between two time series
#' This function estimates the number of cases which have a known outcome over
#' time, following Nishiura et al. (2009).
#' The function calculates a quantity \eqn{k_t} for each day within the input
#' data, which represents the number of cases with a known outcome, on day
#' \eqn{t}. \eqn{k_t} is calculated in the following way:
#' \deqn{
#'  k_t = \sum_{j = 0}^t c_t f_{j - t}.
#' }
#' We then assume that the severity measure, for example CFR, of interest is
#' binomially distributed, in the following way:
#' \deqn{
#'  d_t \sim \text{Binomial}(k_t, \theta_t)
#' }
#' We use maximum likelihood estimation to determine the value of \eqn{\theta_t}
#' for each \eqn{t}, where \eqn{\theta} represents the severity measure of
#' interest.
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
#' df_covid_uk_subset <- subset(df_covid_uk, date <= "2020-05-31")
#'
#' # load epidist object from {epiparameter}
#' onset_to_death_covid <- epiparameter::epidist_db(
#'   disease = "COVID-19",
#'   epi_dist = "onset_to_death",
#'   author = "Linton_etal",
#'   single_epidist = TRUE
#' )
#'
#' # estimate time varying severity without correcting for delays
#' cfr_time_varying <- estimate_time_varying(
#'   data = df_covid_uk_subset,
#'   smooth_inputs = TRUE,
#'   burn_in_value = 7L,
#'   correct_for_delays = FALSE
#' )
#' # View
#' tail(cfr_time_varying)
#'
#' # estimate time varying severity while correcting for delays
#' cfr_time_varying <- estimate_time_varying(
#'   data = df_covid_uk_subset,
#'   epidist = onset_to_death_covid,
#'   smooth_inputs = TRUE,
#'   burn_in_value = 7L,
#'   correct_for_delays = TRUE
#' )
#' tail(cfr_time_varying)
#'
estimate_time_varying <- function(data,
                                  correct_for_delays = TRUE,
                                  epidist,
                                  burn_in_value = get_default_burn_in(epidist),
                                  smooth_inputs = FALSE,
                                  smoothing_window = 1) {
  # input checking
  checkmate::assert_logical(smooth_inputs, len = 1L, any.missing = FALSE)
  checkmate::assert_logical(correct_for_delays, len = 1L, any.missing = FALSE)
  checkmate::assert_integerish(burn_in_value, lower = 1, len = 1L)
  checkmate::assert_data_frame(data)
  checkmate::assert_integerish(smoothing_window, lower = 1, len = 1L)

  stopifnot(
    "Case data must contain columns `cases` and `deaths`" =
      (all(c("cases", "deaths") %in% colnames(data))),
    "`smoothing_window` must be an odd number greater than 0" =
      (smoothing_window %% 2 != 0)
  )
  if (correct_for_delays) {
    checkmate::assert_class(epidist, "epidist")
  }

  # prepare a new dataframe with smoothed columns if requested
  # all temporary operations are performed on df_temp,
  # data is returned with only three new columns added, and no other changes
  df_temp <- data
  # smooth cases if requested
  if (smooth_inputs) {
    df_temp$cases <- stats::runmed(data$cases,
      k = smoothing_window,
      endrule = "keep"
    )

    df_temp$deaths <- stats::runmed(data$deaths,
      k = smoothing_window,
      endrule = "keep"
    )
  } else {
    smoothing_window <- 0
  }

  cases <- df_temp$cases

  case_times <- as.numeric(df_temp$date - min(df_temp$date, na.rm = TRUE),
    units = "days"
  ) + 1

  case_length <- length(case_times)

  ##### prepare data.frame for severity estimation ####
  # create columns with NA values for later assignment
  # when not correcting for delays, set known outcomes to cases
  # this is to avoid if-else ladders
  df_temp$known_outcomes <- df_temp$cases

  # assign columns for severity estimate and intervals
  data$severity_mean <- NA_real_
  data$severity_low <- NA_real_
  data$severity_high <- NA_real_

  # calculation of indices to modify seems questionable
  indices <- seq(case_length - smoothing_window, burn_in_value, -1)
  if (correct_for_delays) {
    pmf_vals <- stats::density(
      epidist,
      at = seq(from = 0, to = nrow(data) - 1L)
    )
    df_temp[indices, "known_outcomes"] <- vapply(
      X = indices,
      FUN = function(x) {
        delay_pmf_eval <- pmf_vals[case_times[seq_len(x - burn_in_value)]]
        known_onsets_current <- cases[seq_len(x - burn_in_value)] *
          rev(delay_pmf_eval)

        # return rounded sum of known_onsets_current
        round(sum(known_onsets_current, na.rm = TRUE))
      },
      FUN.VALUE = numeric(1)
    )
  }

  #### Get severity estimates ####
  for (i in indices) {
    # handle case where deaths are fewer than non-zero known outcomes
    if (df_temp$deaths[i] <= df_temp$known_outcomes[i] &&
      isTRUE(df_temp$known_outcomes[i] > 0)) {
      severity_current_estimate <- stats::binom.test(
        df_temp$deaths[i],
        df_temp$known_outcomes[i]
      )

      data$severity_mean[i] <- severity_current_estimate$estimate[[1]]
      data$severity_low[i] <- severity_current_estimate$conf.int[[1]]
      data$severity_high[i] <- severity_current_estimate$conf.int[[2]]
    }
  }

  # remove known outcomes column as this is not expected as a side effect
  data$known_outcomes <- NULL

  # return data
  data
}

#' Get a default burn-in value from a delay distribution
#'
#' @inheritParams estimate_time_varying
#'
#' @return A single integer, the burn-in value.
#' @keywords internal
get_default_burn_in <- function(epidist) {
  default_value <- 7L
  if (!missing(epidist) && epiparameter::is_parameterised(epidist)) {
    # hardcoded access to mean as epiparameter::get_parameter()
    # returns distr-specific params (e.g. meanlog, shape)
    x <- do.call(
      epiparameter::convert_params_to_summary_stats,
      append(
        list(stats::family(epidist)),
        epiparameter::get_parameters(epidist)
      )
    )[["mean"]]
    x <- as.integer(round(x))
  } else {
    x <- default_value
  }

  # return x
  x
}
