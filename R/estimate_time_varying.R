#' Estimate known outcomes from case and death time-series data
#'
#' @description Calculates how the severity of a disease changes over time,
#' corrected for a user-specified delay. If cases are supplied, and the delay
#' distribution representing the delay between case detection and death, then
#' a case fatality ratio over time is estimated
#'
#' @param df_in A data.frame containing the outbreak data. A daily time series
#' with dates or some other absolute indicator of time (e.g. epiday/epiweek) and
#' the numbers of new cases and new deaths at each time point
#'
#' @param epi_dist The delay distribution used, in the form of an
#' [epiparameter::epidist()] object. This is used to obtain a probability
#' mass function parameterised by time; i.e. \eqn{f(t)} which gives the
#' probability a case has a known outcomes (i.e. death) at time \eqn{t},
#' parameterised with disease-specific parameters before it is supplied here.
#' A typical example would be a symptom onset to death delay distribution.
#'
#' @param burn_in A boolean flag to determine whether a burn in at the start
#' of time time-series should be used. Specifically, it askes whether the user
#' wishes to disregard the first [burn_in_arg] days of the time-series, given
#' that the calculation can produce noisey and uncertain estimates when case
#' and death numbers are both low, which is typical at the start of outbreaks
#'
#' @param burn_in_arg The number of time-points (typically days) to disregard
#' at the start of the time-series, if a burn-in period is desired. Default
#' value is set to 7, assuming the temporal resolution is daily
#'
#' @param smooth_inputs A boolean flag determining whether the user wishes to
#' smooth the case and death time-series, using a moving average procedure
#' before calculating the time-varying severity. Useful for noisey time-series
#' or time-series with strong reporting (e.g., weekend) effects
#'
#' @param correct_for_delays A boolean flag indicating whether the user wishes
#' to correct for the delay between case detection and death. FALSE corresponds
#' to a naive severity being calculated, TRUE corresponds to the user
#' calculating a corrected severity
#'
#' @return A data.frame containing the MLE estimate and 95% confidence interval
#' of the corrected severity
#'
#' @export
#'
#' @examples
#'
#' library(datadelay)
#' library(epiparameter)
#' library(covidregionaldata)
#' library(dplyr)
#'
#' df_covid_uk <- get_national_data(
#'   countries = "united kingdom", source = "who", verbose = FALSE
#' )
#' df_covid_uk <- dplyr::rename(
#'   df_covid_uk,
#'   cases = cases_new, deaths = deaths_new
#' )
#'
#' df_covid_uk_subset <- subset(df_covid_uk, date <= "2020-12-31")
#'
#' onset_to_death_covid <- epidist_db(
#'   disease = "COVID-19",
#'   epi_dist = "onset_to_death",
#'   author = "Linton_etal"
#' )
#'
#' df_covid_cfr_uk_naive <- estimate_time_varying(
#'   df_covid_uk_subset,
#'   epi_dist = onset_to_death_covid,
#'   smooth_inputs = TRUE,
#'   burn_in = TRUE,
#'   correct_for_delays = FALSE
#' )
#'
#' df_covid_cfr_uk_corrected <- estimate_time_varying(
#'   df_covid_uk_subset,
#'   epi_dist = onset_to_death_covid,
#'   smooth_inputs = TRUE,
#'   burn_in = TRUE,
#'   correct_for_delays = TRUE
#' )
#'
#' plot_time_varying(df_covid_cfr_uk_naive, lower = 0, upper = 5)
#' plot_time_varying(df_covid_cfr_uk_corrected, lower = 0, upper = 5)
#'
estimate_time_varying <- function(df_in,
                                  epi_dist,
                                  burn_in = TRUE,
                                  burn_in_arg = 7,
                                  smooth_inputs = FALSE,
                                  smoothing_window = 7,
                                  correct_for_delays = TRUE) {
  # TODO input checking
  checkmate::assert_logical(burn_in, len = 1L, any.missing = FALSE)
  checkmate::assert_logical(smooth_inputs, len = 1L, any.missing = FALSE)
  checkmate::assert_logical(correct_for_delays, len = 1L, any.missing = FALSE)

  pmf_vals <- stats::density(
    epi_dist,
    at = seq(from = 0, to = nrow(df_in) - 1L)
  )

  # assign default value to burn in and modify if requested
  burn_in_num <- 0
  if (burn_in && missing(burn_in_arg)) {
    burn_in_num <- round(epi_dist$summary_stats$centre_spread$mean)
  } else if (burn_in && checkmate::test_number(burn_in_arg, na.ok = FALSE)) {
    burn_in_num <- burn_in_arg
  }

  # smooth cases if requested
  if (smooth_inputs) {
    df_in$cases <- round(
      zoo::rollmean(df_in$cases, k = smoothing_window, fill = NA)
    )

    df_in$deaths <- round(
      zoo::rollmean(df_in$deaths, k = smoothing_window, fill = NA)
    )
  } else {
    smoothing_window <- 0
  }

  cases <- df_in$cases

  case_times <- as.numeric(df_in$date - min(df_in$date, na.rm = TRUE),
    units = "days"
  ) + 1

  case_length <- length(case_times)

  # create columns with NA values for later assignment
  df_in$known_outcomes <- numeric(case_length)
  df_in$u_t <- numeric(case_length) # does not seem to be used?

  # assign columns for severity estimate and intervals
  df_in$severity_me <- NA_real_
  df_in$severity_lo <- NA_real_
  df_in$severity_hi <- NA_real_

  # when not correcting for delays, set known outcomes to cases
  # this is to avoid if-else ladders
  df_in$known_outcomes <- df_in$cases

  # Compile onsets
  for (i in seq(case_length - smoothing_window, burn_in_num, -1)) {

    # handle case of correcting for delays - modify known outcomes
    # for potential use in the binomial test
    if (correct_for_delays) {
      for (i in seq(case_length - smoothing_window, burn_in_num, -1)) {
        # Delay probability mass function, evaluated at times
        # within the case and death times series
        delay_pmf_eval <- pmf_vals[case_times[seq_len(i - burn_in_num)]]

        # Estimate the number of onsets associated with deaths
        known_onsets_current <- cases[seq_len(i - burn_in_num)] *
          rev(delay_pmf_eval)

        # Collecting all current known onset estimates in a new
        # column of the original data.frame
        df_in$known_outcomes[i] <- round(
          sum(known_onsets_current, na.rm = TRUE)
        )
      }
    }

    # handle case where deaths are fewer than non-zero known outcomes
    if (df_in$deaths[i] <= df_in$known_outcomes[i] &&
      isTRUE(df_in$known_outcomes[i] > 0)) {
      severity_current_estimate <- stats::binom.test(
        df_in$deaths[i],
        df_in$known_outcomes[i]
      )

      df_in$severity_me[i] <- severity_current_estimate$estimate[[1]]
      df_in$severity_lo[i] <- severity_current_estimate$conf.int[[1]]
      df_in$severity_hi[i] <- severity_current_estimate$conf.int[[2]]
    }
  }

  # remove known outcomes column as this is not expected as a side effect
  df_in$known_outcomes <- NULL

  # return data
  df_in
}
