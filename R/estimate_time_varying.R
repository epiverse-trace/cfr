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
#'
#' @param burn_in_value The number of time-points (typically days) to disregard
#' at the start of the time-series, if a burn-in period is desired.
#' The default value is set to the mean of the central spread of the `epidist`
#' object passed to the function, assuming the temporal resolution is daily.
#' Alternatively, a sensible value might be 7, to disregard the first week of
#' cases and deaths.
#' To consider all case data including the start of the time-series, set this
#' argument to 0.
#'
#' @param smooth_inputs A boolean flag determining whether the user wishes to
#' smooth the case and death time-series, using a moving average procedure
#' before calculating the time-varying severity. Useful for noisey time-series
#' or time-series with strong reporting (e.g., weekend) effects
#'
#' @param smoothing_window A number determining the smoothing window size to use
#' when smoothing the case and death time-series, using a moving average
#' procedure before calculating the time-varying severity.
#' Useful for noisy time-series or time-series with strong reporting
#' (e.g., weekend) effects.
#' The default value is 1 for _no smoothing_. Values > 1 apply smoothing.
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
#'   burn_in_value = 7L,
#'   correct_for_delays = FALSE
#' )
#'
#' df_covid_cfr_uk_corrected <- estimate_time_varying(
#'   df_covid_uk_subset,
#'   epi_dist = onset_to_death_covid,
#'   smooth_inputs = TRUE,
#'   burn_in_value = 7L,
#'   correct_for_delays = TRUE
#' )
#'
estimate_time_varying <- function(df_in,
                                  epi_dist,
                                  burn_in_value = get_default_burn_in(epi_dist),
                                  smooth_inputs = FALSE,
                                  smoothing_window = 7,
                                  correct_for_delays = TRUE) {
  # TODO input checking
  checkmate::assert_logical(smooth_inputs, len = 1L, any.missing = FALSE)
  checkmate::assert_logical(correct_for_delays, len = 1L, any.missing = FALSE)
  checkmate::assert_integerish(burn_in_value, lower = 0, len = 1L)

  pmf_vals <- stats::density(
    epi_dist,
    at = seq(from = 0, to = nrow(df_in) - 1L)
  )

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

  ##### prepare data.frame for severity estimation ####
  # create columns with NA values for later assignment
  df_in$u_t <- numeric(case_length) # does not seem to be used?
  # when not correcting for delays, set known outcomes to cases
  # this is to avoid if-else ladders
  df_in$known_outcomes <- df_in$cases

  # assign columns for severity estimate and intervals
  df_in$severity_me <- NA_real_
  df_in$severity_lo <- NA_real_
  df_in$severity_hi <- NA_real_

  indices <- seq(case_length - smoothing_window, burn_in_value, -1)
  if (correct_for_delays) {
    df_in$known_outcomes[indices] <- vapply(
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
