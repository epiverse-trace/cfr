#' Estimate known outcomes from case and death time-series data
#'
#' @description Calculates how the severity of a disease changes over time,
#' corrected for a user-specified delay. If cases are supplied, and the delay
#' distribution representing the delay between case detection and death, then
#' a case fatality ratio over time is estimated
#'
#' @inheritParams estimate_static
#' @param burn_in_value The number of time-points (typically days) to disregard
#' at the start of the time-series, if a burn-in period is desired.
#' The default value is set to the mean of the central spread of the `<epidist>`
#' object passed to the function, assuming the temporal resolution is daily.
#' Alternatively, a sensible value might be 7, to disregard the first week of
#' cases and deaths.
#' To consider all case data including the start of the time-series, set this
#' argument to 1.
#'
#' @param smooth_inputs A boolean flag determining whether the user wishes to
#' smooth the case and death time-series, using a moving average procedure
#' before calculating the time-varying severity. Useful for noisy time-series
#' or time-series with strong reporting (e.g., weekend) effects
#'
#' @param smoothing_window An _odd_ number determining the smoothing window size
#' to use when smoothing the case and death time-series, using a moving average
#' procedure before calculating the time-varying severity.
#' Useful for noisy time-series or time-series with strong reporting
#' (e.g., weekend) effects.
#' The default value is 1 for _no smoothing_. Values > 1 apply smoothing.
#'
#' @return A data.frame containing the MLE estimate and 95% confidence interval
#' of the corrected severity
#'
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
#'   author = "Barry_etal"
#' )
#'
#' # estimate time varying severity without correcting for delays
#' estimate_time_varying(
#'   data = ebola1976,
#'   smooth_inputs = TRUE,
#'   burn_in_value = 7L,
#'   correct_for_delays = FALSE
#' )
#'
#' # estimate time varying severity while correcting for delays
#' estimate_time_varying(
#'   data = ebola1976,
#'   epi_dist = onset_to_death_ebola,
#'   smooth_inputs = TRUE,
#'   burn_in_value = 7L,
#'   correct_for_delays = TRUE
#' )
#'
estimate_time_varying <- function(data,
                                  epi_dist = NULL,
                                  burn_in_value = get_default_burn_in(epi_dist),
                                  smooth_inputs = FALSE,
                                  smoothing_window = 1,
                                  correct_for_delays = FALSE) {
  # TODO input checking
  checkmate::assert_logical(smooth_inputs, len = 1L, any.missing = FALSE)
  checkmate::assert_logical(correct_for_delays, len = 1L, any.missing = FALSE)
  checkmate::assert_integerish(burn_in_value, lower = 1, len = 1L)
  checkmate::assert_data_frame(data)
  checkmate::assert_logical(correct_for_delays, len = 1L)
  checkmate::assert_integerish(smoothing_window, lower = 1, len = 1L)
  checkmate::assert_class(epi_dist, "epidist", null.ok = TRUE) # optional arg.

  stopifnot(
    "Case data must contain columns `cases` and `deaths`" =
      (all(c("cases", "deaths") %in% colnames(data))),
    "`smoothing_window` must be an odd number greater than 0" =
      (smoothing_window %% 2 != 0)
  )
  if (correct_for_delays) {
    checkmate::assert_class(epi_dist, "epidist")
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
  data$severity_me <- NA_real_
  data$severity_lo <- NA_real_
  data$severity_hi <- NA_real_

  # calculation of indices to modify seems questionable
  indices <- seq(case_length - smoothing_window, burn_in_value, -1)
  if (correct_for_delays) {
    pmf_vals <- stats::density(
      epi_dist,
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

      data$severity_me[i] <- severity_current_estimate$estimate[[1]]
      data$severity_lo[i] <- severity_current_estimate$conf.int[[1]]
      data$severity_hi[i] <- severity_current_estimate$conf.int[[2]]
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
get_default_burn_in <- function(epi_dist = NULL) {
  if (is.null(epi_dist)) {
    7
  } else {
    as.integer(round(epi_dist$summary_stats$centre_spread$mean))
  }
}
