#' Rolling case fatality rate
#'
#' @description Calculates the CFR at each time point in the case and death time
#' series supplied, using an expanding window of time. The static CFR is
#' calculated for each time point, using the time series from the start to each
#' time point increasing the number of time points included by one each
#' iteration
#'
#' @param df_in A data.frame containing the outbreak data. A daily time series
#' with dates or some other absolute indicator of time (e.g. epiday/epiweek) and
#' the numbers of new cases and new deaths at each time point
#'
#' @param correct_for_delays A boolean flag indicating whether the user wishes
#' to correct for the delay between case detection and death. FALSE corresponds
#' to a naive CFR being calculated, TRUE corresponds to the user calculating a
#' corrected CFR
#'
#' @param delay_pmf The delay distribution used, in the form of a probability
#' mass function parameterised by time. I.e. f(t) which gives the probability a
#' case has a known outcomes (i.e. death) at time t, parameterised with
#' disease-specific parameters before it is supplied here. A typical example
#' would be a symptom onset to death delay distribution
#'
#' @return A data.frame containing the same case and death time series supplied
#' with the desired CFR (naive or corrected) appended at each time point
#' as extra columns
#' @export
#'
#' @examples
#' # Get onset to death distribution from epiparameter
#' onset_to_death_ebola <- epiparameter::epidist("ebola", "onset_to_death")$pmf
#'
#' # Calculate rolling naive CFR
#' df_ncfr <- rolling_cfr(ebola1976, correct_for_delays = FALSE)
#'
#' # Calculate rolling corrected CFR
#' df_ccfr <- rolling_cfr(
#'   df_in = ebola1976,
#'   correct_for_delays = TRUE,
#'   onset_to_death_ebola
#' )
rolling_cfr <- function(df_in,
                        correct_for_delays = TRUE,
                        delay_pmf) {

  # returns error message if no delay distribution is supplied, but correction
  # for delays was requested
  if (correct_for_delays && missing(delay_pmf)) {
    stop(
      "To correct for the delay between case detection and death,\
       please specify an onset-to-death (or similar) probability mass function"
    )
  }
  if (!missing(delay_pmf)) {
    stopifnot(
      "`delay_pmf` must be a function`" =
        (is.function(delay_pmf))
    )
  }
  stopifnot(
    "Case data must contain columns `cases` and `deaths`" =
      (all(c("cases", "deaths") %in% colnames(df_in)))
  )

  # calculating the number of days in the time series to be looped over
  n_days <- seq_len(nrow(df_in))

  df_cfr <- data.frame()
  # calculating the corrected CFR rolling over all days
  if (correct_for_delays) {
    df_cfr <- lapply(n_days, function(i) {
      static_cfr(
        df_in = df_in[seq_len(i), ],
        correct_for_delays = TRUE,
        delay_pmf = delay_pmf
      )
    })
  } else {
    # calculating the uncorrected CFR rolling over all days
    df_cfr <- lapply(n_days, function(i) {
      static_cfr(
        df_in = df_in[seq_len(i), ],
        correct_for_delays = FALSE
      )
    })
  }

  # bind the data together
  df_cfr <- data.table::setDF(data.table::rbindlist(lapply(df_cfr, as.list)))
  # attaching the dates, cases and deaths to the CFR estimates
  df_cfr <- cbind(df_cfr, df_in[, c("date", "cases", "deaths")])

  df_cfr
}
