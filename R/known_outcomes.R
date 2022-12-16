#' Estimate known outcomes from case and death time-series data
#'
#' @description Estimates the expected number of individuals with known outcomes
#' from a case and death time series of data of an outbreak, up to the time
#' point supplied.
#' Can either calculate the daily new number of known outcomes or the cumulative
#' number.
#' Uses the probability mass function representing the delay between
#' case detection and death, typically approximated by a symptom onset to death
#' distribution from the literature for the disease in question.
#'
#' @param df_in A data.frame containing the outbreak data. A daily time series
#' with dates or some other absolute indicator of time (e.g. epiday/epiweek) and
#' the numbers of new cases and new deaths at each time point
#'
#' @param delay_pmf The delay distribution used, in the form of a probability
#' mass function parameterised by time; i.e. \eqn{f(t)} which gives the
#' probability a case has a known outcomes (i.e. death) at time \eqn{t},
#' parameterised with disease-specific parameters before it is supplied here.
#' A typical example would be a symptom onset to death delay distribution.
#'
#' @param cumulative A boolean flag to indicate whether the user wants the daily
#' or total number of known outcomes
#'
#' @return A data.frame containing the MLE estimate and 95% confidence interval
#' of the corrected CFR
#' @export
#'
#' @examples
#' # Load Ebola 1976 outbreak data
#' data("ebola1976")
#'
#' # Access a relevant symptom onset to death distribution
#' onset_to_death_ebola <- epiparameter::epidist("ebola", "onset_to_death")
#' delay_pmf <- onset_to_death_ebola$pmf
#'
#' df_known_outcomes <- known_outcomes(df_in = ebola1976, delay_pmf)
known_outcomes <- function(df_in,
                           delay_pmf,
                           cumulative = TRUE) {
  # some input checking
  stopifnot(
    "Case data must be a data.frame" =
      (is.data.frame(df_in)),
    "Case data must contain columns `cases` and `deaths`" =
      (all(c("cases", "deaths") %in% colnames(df_in))),
    "Option `cumulative` must be `TRUE` or `FALSE`" =
      (is.logical(cumulative))
  )
  if (!missing(delay_pmf)) {
    stopifnot(
      "`delay_pmf` must be a function`" =
        (is.function(delay_pmf))
    )
  }

  # nolint start
  # Code following https://github.com/adamkucharski/ebola-cfr/
  # blob/e2683eee62fb6fef8140b4b1d9dc13d542c5eacb/R/cfr_function.R#L12-L22
  # nolint end
  pmf_vals <- delay_pmf(seq(from = 0, to = nrow(df_in) - 1L))
  expected_outcomes <- vapply(
    X = seq_along(df_in$cases),
    FUN = function(i) {
      sum(rev(df_in$cases[seq_len(i)]) * pmf_vals[seq_len(i)])
    },
    FUN.VALUE = numeric(1L)
  )

  # performing the cumulative sum as per the function argument. TRUE by default.
  if (cumulative) {
    df_in$known_outcomes <- cumsum(expected_outcomes)
  } else {
    df_in$known_outcomes <- expected_outcomes
  }

  df_in
}
