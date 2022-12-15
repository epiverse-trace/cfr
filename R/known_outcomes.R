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
#' onset_to_death_ebola <- epiparameter::epidist("ebola", "onset_to_death")$pmf
#'
#' df_known_outcomes <- known_outcomes(df_in = ebola1976, onset_to_death_ebola)
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
  # extracting the case time series and removing first element
  # so the weights and cases match - see Nishiura et al. for definition of
  # correction term used
  cases <- df_in$cases

  # numerically evaluate the onset to death distribution over the same
  # number of time points as the length of the case time series
  onset_wts <- delay_pmf(seq_len(length(cases)))

  # convolve the onset to death distribution with the reverse of the
  # case time series. Use the reverse of the case time series to match the
  # formal definition of a convolution (see convolution() documentation for
  # further explanation, using ?convolution() command)
  df_in$known_outcomes <- stats::convolve(
    onset_wts, rev(cases),
    type = "circular"
  )

  # performing the cumulative sum as per the function argument. TRUE by default.
  if (cumulative) {
    df_in$known_outcomes <- cumsum(df_in$known_outcomes)
  }

  df_in
}
