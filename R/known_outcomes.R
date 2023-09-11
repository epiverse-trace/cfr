#' Estimate known outcomes from case and death time-series data
#'
#' @description Estimates the expected number of individuals with known outcomes
#' from a case and death time series of data of an outbreak, up to the time
#' point supplied.
#' Calculates the daily new number of known outcomes.
#'
#' Uses the probability mass function representing the delay between
#' case detection and death, typically approximated by a symptom onset to death
#' distribution from the literature for the disease in question.
#'
#' @param data A data.frame containing the outbreak data. A daily time series
#' with dates or some other absolute indicator of time (e.g. epiday/epiweek) and
#' the numbers of new cases at each time point. This function does not require
#' data on daily deaths, but this column (and any others) will be retained if
#' present.
#'
#' @param epidist The delay distribution used, in the form of an
#' [epiparameter::epidist()] object. This is used to obtain a probability
#' mass function parameterised by time; i.e. \eqn{f(t)} which gives the
#' probability a case has a known outcomes (i.e. death) at time \eqn{t},
#' parameterised with disease-specific parameters before it is supplied here.
#' A typical example would be a symptom onset to death delay distribution.
#'
#' @return A data.frame with the columns in `data`, and with two additional
#' columns,
#'
#'  - "known_outcomes" for the total number of known outcomes on that day
#' of the outbreak, and
#'
#'  - "u_t" for the under-reporting factor.
#'
#' @export
#'
#' @examples
#' # Load Ebola 1976 outbreak data
#' data("ebola1976")
#'
#' # read epidist for EVD onset to death from {epiparameter}
#' onset_to_death_ebola <- epiparameter::epidist_db(
#'   disease = "Ebola Virus Disease",
#'   epi_dist = "onset_to_death",
#'   author = "Barry_etal",
#'   single_epidist = TRUE
#' )
#'
#' # examine the first few rows of the output
#' head(
#'   known_outcomes(data = ebola1976, onset_to_death_ebola)
#' )
known_outcomes <- function(data,
                           epidist) {
  # some input checking
  stopifnot(
    "Case data must be a data.frame" =
      (is.data.frame(data)),
    "Case data must contain columns `cases`" =
      "cases" %in% colnames(data)
  )
  checkmate::assert_class(epidist, "epidist")

  pmf_vals <- stats::density(
    epidist,
    at = seq(from = 0, to = nrow(data) - 1L)
  )

  # defining vectors to be used in the main loop
  cases <- data$cases

  # the times at which cases are reported, in numbers of days (or whichever
  # time units are used) since the first case was reported
  case_times <- as.numeric(data$date - min(data$date, na.rm = TRUE),
    units = "days"
  ) + 1

  # the total number of time points at which cases were reported
  case_length <- length(case_times)

  # declaring the outputs of the main loop as vectors within the main
  # data.frame
  data$known_outcomes <- numeric(case_length)

  # main calculation loop
  for (i in rev(seq_len(case_length))) {
    # Delay probability mass function, evaluated at times
    # within the case and death times series
    delay_pmf_eval <- pmf_vals[case_times[seq_len(i)]]

    # Estimate the number of onsets associated with deaths
    known_onsets_current <- cases[seq_len(i)] * rev(delay_pmf_eval)

    # Collecting all current known onset estimates in a new
    # column of the original data.frame
    data$known_outcomes[i] <- sum(known_onsets_current)
  }
  # Calculating the proportion of cases with known onset,
  # for use in the simple likelihood function
  data$u_t <- cumsum(data$known_outcomes) / cumsum(cases)

  # return dataframe with added columns
  data
}
