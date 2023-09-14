#' @title Estimate known outcomes of cases using a delay distribution
#'
#' @description Estimates the expected number of individuals with known outcomes
#' from a case and outcome time series of outbreak data, and an epidemiological
#' delay distribution of symptom onset to outcome.
#' When calculating a case fatality risk, the outcomes must be deaths, the delay
#' distribution must be an onset-to-death distribution, and the function returns
#' estimates of the known death outcomes.
#'
#' @inheritParams cfr_static
#'
#' @return A `<data.frame>` with the columns in `data`, and with two additional
#' columns:
#'
#'  - `"known_outcomes"` for the total number of known outcomes on that day
#' of the outbreak, and
#'
#'  - `"u_t"` for the under-reporting factor.
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
#' outcomes <- known_outcomes(data = ebola1976, onset_to_death_ebola)
#'
#' head(outcomes)
known_outcomes <- function(data,
                           epidist) {
  # some input checking; this function is mainly called internally
  # but currently exported
  # input checking is a candidate for removal
  checkmate::assert_data_frame(
    data,
    min.rows = 1, min.cols = 2
  )
  checkmate::assert_names(
    names(data),
    must.include = c("cases", "date")
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
  kn_out <- numeric(case_length)

  # main calculation loop
  for (i in rev(seq_len(case_length))) {
    # Delay probability mass function, evaluated at times
    # within the case and death times series
    delay_pmf_eval <- pmf_vals[case_times[seq_len(i)]]

    # Estimate the number of onsets associated with deaths
    known_onsets_current <- cases[seq_len(i)] * rev(delay_pmf_eval)

    # Collecting all current known onset estimates in a new
    # column of the original data.frame
    kn_out[i] <- sum(known_onsets_current)
  }
  # Calculating the proportion of cases with known onset,
  # for use in the simple likelihood function
  data$known_outcomes <- kn_out
  data$u_t <- cumsum(kn_out) / cumsum(cases)

  # return dataframe with added columns
  data
}
