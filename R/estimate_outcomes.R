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
#'  - `"estimated_outcomes"` for the number of cases with an outcome of interest
#' (usually, death) estimated to be known on the dates specified in `data`, and
#'
#'  - `"u_t"` for the ratio of cumulative number of estimated known outcomes
#' and the cumulative number of cases reported until each date specified in
#' `data`.
#'
#' @export
#'
#' @examples
#' # Load Ebola 1976 outbreak data
#' data("ebola1976")
#'
#' # estimate severity for each day while correcting for delays
#' # obtain onset-to-death delay distribution parameters from Barry et al. 2018
#' # examine the first few rows of the output
#' estimated_outcomes <- estimate_outcomes(
#'   data = ebola1976,
#'   delay_density = function(x) dgamma(x, shape = 2.40, scale = 3.33)
#' )
#'
#' head(estimated_outcomes)
estimate_outcomes <- function(data,
                              delay_density) {
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
  # check for any NAs among data
  checkmate::assert_data_frame(
    data[, c("cases", "deaths")],
    any.missing = FALSE
  )
  stopifnot(
    "`delay_density` must be a function with a single required argument,
    and evaluating distribution density at a vector of values and returning a
    numeric vector of the same length.
    E.g. function(x) stats::dgamma(x = x, shape = 5, scale = 1)" =
      (test_fn_req_args(delay_density) &&
        test_fn_num_out(delay_density)) || is.null(delay_density)
  )

  pmf_vals <- delay_density(seq(from = 0, to = nrow(data) - 1L))

  # defining vectors to be used in the main loop
  cases <- data$cases

  # the times at which cases are reported, in numbers of days (or whichever
  # time units are used) since the first case was reported
  case_times <- as.numeric(
    data$date - min(data$date, na.rm = TRUE),
    units = "days"
  ) + 1

  # the total number of time points at which cases were reported
  case_length <- length(case_times)

  # calculate expected outcomes
  estimated_outcomes <- vapply(
    X = rev(seq_len(case_length)),
    FUN = function(i) {
      delay_pmf_eval <- pmf_vals[case_times[seq_len(i)]]

      # Estimate the number of onsets associated with deaths
      expected_outcomes <- cases[seq_len(i)] * rev(delay_pmf_eval)

      # return total expected outcomes
      sum(expected_outcomes)
    },
    FUN.VALUE = numeric(1)
  )

  # calculate severity as ratio - note use of reversed vector
  data$estimated_outcomes <- rev(estimated_outcomes)
  data$u_t <- cumsum(data$estimated_outcomes) / cumsum(cases)

  # replace u_t that is NaN with NA (due to zero division)
  data$u_t[is.nan(data$u_t)] <- NA_real_

  # return dataframe with added columns
  data
}
