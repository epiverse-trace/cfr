#' @title Estimate a severity measure that varies over time
#'
#' @description Calculates how the severity of a disease changes over time
#' while optionally correcting for reporting delays using an epidemiological
#' delay distribution of the time between symptom onset and death
#' (onset-to-death).
#'
#' @inheritParams cfr_static
#' @param burn_in_value A single integer value for the number of time-points
#' (typically days) to disregard at the start of the time-series, if a burn-in
#' period is desired.
#'
#' When an `<epidist>` is provided and delay correction is requested, the
#' default value is set to the mean of the `<epidist>`.
#'
#' Defaults to 7 if no `<epidist>` is provided, or if the `<epidist>` is not
#' parameterised. This is a sensible default value that disregards the first
#' week of cases and deaths, assuming daily data.
#'
#' To consider all case data including the start of the time-series, set this
#' argument to 1.
#'
#' @param smoothing_window An _odd_ number determining the smoothing window size
#' to use when smoothing the case and death time-series, using a rolling median
#' procedure (as the `k` argument to [stats::runmed()]) before calculating the
#' time-varying severity.
#'
#' The default behaviour is to apply no smoothing. The minimum value of this
#' argument is 1.
#'
#' @return A `<data.frame>` containing the MLE estimate and 95% confidence
#' interval of the corrected severity.
#'
#' @details
#' # Details: Adjusting for delays between two time series
#'
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
#' The epidemiological delay distribution passed to `epidist` is used to obtain
#' a probability mass function parameterised by time; i.e. \eqn{f(t)} which
#' gives the probability a case has a known outcomes (usually, death) at time
#' \eqn{t}, parameterised with disease-specific parameters before it is supplied
#' here.
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
#' cfr_time_varying <- cfr_time_varying(
#'   data = df_covid_uk_subset,
#'   burn_in_value = 7L
#' )
#' # View
#' tail(cfr_time_varying)
#'
#' # estimate time varying severity while correcting for delays
#' cfr_time_varying <- cfr_time_varying(
#'   data = df_covid_uk_subset,
#'   epidist = onset_to_death_covid,
#'   burn_in_value = 7L
#' )
#' tail(cfr_time_varying)
#'
cfr_time_varying <- function(data,
                             epidist = NULL,
                             burn_in_value = get_default_burn_in(epidist),
                             smoothing_window = NULL) {
  # input checking
  checkmate::assert_integerish(burn_in_value, lower = 1, len = 1L)

  # expect rows more than burn in value
  checkmate::assert_data_frame(data, min.cols = 3, min.rows = burn_in_value + 1)
  checkmate::assert_number(smoothing_window, lower = 1, null.ok = TRUE)

  stopifnot(
    "Case data must contain columns `cases` and `deaths`" =
      (all(c("cases", "deaths") %in% colnames(data))),
    "`smoothing_window` must be an odd number greater than 0" =
      (smoothing_window %% 2 != 0)
  )
  checkmate::assert_class(epidist, "epidist", null.ok = TRUE)

  # prepare a new dataframe with smoothed columns if requested
  # all temporary operations are performed on df_temp,
  # data is returned with only three new columns added, and no other changes
  df_temp <- data
  # smooth cases if requested
  if (is.null(smoothing_window)) {
    # set to 0 for internal use only --- see below
    smoothing_window <- 0
  } else {
    # smooth data if requested
    df_temp$cases <- stats::runmed(data$cases,
      k = smoothing_window,
      endrule = "keep"
    )

    df_temp$deaths <- stats::runmed(data$deaths,
      k = smoothing_window,
      endrule = "keep"
    )
  }

  cases <- df_temp$cases

  case_times <- as.numeric(df_temp$date - min(df_temp$date, na.rm = TRUE),
    units = "days"
  ) + 1

  case_length <- length(case_times)

  ##### prepare matrix for severity estimation ####
  # create columns with NA values for later assignment
  # when not correcting for delays, set known outcomes to cases
  # this is to avoid if-else ladders
  df_temp$known_outcomes <- df_temp$cases

  # assign columns for severity estimate and intervals
  severity_estimates <- matrix(
    data = NA_real_, nrow = nrow(data), ncol = 3,
    dimnames = list(NULL, sprintf("severity_%s", c("mean", "low", "high")))
  )

  # calculation of indices to modify seems questionable
  indices <- seq(case_length - smoothing_window, burn_in_value, -1)
  if (!is.null(epidist)) {
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

      severity_estimates[i, ] <- c(
        severity_current_estimate$estimate[[1]],
        severity_current_estimate$conf.int[[1]],
        severity_current_estimate$conf.int[[2]]
      )
    }
  }

  # remove known outcomes column as this is not expected as a side effect
  data$known_outcomes <- NULL

  # bind estimates to data
  data <- cbind(data, severity_estimates)

  # return data
  data
}

#' Get a default burn-in value from a delay distribution
#'
#' @inheritParams cfr_time_varying
#'
#' @return A single integer, the burn-in value.
#' @keywords internal
get_default_burn_in <- function(epidist = NULL) {
  default_value <- 7L
  if (!is.null(epidist) && !is.na(mean(epidist))) {
    x <- as.integer(round(mean(epidist)))
  } else {
    x <- default_value
  }

  # return x
  x
}
