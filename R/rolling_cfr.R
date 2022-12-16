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
#' @param poisson_threshold The case count above which to use Poisson
#' approximation. Set to 200 by default.
#'
#' @return A data.frame containing the same case and death time series supplied
#' with the desired CFR (naive or corrected) appended at each time point
#' as extra columns
#' @export
#'
#' @examples
#' # Get onset to death distribution from epiparameter
#' onset_to_death_ebola <- epiparameter::epidist("ebola", "onset_to_death")
#' delay_pmf <- onset_to_death_ebola$pmf
#'
#' # Calculate rolling naive CFR
#' df_ncfr <- rolling_cfr(ebola1976, correct_for_delays = FALSE)
#'
#' # Calculate rolling corrected CFR
#' df_ccfr <- rolling_cfr(
#'   df_in = ebola1976,
#'   correct_for_delays = TRUE,
#'   delay_pmf
#' )
rolling_cfr <- function(df_in,
                        correct_for_delays = TRUE,
                        delay_pmf,
                        poisson_threshold = 200) {

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

  if (correct_for_delays) {
    # calculating the total number of cases and deaths after correcting for
    # the number of cases with known outcomes and using this estimate as the
    # of deaths
    df_in <- known_outcomes(
      df_in = df_in,
      delay_pmf = delay_pmf,
      cumulative = FALSE
    )
  }
  # prepare cumulative sums
  cumulative_cases <- cumsum(df_in$cases)
  cumulative_deaths <- cumsum(df_in$deaths)

  # empty list for results
  cfr_estimate <- list()

  # calculating the corrected CFR rolling over all days
  if (correct_for_delays) {
    # calculating the proportion of cases with known outcome
    u_t <- cumsum(df_in$known_outcomes) / cumulative_cases

    # calculating the maximum likelihood estimate and 95% confidence interval
    # using the binomial likelihood function from Nishiura
    cfr_estimate <- Map(
      cumulative_cases, cumulative_deaths, u_t,
      f = estimate_ccfr, poisson_threshold = poisson_threshold
    )
  } else {
    # calculating the uncorrected CFR rolling over all days
    cfr_me <- cumulative_deaths / cumulative_cases

    cfr_lims <- Map(
      cumulative_deaths, cumulative_cases,
      f = stats::binom.test, p = 1
    )
    cfr_estimate <- Map(
      cfr_lims, cfr_me,
      f = function(bintest, me) {
        c(me, bintest[["conf.int"]])
      }
    )
  }
  df_cfr <- as.data.frame(
    data.table::transpose(
      cfr_estimate
    ),
    col.names = c("cfr_me", "cfr_low", "cfr_high")
  )

  # attaching the dates, cases and deaths to the CFR estimates
  df_cfr <- cbind(df_cfr, df_in[, c("date", "cases", "deaths")])

  df_cfr
}
