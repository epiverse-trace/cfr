#' Static case fatality rate
#'
#' @description Calculates the CFR at a single point in time using in the case
#' and death time series supplied. Uses all of the time points supplied by the
#' user and returns an estimate for a single point in time.
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
#' @param epi_dist The delay distribution used, in the form of an
#' [epiparameter::epidist()] object. This is used to obtain a probability
#' mass function parameterised by time; i.e. \eqn{f(t)} which gives the
#' probability a case has a known outcomes (i.e. death) at time \eqn{t},
#' parameterised with disease-specific parameters before it is supplied here.
#' A typical example would be a symptom onset to death delay distribution.
#'
#' @param poisson_threshold The case count above which to use Poisson
#' approximation. Set to 100 by default.
#'
#' @return A named vector with the MLE and 95% confidence interval of the
#' CFR estimates, named "cfr_me", "cfr_low", and "cfr_high".
#' 
#' @export
#'
#' @examples
#' # read epidist for EVD onset to death from {epiparameter}
#' onset_to_death_ebola <- epiparameter::epidist_db(
#'   disease = "Ebola Virus Disease",
#'   epi_dist = "onset_to_death",
#'   author = "Barry_etal"
#' )
#'
#' # load stored ebola case data from 1976 outbreak
#' data("ebola1976")
#'
#' # Calculate static naive CFR using saved
#' static_cfr(df_in = ebola1976, correct_for_delays = FALSE)
#'
#' # Calculate static corrected CFRs
#' static_cfr(
#'   df_in = ebola1976,
#'   correct_for_delays = TRUE,
#'   onset_to_death_ebola
#' )
static_cfr <- function(df_in,
                       correct_for_delays = TRUE,
                       epi_dist,
                       poisson_threshold = 100) {

  # returns error message if no delay distribution is supplied, but correction
  # for delays was requested
  if (missing(epi_dist) && (correct_for_delays)) {
    stop(
      "To correct for the delay between case detection and death,\
       please provide an onset-to-death (or similar) `epidist` object"
    )
  }
  if (!missing(epi_dist)) {
    stopifnot(
      "`epi_dist` must be an `epidist` object" =
        (epiparameter::is_epidist(epi_dist))
    )
  }
  stopifnot(
    "Case data must contain columns `cases` and `deaths`" =
      (all(c("cases", "deaths") %in% colnames(df_in)))
  )

  # calculating the naive CFR: (total deaths) / (total cases)
  cfr_estimate <- numeric()
  # with the standard binomial
  if (correct_for_delays) {
    # calculating the corrected cfr, corrected for delay between case detection
    # and outcome
    # calculating the number of cases with known outcome, used as a replacement
    # for total deaths in the original cfr formula
    df_corrected <- known_outcomes(
      df_in = df_in,
      epi_dist = epi_dist
    )

    # calculating the total number of cases and deaths after correcting for
    # the number of cases with known outcomes and using this estimate as the
    # of deaths
    total_cases <- sum(df_corrected$cases)
    total_deaths <- sum(df_corrected$deaths)

    # calculating the proportion of cases with known outcome
    u_t <- sum(df_corrected$known_outcomes) / total_cases

    # calculating the maximum likelihood estimate and 95% confidence interval
    # using the binomial likelihood function from Nishiura
    cfr_estimate <- estimate_ccfr(
      total_cases = total_cases,
      total_deaths = total_deaths,
      u_t = u_t,
      poisson_threshold = poisson_threshold
    )
  } else {
    # calculating the total number of cases (without correcting) and deaths
    total_cases <- sum(df_in$cases)
    total_deaths <- sum(df_in$deaths)

    # calculating the central estimate
    cfr_me <- total_deaths / total_cases

    # calculating the lower and upper 95% confidence interval using the exact
    # binomial test
    cfr_conf <- stats::binom.test(round(total_deaths), total_cases, p = 1)

    # extracting the lower and upper intervals respectively
    cfr_lims <- cfr_conf$conf.int

    # putting together the estimates
    cfr_estimate <- c(cfr_me, cfr_lims)
    names(cfr_estimate) <- c("cfr_me", "cfr_low", "cfr_high")
  }

  cfr_estimate
}
