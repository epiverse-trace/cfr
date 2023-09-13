#' Estimate a static disease severity measure
#'
#' @description Calculates the severity of a disease, while correcting for a
#' user-specified reporting delay. If cases are supplied, and the delay
#' distribution represents the delay between case detection and death, then a
#' case fatality ratio is estimated.
#'
#' @param data A `<data.frame>` containing the outbreak data. A daily time
#' series with dates or some other absolute indicator of time (e.g. epiday or
#' epiweek) and the numbers of new cases and new deaths at each time point.
#' Note that the required columns are "date" (for the date), "cases" (for the
#' number of reported cases), and "deaths" (for the number of reported deaths)
#' on each day of the outbreak.
#'
#' Note that the `<data.frame>` is required to have an unbroken sequence of
#' dates with no missing dates in between. The "date" column must be of class
#' `Date` (see [as.Date()]).
#'
#' Note also that the total number of cases must be greater than the total
#' number of reported deaths.
#'
#' @param correct_for_delays A single logical value indicating whether
#' to correct for the delay between case detection and death using an
#' epidemiological delay distribution passed to `epidist`. `TRUE` by default,
#' while `FALSE` corresponds to a naive severity being calculated.
#'
#' @param epidist The delay distribution used, in the form of an
#' [epiparameter::epidist()] object. This is used to obtain a probability
#' mass function parameterised by time; i.e. \eqn{f(t)} which gives the
#' probability a case has a known outcomes (i.e. death) at time \eqn{t},
#' parameterised with disease-specific parameters before it is supplied here.
#' A typical example would be a symptom onset to death delay distribution.
#'
#' @param poisson_threshold The case count above which to use Poisson
#' approximation. Set to 100 by default.
#'
#' @details
#' # Details: Adjusting for delays between two time series
#'
#' The method used in `estimate_static()` function follows Nishiura et al.
#' (2009).
#' The function calculates a quantity \eqn{u_t} for each day within the input
#' data, which represents the proportion of cases with a known outcome on day
#' \eqn{t}.
#' Following Nishiura et al., \eqn{u_t} is calculated as:
#' \deqn{
#'
#' u_t = \frac{\sum_{i = 0}^t
#'         \sum_{j = 0}^\infty c_i f_{j - i}}{\sum_{i = 0} c_i}
#'
#' }
#' where \eqn{f_t} is the value of the probability mass function at time \eqn{t}
#' and \eqn{c_t}, \eqn{d_t} are the number of new cases and new deaths at time
#' \eqn{t}, (respectively).
#' We then use \eqn{u_t} at the end of the outbreak in the following likelihood
#' function to estimate the severity of the disease in question.
#' \deqn{
#'   L( \theta | y) = \log{\binom{u_tC}{D}} + D \log{ \theta } +
#'   (u_tC - D)\log{(1.0 - \theta)}
#' }
#' \eqn{C} and \eqn{D} are the cumulative number of cases and deaths
#' (respectively) up until time \eqn{t}.
#' \eqn{\theta} is the parameter we wish to estimate, the severity of the
#' disease. We estimate \eqn{\theta} using simple maximum-likelihood methods,
#' allowing the functions within this package to be quick and easy tools to use.
#'
#' The precise severity measure — CFR, IFR, HFR, etc — that \eqn{\theta}
#' represents depends upon the input data given by the user.
#'
#' @references
#' Nishiura, H., Klinkenberg, D., Roberts, M., & Heesterbeek, J. A. P. (2009).
#' Early Epidemiological Assessment of the Virulence of Emerging Infectious
#' Diseases: A Case Study of an Influenza Pandemic. PLOS ONE, 4(8), e6852.
#' \doi{10.1371/journal.pone.0006852}
#'
#' @return A `<data.frame>` with the MLE and 95% confidence interval of the
#' severity estimates, named "severity_mean", "severity_low", and
#' "severity_high".
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
#'   author = "Barry_etal",
#'   single_epidist = TRUE
#' )
#'
#' # estimate severity without correcting for delays
#' estimate_static(
#'   ebola1976,
#'   correct_for_delays = FALSE
#' )
#'
#' # estimate severity while correcting for delays
#' estimate_static(
#'   ebola1976,
#'   correct_for_delays = TRUE,
#'   epidist = onset_to_death_ebola
#' )
#'
estimate_static <- function(data,
                            correct_for_delays = TRUE,
                            epidist,
                            poisson_threshold = 100) {
  # input checking
  checkmate::assert_data_frame(data)
  # check that input `<data.frame>` has columns date, cases, and deaths
  checkmate::assert_names(
    colnames(data),
    must.include = c("date", "cases", "deaths")
  )
  # check that data$date is a date column
  checkmate::assert_date(data$date, any.missing = FALSE, all.missing = FALSE)
  # check for excessive missing date and throw an error
  stopifnot(
    "Input data must have sequential dates with none missing or duplicated" =
      identical(unique(diff(data$date)), 1) # use numeric 1, not integer
    # this solution works when df$date is `Date`
    # this may need more thought for dates that are integers, POSIXct,
    # or other units; consider the units package
  )
  checkmate::assert_logical(correct_for_delays, len = 1L)
  checkmate::assert_count(poisson_threshold)

  # returns error message if no delay distribution is supplied, but correction
  # for delays was requested
  if (correct_for_delays) {
    checkmate::assert_class(epidist, "epidist")
  }

  # with the standard binomial
  if (correct_for_delays) {
    # calculating the corrected severity, corrected for delay between case
    # detection and outcome calculating the number of cases with known outcome,
    # used as a replacement for total deaths in the original severity formula
    df_corrected <- known_outcomes(
      data = data,
      epidist = epidist
    )

    # calculating the maximum likelihood estimate and 95% confidence interval
    # using the binomial likelihood function from Nishiura
    severity_estimate <- estimate_severity(
      total_cases = sum(df_corrected$cases, na.rm = TRUE),
      total_deaths = sum(df_corrected$deaths, na.rm = TRUE),
      total_outcomes = sum(df_corrected$known_outcomes, na.rm = TRUE),
      poisson_threshold = poisson_threshold
    )
  } else {
    # calculating the total number of cases (without correcting) and deaths
    total_cases <- sum(data$cases, na.rm = TRUE)
    total_deaths <- sum(data$deaths, na.rm = TRUE)

    # calculating the central estimate
    severity_mean <- total_deaths / total_cases

    # calculating the lower and upper 95% confidence interval using the exact
    # binomial test
    severity_conf <- stats::binom.test(round(total_deaths), total_cases, p = 1)

    # extracting the lower and upper intervals respectively
    severity_lims <- severity_conf$conf.int

    severity_estimate <- data.frame(
      "severity_mean" = severity_mean,
      "severity_low" = severity_lims[[1]],
      "severity_high" = severity_lims[[2]]
    )
  }

  # return the severity estimate
  severity_estimate
}
