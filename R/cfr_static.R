#' Estimate a static disease severity measure
#'
#' @description Calculates the severity of a disease, while optionally
#' correcting for reporting delays using an epidemiological delay distribution
#' of the time between symptom onset and death (onset-to-death).
#'
#' Other delay distributions may be passed to calculate different disease
#' severity measures such as the hospitalisation fatality risk.
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
#' @param delay_density An optional argument that controls whether delay
#' correction is applied in the severity estimation.
#' May be `NULL`, for no delay correction, or a function that returns the
#' density function of a distribution to evaluate
#' density at user-specified values, e.g.
#' `function(x) stats::dgamma(x = x, shape = 5, scale = 1)`.
#'
#' @param poisson_threshold The case count above which to use Poisson
#' approximation. Set to 100 by default. Must be > 0.
#'
#' @details
#' # Details: Adjusting for delays between two time series
#'
#' The method used in `cfr_static()` follows Nishiura et al.
#' (2009).
#' The function calculates a quantity \eqn{u_t} for each day within the input
#' data, which represents the proportion of cases estimated to have
#' a known outcome on day \eqn{t}.
#' Following Nishiura et al., \eqn{u_t} is calculated as:
#' \deqn{u_t = \dfrac{\sum_{i = 0}^t
#'         \sum_{j = 0}^\infty c_i f_{j - i}}{\sum_{i = 0} c_i}}
#' where \eqn{f_t} is the value of the probability mass function at time \eqn{t}
#' and \eqn{c_t}, \eqn{d_t} are the number of new cases and new deaths at time
#' \eqn{t}, (respectively).
#' We then use \eqn{u_t} at the end of the outbreak in the following likelihood
#' function to estimate the severity of the disease in question.
#' \deqn{{\sf L}({\theta \mid y}) = \log{\dbinom{u_tC}{D}} + D \log{\theta} +
#'   (u_tC - D)\log{(1.0 - \theta)}}
#' \eqn{C} and \eqn{D} are the cumulative number of cases and deaths
#' (respectively) up until time \eqn{t}.
#' \eqn{\theta} is the parameter we wish to estimate, the severity of the
#' disease. We estimate \eqn{\theta} using simple maximum-likelihood methods,
#' allowing the functions within this package to be quick and easy tools to use.
#'
#' The precise severity measure — CFR, IFR, HFR, etc — that \eqn{\theta}
#' represents depends upon the input data given by the user.
#'
#' The epidemiological delay-distribution density function passed to
#' `delay_density` is used to evaluate the probability mass function
#' parameterised by time; i.e. \eqn{f(t)} which
#' gives the probability that a case has a known outcome (usually death) at time
#' \eqn{t}, parameterised with disease-specific parameters before it is supplied
#' here.
#'
#' @references
#' Nishiura, H., Klinkenberg, D., Roberts, M., & Heesterbeek, J. A. P. (2009).
#' Early Epidemiological Assessment of the Virulence of Emerging Infectious
#' Diseases: A Case Study of an Influenza Pandemic. PLOS ONE, 4(8), e6852.
#' \doi{10.1371/journal.pone.0006852}
#'
#' @return A `<data.frame>` with the maximum likelihood estimate and 95%
#' confidence interval of the severity estimates, named "severity_mean",
#' "severity_low", and "severity_high".
#'
#' @export
#'
#' @examples
#' # load package data
#' data("ebola1976")
#'
#' # estimate severity without correcting for delays
#' cfr_static(ebola1976)
#'
#' # estimate severity for each day while correcting for delays
#' # obtain onset-to-death delay distribution parameters from Barry et al. 2018
#' # The Lancet. <https://doi.org/10.1016/S0140-6736(18)31387-4>
#' cfr_static(
#'   ebola1976,
#'   delay_density = function(x) dgamma(x, shape = 2.40, scale = 3.33)
#' )
#'
cfr_static <- function(data,
                       delay_density = NULL,
                       poisson_threshold = 100) {
  # input checking
  checkmate::assert_data_frame(
    data,
    min.rows = 1, min.cols = 3
  )
  # check that input `<data.frame>` has columns date, cases, and deaths
  checkmate::assert_names(
    colnames(data),
    must.include = c("date", "cases", "deaths")
  )
  # check for any NAs among data
  checkmate::assert_data_frame(
    data[, c("date", "cases", "deaths")],
    types = c("Date", "integerish"),
    any.missing = FALSE
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
  checkmate::assert_count(poisson_threshold, positive = TRUE)

  # NOTE: delay_density is checked in estimate_outcomes() if passed and not NULL
  # calculating the total number of cases (without correcting) and deaths

  # Calculate total cases and deaths to pass to secondary checking
  total_cases <- sum(data$cases, na.rm = TRUE)
  total_deaths <- sum(data$deaths, na.rm = TRUE)

  # Add input checking for total cases and deaths
  checkmate::assert_count(total_cases)
  # use assert_number to set upper limit at total_cases
  checkmate::assert_number(total_deaths, upper = total_cases, lower = 0)

  # apply delay correction if a delay distribution is provided
  if (!is.null(delay_density)) {
    # calculating the corrected severity, corrected for delay between case
    # detection and outcome estimating the number of cases with a known outcome,
    # used as a replacement for total deaths in the original severity formula
    df_corrected <- estimate_outcomes(
      data = data,
      delay_density = delay_density
    )

    # calculate total cases, deaths, and outcomes
    total_outcomes <- sum(df_corrected$estimated_outcomes, na.rm = TRUE)

    # NOTE: previous code used `u_t = total_outcomes / total_cases`
    # which can be simplified in all operations to simply `total_outcomes`

    # Get direct estimate of p, and throw warning if p < 1e-4
    p_mid <- total_deaths / round(total_outcomes)

    if (p_mid < 1e-4) {
      warning(
        "Ratio of total deaths to total cases with known outcome",
        " is below 0.01%: CFR estimates may be unreliable.",
        call. = FALSE
      )
    }

    # calculating the maximum likelihood estimate and 95% confidence interval
    # using the binomial likelihood function from Nishiura
    severity_estimate <- .estimate_severity(
      total_cases = total_cases,
      total_deaths = total_deaths,
      total_outcomes = total_outcomes,
      poisson_threshold = poisson_threshold,
      p_mid = p_mid
    )
    # .estimate_severity() returns vector; convert to list and then data.frame
    severity_estimate <- as.data.frame(as.list(severity_estimate))
  } else {
    # calculating the central estimate
    severity_mean <- total_deaths / total_cases

    # calculating the lower and upper 95% confidence interval using the exact
    # binomial test
    severity_conf <- stats::binom.test(round(total_deaths), total_cases, p = 1)

    # extracting the lower and upper intervals respectively
    severity_lims <- severity_conf$conf.int

    severity_estimate <- data.frame(
      severity_mean = severity_mean,
      severity_low = severity_lims[[1]],
      severity_high = severity_lims[[2]]
    )
  }

  # return the severity estimate
  severity_estimate
}
