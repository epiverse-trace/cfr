#' Estimate a static (in time) severity measure
#'
#' @description Calculates the severity of a disease, corrected for a
#' user-specified delay. If cases are supplied, and the delay distribution
#' representing the delay between case detection and death, then a case
#' fatality ratio over time is estimated
#'
#' @param data A data.frame containing the outbreak data. A daily time series
#' with dates or some other absolute indicator of time (e.g. epiday/epiweek) and
#' the numbers of new cases and new deaths at each time point.
#' Note that the required columns are "date" (for the date), "cases" (for the
#' number of reported cases), and "deaths" (for the number of reported deaths)
#' on each day of the outbreak.
#' Note that the data.frame is required to have an unbroken sequence of dates
#' with no missing dates in between. The "date" column must be of the class
#' `Date` (see [as.Date()]).
#' Note also that the total number of cases must be greater than the total
#' number of reported deaths.
#'
#' @param correct_for_delays A boolean flag indicating whether the user wishes
#' to correct for the delay between case detection and death. FALSE corresponds
#' to a naive severity being calculated, TRUE corresponds to the user
#' calculating a corrected severity
#'
#' @param epidist The delay distribution used, in the form of an
#' [epiparameter::epidist()] object. This is used to obtain a probability
#' mass function parameterised by time; i.e. \eqn{f(t)} which gives the
#' probability a case has a known outcomes (i.e. death) at time \eqn{t},
#' parameterised with disease-specific parameters before it is supplied here.
#' A typical example would be a symptom onset to death delay distribution.
#'
#' @param poisson_threshold The case count above which to use Poisson
#' approximation. Set to 200 by default.
#'
#' @return A data.frame with the MLE and 95% confidence interval of the
#' severity estimates, named "severity_me", "severity_lo", and "severity_hi".
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
                            correct_for_delays = FALSE,
                            epidist = NULL,
                            poisson_threshold = 100) {
  # input checking
  checkmate::assert_data_frame(data)
  # check that input data frame has columns date, cases, and deaths
  checkmate::assert_names(
    colnames(data),
    must.include = c("date", "cases", "deaths")
  )
  # check that data$date is a date column
  checkmate::assert_date(data$date, any.missing = FALSE, all.missing = FALSE)
  # check for excessive missing date and throw an error
  stopifnot(
    "Case data must contain columns `cases` and `deaths`" =
      (all(c("cases", "deaths") %in% colnames(data))),
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

  # calculating the naive severity: (total deaths) / (total cases)
  severity_estimate <- numeric()

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
    severity_me <- total_deaths / total_cases

    # calculating the lower and upper 95% confidence interval using the exact
    # binomial test
    severity_conf <- stats::binom.test(round(total_deaths), total_cases, p = 1)

    # extracting the lower and upper intervals respectively
    severity_lims <- severity_conf$conf.int

    severity_estimate <- data.frame(
      "severity_me" = severity_me,
      "severity_lo" = severity_lims[[1]],
      "severity_hi" = severity_lims[[2]]
    )
  }

  # return the severity estimate
  severity_estimate
}
