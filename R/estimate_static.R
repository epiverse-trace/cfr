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
#' @param epi_dist The delay distribution used, in the form of an
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
#' severity estimates, named "severity_me", "severity_low", and "severity_high",
#' as well as a column "location" holding the grouping variable indicating the
#' region.
#'
#' @export
#'
#' @examples
#' library(datadelay)
#' library(epiparameter)
#' library(covidregionaldata)
#'
#' data("ebola1976")
#'
#' ebola1976$location <- "Democractic Republic of the Congo"
#'
#' df_ebola_subset <- subset(ebola1976, date <= "1976-09-30")
#'
#' onset_to_death_ebola <- epidist_db(
#'   disease = "Ebola Virus Disease",
#'   epi_dist = "onset_to_death",
#'   author = "Barry_etal"
#' )
#'
#' df_ncfr_static_ebola <- estimate_static(
#'   df_ebola_subset,
#'   correct_for_delays = FALSE
#' )
#'
#' format_output(
#'   df_ncfr_static_ebola,
#'   estimate_type = "severity",
#'   type = "Naive CFR"
#' )
#'
#' # calculating the corrected CFR
#' df_ccfr_static_ebola <- estimate_static(
#'   df_ebola_subset,
#'   correct_for_delays = TRUE,
#'   epi_dist = onset_to_death_ebola
#' )
#'
#' format_output(
#'   df_ccfr_static_ebola,
#'   estimate_type = "severity",
#'   type = "Corrected CFR"
#' )
#'
estimate_static <- function(data,
                            correct_for_delays = TRUE,
                            epi_dist,
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
    "Input data must have sequential dates with none missing or duplicated" =
      identical(unique(diff(data$date)), 1) # use numeric 1, not integer
    # this solution works when df$date is `Date`
    # this may need more thought for dates that are integers, POSIXct,
    # or other units; consider the units package
  )
  checkmate::assert_logical(correct_for_delays, len = 1L)
  checkmate::assert_number(poisson_threshold, lower = 0, finite = FALSE)

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
      (all(c("cases", "deaths") %in% colnames(data)))
  )

  # calculating the naive severity: (total deaths) / (total cases)
  severity_estimate <- numeric()

  # with the standard binomial
  if (correct_for_delays == TRUE) {
    # calculating the corrected severity, corrected for delay between case
    # detection and outcome calculating the number of cases with known outcome,
    # used as a replacement for total deaths in the original severity formula
    df_corrected <- known_outcomes(
      data = data,
      epi_dist = epi_dist
    )

    # calculating the total number of cases and deaths after correcting for
    # the number of cases with known outcomes and using this estimate as the
    # of deaths
    df_corrected$total_cases <- sum(df_corrected$cases, na.rm = TRUE)
    df_corrected$total_deaths <- sum(df_corrected$deaths, na.rm = TRUE)
    df_corrected$total_outcomes <- sum(
      df_corrected$known_outcomes,
      na.rm = TRUE
    )

    # calculating the proportion of cases with known outcome
    df_corrected$u_t <- df_corrected$total_outcomes / df_corrected$total_cases

    # calculating the maximum likelihood estimate and 95% confidence interval
    # using the binomial likelihood function from Nishiura
    severity_estimate <- estimate_severity(
      data = df_corrected,
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
