#' Estimate the ascertainment rate of a disease given a baseline (assumed to be
#' true) severity estimate
#'
#' @description Estimates the proportion of cases or infections that have been
#' ascertained (declared as official cases), given a time-series of cases and
#' deaths, a delay distribution and a baseline severity estimate. The resulting
#' ascertainment estimate is calculated as the ratio of the baseline severity
#' estimate and the delay-adjusted severity estimate
#'
#' @param df_in A data.frame containing the outbreak data. A daily time series
#' with dates or some other absolute indicator of time (e.g. epiday/epiweek) and
#' the numbers of new cases and new deaths at each time point
#'
#' @param epi_dist The delay distribution used, in the form of an
#' [epiparameter::epidist()] object. This is used to obtain a probability
#' mass function parameterised by time; i.e. \eqn{f(t)} which gives the
#' probability a case has a known outcomes (i.e. death) at time \eqn{t},
#' parameterised with disease-specific parameters before it is supplied here.
#' A typical example would be a symptom onset to death delay distribution.
#'
#' @param type A boolean flag which determines whether [estimate_static()] or
#' [estimate_time_varying()] is used to calculate the resulting ascertainment
#' rate
#'
#' @param severity_baseline The assumed to be true baseline severity estimate
#' used in the final ratio to estimate the overall ascertainment rate
#'
#' @param burn_in A boolean flag to determine whether a burn in at the start
#' of time time-series should be used. Specifically, it askes whether the user
#' wishes to disregard the first [burn_in_arg] days of the time-series, given
#' that the calculation can produce noisey and uncertain estimates when case
#' and death numbers are both low, which is typical at the start of outbreaks
#'
#' @param smooth_inputs A boolean flag determining whether the user wishes to
#' smooth the case and death time-series, using a moving average procedure
#' before calculating the time-varying severity. Useful for noisey time-series
#' or time-series with strong reporting (e.g., weekend) effects
#'
#' @param correct_for_delays A boolean flag indicating whether the user wishes
#' to correct for the delay between case detection and death. FALSE corresponds
#' to a naive severity being calculated, TRUE corresponds to the user
#' calculating a corrected severity
#'
#' @param max_date A string representing a user supplied maximum date, up to
#' which the time-varying severity estimate will be calculated. Useful in the
#' case of long time-series, where the user wishes to focus on a specific
#' time-period
#'
#' @return A data.frame containing the MLE estimate and 95% confidence interval
#' of the corrected severity
#' @export
#'
#' @examples
#' library(datadelay)
#' library(epiparameter)
#' library(covidregionaldata)
#' library(dplyr)
#'
#' df_covid_uk <- get_national_data(
#'   countries = "united kingdom", source = "who", verbose = FALSE
#' )
#' df_covid_uk <- dplyr::rename(
#'   df_covid_uk,
#'   cases = cases_new, deaths = deaths_new
#' )
#'
#' df_covid_uk_subset <- subset(df_covid_uk, date <= "2020-05-31")
#'
#' onset_to_death_covid <- epidist_db(
#'   disease = "COVID-19",
#'   epi_dist = "onset_to_death",
#'   author = "Linton_etal"
#' )
#'
#' df_reporting_varying <- estimate_reporting(df_covid_uk,
#'   epi_dist = onset_to_death_covid,
#'   type = "varying",
#'   severity_baseline = 0.014,
#'   smooth_inputs = TRUE,
#'   burn_in_value = 7L,
#'   correct_for_delays = TRUE,
#'   max_date = "2020-06-30"
#' )
#'
#' format_output(
#'   df_reporting_varying,
#'   estimate_type = "reporting",
#'   type = "Under-reporting"
#' )
#'
estimate_reporting <- function(df_in,
                               epi_dist,
                               type = "static",
                               severity_baseline = 0.014,
                               burn_in_value = get_default_burn_in(epi_dist),
                               smooth_inputs = NULL,
                               correct_for_delays = NULL,
                               max_date = NULL) {
  if (type == "static") {
    df_severity <- estimate_static(
      df_in,
      epi_dist = epi_dist,
      correct_for_delays = TRUE
    )
  } else if (type == "varying") {
    df_severity <- estimate_time_varying(
      df_in,
      epi_dist = epi_dist,
      smooth_inputs = smooth_inputs,
      burn_in_value = burn_in_value,
      correct_for_delays = correct_for_delays
    )

    df_severity <- subset(df_severity, !is.na(df_severity$severity_me))

    if (!is.null(max_date)) {
      df_severity <- subset(df_severity, date == max(max_date))
    } else {
      df_severity <- subset(df_severity, date == max(date))
    }
  }

  if (!is.null(df_in$country)) {
    df_out <- data.frame(
      location = unique(df_in$country),
      reporting_me = numeric(1),
      reporting_lo = numeric(1),
      reporting_hi = numeric(1)
    )
  } else {
    df_out <- data.frame(
      reporting_me = numeric(1),
      reporting_lo = numeric(1),
      reporting_hi = numeric(1)
    )
  }

  df_out$reporting_me <- severity_baseline / unique(df_severity$severity_me)
  df_out$reporting_lo <- severity_baseline / unique(df_severity$severity_hi)
  df_out$reporting_hi <- severity_baseline / unique(df_severity$severity_lo)

  df_out$reporting_me[df_out$reporting_me > 1] <- 1
  df_out$reporting_lo[df_out$reporting_lo > 1] <- 1
  df_out$reporting_hi[df_out$reporting_hi > 1] <- 1

  return(df_out)
}
