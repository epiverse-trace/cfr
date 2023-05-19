#' Estimate the ascertainment rate of a disease given a baseline (assumed to be
#' true) severity estimate
#'
#' @description Estimates the proportion of cases or infections that have been
#' ascertained (declared as official cases), given a time-series of cases and
#' deaths, a delay distribution and a baseline severity estimate. The resulting
#' ascertainment estimate is calculated as the ratio of the baseline severity
#' estimate and the delay-adjusted severity estimate
#'
#' @inheritParams estimate_time_varying
#'
#' @param type A string, either `"static"` or `"varying"` which determines
#' whether [estimate_static()] or [estimate_time_varying()] is used to calculate
#' the resulting ascertainment rate
#' @param severity_baseline The assumed to be true baseline severity estimate
#' used in the final ratio to estimate the overall ascertainment rate
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
#'
#' df_covid_uk <- get_national_data(
#'   countries = "united kingdom", source = "who", verbose = FALSE
#' )
#' # rename columns
#' colnames(df_covid_uk)[colnames(df_covid_uk) == "cases_new"] <- "cases"
#' colnames(df_covid_uk)[colnames(df_covid_uk) == "deaths_new"] <- "deaths"
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
#'   max_date = "2020-06-30",
#'   location = "U.K."
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
                               max_date = NULL,
                               location = "country") {
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

    df_severity <- df_severity[!is.na(df_severity$severity_me), ]

    if (!is.null(max_date)) {
      df_severity <- df_severity[df_severity$date == max_date, ]
    } else {
      df_severity <- df_severity[df_severity$date == max(df_severity$date), ]
    }
  }

  # data frame for exports
  df_out <- apply(
    df_severity[, grepl("severity", colnames(df_severity), fixed = TRUE)],
    MARGIN = 2,
    FUN = function(x) {
      x_ <- severity_baseline / x
      x_[x_ > 1.0] <- 1.0
      x_
    },
    simplify = FALSE
  )
  df_out <- as.data.frame(df_out)
  colnames(df_out) <- gsub(
    "severity", "reporting",
    x = colnames(df_out), fixed = TRUE
  )

  # assign location or NA if not present
  df_out$location <- NA_character_
  if (location %in% colnames(df_in)) {
    df_out$location <- unique(df_in[[location]])
  }

  return(df_out)
}
