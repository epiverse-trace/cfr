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
#' @inheritParams estimate_static
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
#' of the corrected severity, named "ascertainment_me" (for the central estimate
#' ), and "ascertainment_lo" and "ascertainment_hi" for the lower and upper
#' interval limits.
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
#' estimate_ascertainment(
#'   data = df_covid_uk,
#'   epidist = onset_to_death_covid,
#'   type = "varying",
#'   severity_baseline = 0.014,
#'   smooth_inputs = TRUE,
#'   burn_in_value = 7L,
#'   correct_for_delays = TRUE,
#'   max_date = "2020-06-30"
#' )
#'
estimate_ascertainment <- function(data,
                                   epidist = NULL,
                                   type = c("static", "varying"),
                                   severity_baseline = 0.014,
                                   burn_in_value = get_default_burn_in(
                                     epidist
                                   ),
                                   smooth_inputs = FALSE,
                                   smoothing_window = 1,
                                   correct_for_delays = FALSE,
                                   max_date = NULL) {
  # input checking
  checkmate::assert_data_frame(data)
  checkmate::assert_names(
    colnames(data),
    must.include = c("date", "cases", "deaths")
  )
  checkmate::assert_number(
    severity_baseline,
    lower = 0.0, upper = 1.0, finite = TRUE
  )
  checkmate::assert_integerish(burn_in_value,
    lower = 1, len = 1L
  )
  checkmate::assert_logical(
    smooth_inputs,
    len = 1L, any.missing = FALSE, all.missing = FALSE
  )
  checkmate::assert_logical(
    correct_for_delays,
    len = 1L, any.missing = FALSE, all.missing = FALSE
  )
  checkmate::assert_string(max_date, null.ok = TRUE)
  if (correct_for_delays) {
    checkmate::assert_class(epidist, "epidist")
  }

  # match argument for type
  type <- match.arg(type, several.ok = FALSE)

  if (type == "static") {
    df_severity <- estimate_static(
      data,
      epidist = epidist,
      correct_for_delays = correct_for_delays
    )
  } else if (type == "varying") {
    df_severity <- estimate_time_varying(
      data,
      epidist = epidist,
      smooth_inputs = smooth_inputs,
      smoothing_window = smoothing_window,
      burn_in_value = burn_in_value,
      correct_for_delays = correct_for_delays
    )

    df_severity <- df_severity[!is.na(df_severity$severity_me), ]

    # collect the severity at the last date, or the date specified by
    # the user in `max_date`
    if (!is.null(max_date)) {
      df_severity <- df_severity[df_severity$date == max_date, ]
    } else {
      df_severity <- df_severity[df_severity$date == max(df_severity$date), ]
    }
  }

  # data frame for exports, first scale all values and ensure maximum is 1.0
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

  # re-convert to data.frame from list
  # here, the estimate called "severity_me" translates to "ascertainment_me"
  # and the estimate "severity_hi" translates to "ascertainment_lo"
  # TODO: check if this is correct
  df_out <- as.data.frame(df_out,
    row.names = NULL,
    col.names = c("ascertainment_me", "ascertainment_hi", "ascertainment_lo")
  )

  # return data with columns in correct order
  df_out[, c("ascertainment_me", "ascertainment_lo", "ascertainment_hi")]
}
