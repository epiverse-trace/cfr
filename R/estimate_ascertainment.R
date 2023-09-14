#' Estimate the ascertainment ratio of a disease
#'
#' @description Estimates the proportion of cases or infections that have been
#' ascertained, given a time-series of cases and deaths, a delay distribution
#' and a baseline severity estimate. The resulting ascertainment estimate is
#' calculated as the ratio of the baseline severity estimate, which is assumed
#' to be the 'true' disease severity, and the delay-adjusted severity estimate.
#'
#' @inheritParams cfr_time_varying
#' @inheritParams cfr_static
#'
#' @param type A string, either `"static"` or `"varying"` which determines
#' whether [cfr_static()] or [cfr_time_varying()] is used to calculate
#' the resulting ascertainment ratio. Defaults to `"static"` if this argument is
#' missing.
#' @param severity_baseline A single number in the range 0.0 -- 1.0 for the
#' assumed true baseline severity estimate used to estimate the overall
#' ascertainment ratio. Missing by default, which causes the function to error;
#' must be supplied by the user.
#' @param max_date A `Date` representing a user supplied maximum date, up to
#' which the time-varying severity estimate will be calculated. Useful in the
#' case of long time-series, where the user wishes to focus on a specific
#' time-period. See [as.Date()] for converting a string to a `Date`.
#'
#' @return A `<data.frame>` containing the MLE estimate and 95% confidence
#' interval of the corrected severity, named "ascertainment_mean" (for the
#' central estimate), and "ascertainment_low" and "ascertainment_high" for the
#' lower and upper interval limits.
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
#'   max_date = as.Date("2020-06-30")
#' )
#'
estimate_ascertainment <- function(data,
                                   correct_for_delays = TRUE,
                                   epidist,
                                   type = c("static", "varying"),
                                   severity_baseline,
                                   burn_in_value = get_default_burn_in(
                                     epidist
                                   ),
                                   smooth_inputs = FALSE,
                                   smoothing_window = 1,
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
  checkmate::assert_date(max_date, null.ok = TRUE)
  if (correct_for_delays) {
    checkmate::assert_class(epidist, "epidist")
  }

  # match argument for type
  type <- match.arg(type, several.ok = FALSE)

  # switch the output based on user specified type
  df_severity <- switch(type,
    static = cfr_static(
      data,
      epidist = epidist,
      correct_for_delays = correct_for_delays
    ),
    varying = {
      df_sev <- cfr_time_varying(
        data,
        epidist = epidist,
        smooth_inputs = smooth_inputs,
        smoothing_window = smoothing_window,
        burn_in_value = burn_in_value,
        correct_for_delays = correct_for_delays
      )

      df_sev <- df_sev[!is.na(df_sev$severity_mean), ]

      # collect the severity at the last date, or the date specified by
      # the user in `max_date`
      if (!is.null(max_date)) {
        df_sev <- df_sev[df_sev$date == max_date, ]
      } else {
        df_sev <- df_sev[df_sev$date == max(df_sev$date), ]
      }

      # subset column names
      df_sev <- df_sev[
        ,
        grepl("severity", colnames(df_sev), fixed = TRUE)
      ]
    }
  )

  # data.frame for exports, first scale values by the 1/severity baseline
  # then ensure maximum is 1.0
  df_severity <- severity_baseline / df_severity
  # throw a warning for ascertainment ration > 1.0
  if (any(df_severity > 1.0)) {
    warning(
      "Ascertainment ratios > 1.0 detected, setting these values to 1.0"
    )
  }
  df_severity[df_severity > 1.0] <- 1.0

  # reset row numbers, which might be confusing
  rownames(df_severity) <- NULL

  # re-convert to data.frame from list
  # here, the estimate called "severity_mean" translates to "ascertainment_me"
  # and the estimate "severity_high" translates to "ascertainment_lo"
  # TODO: check if this is correct
  colnames(df_severity) <- c(
    "ascertainment_mean", "ascertainment_high", "ascertainment_low"
  )

  # return data with columns in correct order
  df_severity[, c(
    "ascertainment_mean", "ascertainment_low", "ascertainment_high"
  )]
}
