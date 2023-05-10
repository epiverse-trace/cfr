#' Plot time-varying severity estimates
#'
#' @description Produces a plot, using base R, of the time-varying severity
#' estimate over the entire period it was calculated.
#'
#' @param df_in A data.frame of the format returned by [known_outcomes()].
#'
#' @param lower A numeric value determining the lower limit of the y-axis of the
#' plot. In place to easily help the user produce a readable graph, as severity
#' estimates often have typical ranges for certain diseases.
#'
#' @param upper A numeric value determining the upper limit of the y-axis of the
#' plot.
#'
#' @return A plot of the time-varying severity estimate
#'
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
#' df_covid_uk_subset <- subset(df_covid_uk, date <= "2020-12-31")
#'
#' onset_to_death_covid <- epidist_db(
#'   disease = "COVID-19",
#'   epi_dist = "onset_to_death",
#'   author = "Linton_etal"
#' )
#'
#' df_covid_cfr_uk_naive <- estimate_time_varying(
#'   df_covid_uk_subset,
#'   epi_dist = onset_to_death_covid,
#'   smooth_inputs = TRUE,
#'   burn_in = TRUE,
#'   correct_for_delays = FALSE
#' )
#'
#' df_covid_cfr_uk_corrected <- estimate_time_varying(
#'   df_covid_uk_subset,
#'   epi_dist = onset_to_death_covid,
#'   smooth_inputs = TRUE,
#'   burn_in = TRUE,
#'   correct_for_delays = TRUE
#' )
#'
#' plot_time_varying(df_covid_cfr_uk_naive, lower = 0, upper = 5)
#' plot_time_varying(df_covid_cfr_uk_corrected, lower = 0, upper = 5)
#'
plot_time_varying <- function(df_in,
                              lower = 0,
                              upper = 10) {
  df_plot <- subset(df_in, is.na(severity_me) == FALSE)

  dates <- df_plot$date
  severity_me <- df_plot$severity_me * 100
  severity_lo <- df_plot$severity_lo * 100
  severity_hi <- df_plot$severity_hi * 100

  plot(dates, severity_me,
    ylim = c(lower, upper),
    type = "l", xlab = "Date", ylab = "CFR (%)"
  )
  # make polygon where coordinates start with lower limit and
  # then upper limit in reverse order
  polygon(c(dates, rev(dates)),
    c(severity_hi, rev(severity_lo)),
    col = "grey75", border = FALSE
  )

  lines(dates, severity_me, lwd = 2)
  # add red lines on borders of polygon
  lines(dates, severity_hi, col = "blue", lty = 2, lwd = 0.5)
  lines(dates, severity_lo, col = "blue", lty = 2, lwd = 0.5)
  par(mfrow = c(1, 1))

  grid(
    nx = NULL, ny = NULL,
    lty = 6, # Grid line type
    col = "cornsilk2", # Grid line color
    lwd = 2
  )
}
