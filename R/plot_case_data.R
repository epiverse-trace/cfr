#' Plot the raw case time-series data
#'
#' @description Produces a simple plot, using base R, of the raw case
#' time-series data
#'
#' @param df_in A data.frame containing the outbreak data. A daily time series
#' with dates or some other absolute indicator of time (e.g. epiday/epiweek) and
#' the numbers of new case at each time point. The input data.frame assumes
#' columns with the names: date, cases and deaths
#'
#' @return A plot of the case time-series
#'
#' @export
#'
#' @examples
#' # Load Ebola 1976 outbreak data
#' data("ebola1976")
#'
#' plot_case_data(ebola1976)
plot_case_data <- function(df_in) {
  dates <- df_in$date
  cases <- df_in$cases

  plot(dates, cases,
    col = "blue",
    type = "s", lwd = 2,
    xlab = "Date", ylab = "Incidence"
  )
  graphics::legend("topright",
    legend = "Cases",
    col = "blue",
    lty = 1,
    cex = 1
  )
  graphics::grid(
    nx = NULL, ny = NULL,
    lty = 6, # Grid line type
    col = "cornsilk2", # Grid line color
    lwd = 1
  )
}
