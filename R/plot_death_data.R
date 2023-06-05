#' Plot the raw death time-series data
#'
#' @description Produces a simple plot, using base R, of the raw death
#' time-series data
#'
#' @param data A data.frame containing the outbreak data. A daily time series
#' with dates or some other absolute indicator of time (e.g. epiday/epiweek) and
#' the numbers of new deaths at each time point. The input data.frame assumes
#' columns with the names: date, cases and deaths
#'
#' @return A plot of the death time-series
#'
#' @export
#'
#' @examples
#' # Load Ebola 1976 outbreak data
#' data("ebola1976")
#'
#' plot_death_data(ebola1976)
plot_death_data <- function(data) {
  dates <- data$date
  deaths <- data$deaths

  plot(dates, deaths,
    col = "red",
    type = "s", lwd = 2,
    xlab = "Date", ylab = "Incidence"
  )
  graphics::legend("topright",
    legend = "Deaths",
    col = "red",
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
