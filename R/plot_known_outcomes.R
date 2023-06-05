#' Plot the estimated known outcomes, as well as the raw data
#'
#' @description Produces a simple plot, using base R, of the raw case and death
#' time-series data, as well as the estimated number of known outcomes on each
#' day.
#'
#' @param data A data.frame of the format returned by [known_outcomes()].
#'
#' @return A plot of the three time-series on the same plot, with a legend
#'
#' @export
#'
#' @examples
#' # Load Ebola 1976 outbreak data
#' data("ebola1976")
#'
#' onset_to_death_ebola <- epiparameter::epidist_db(
#'   disease = "Ebola Virus Disease",
#'   epi_dist = "onset_to_death",
#'   author = "Barry_etal"
#' )
#'
#' df_known_outcomes <- known_outcomes(
#'   data = ebola1976,
#'   epi_dist = onset_to_death_ebola
#' )
#'
#' plot_known_outcomes(df_known_outcomes)
plot_known_outcomes <- function(data) {
  dates <- data$date
  cases <- data$cases
  known_outcomes <- data$known_outcomes

  plot(dates, cases,
    col = "blue", type = "s", lwd = 2,
    xlab = "Date", ylab = "Incidence"
  )

  graphics::lines(dates, known_outcomes, type = "s", lwd = 2, col = "green")

  graphics::legend("topleft",
    legend = c("Cases", "Known outcomes"),
    col = c("blue", "green"),
    lty = 1,
    cex = 1
  )

  graphics::grid(
    nx = NULL, ny = NULL,
    lty = 6, # Grid line type
    col = "cornsilk2", # Grid line color
    lwd = 2
  )
}
