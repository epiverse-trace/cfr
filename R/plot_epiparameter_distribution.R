#' Plot an epiparameter distribution object
#'
#' @description Produces a simple plot, using base R, of an epiparameter distribution object
#'
#' @param epidist An epiparameter object retrieved using the epiparameter::epidist_db()
#'
#' @param from The day from which to start plotting the distribution. Default option is day 0.
#'
#' @param to The day to which to start plotting the distribution. Default option is day 30.
#'
#' @param by The time resolution at which which to plot the distribution. Default option is 0.1 days.
#'
#' @return A plot of the epiparameter distribution over time
#'
#' @export
#'
#' @examples
#' # Retrieve the delay distribution between onset and death for Ebola,
#' # using the epiparameter function `epiparameter::epidist_db()`
#'
#' onset_to_death_ebola <- epiparameter::epidist_db(
#'   disease = "Ebola Virus Disease",
#'   epi_dist = "onset_to_death",
#'   author = "Barry_etal"
#' )
#'
#' plot_epiparameter_distribution(onset_to_death_ebola, by = 1)
plot_epiparameter_distribution <- function(epidist,
                                           from = 0,
                                           to = 30,
                                           by = 0.1) {
  time <- seq(from, to, by)

  pmf_vals <- stats::density(
    epidist,
    at = time
  )

  plot(
    x = time,
    y = pmf_vals,
    xlab = "Time since exposure (t)",
    ylab = "Probability",
    type = "l"
  )

  polygon(c(time, max(time)), c(pmf_vals, 0),
    col = adjustcolor("slateblue1", alpha.f = 0.2)
  )
}
