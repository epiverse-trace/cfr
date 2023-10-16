#' Get the PDF/PMF from a distribution
#'
#' @description Calculates the density of the distribution at user-specified
#' values. PDF and PMF apply for continuous and discrete distributions,
#' respectively.
#'
#' @param f An object specifying a distribution, or a closure wrapping a call to
#' the density function on a distribution object.
#' Only `<epidist>` objects from \pkg{epiparameter} and closures are currently
#' supported. See **Examples**.
#' @param x A sequence of numeric values at which to estimate the density.
#'
#' @return
#' A numeric vector for the distribution density at the values passed in `x`.
#'
#' @keywords internal
get_density <- function(f, x) {
  # no input checking for internal function
  if (inherits(f, "epidist")) {
    values <- stats::density(f, at = x)
  } else if (is.function(f)) {
    values <- f(x)
  }

  # return values
  values
}
