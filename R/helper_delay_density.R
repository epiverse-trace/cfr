#' @title Check delay density functions passed to exported functions
#'
#' @name delay_density_helpers
#' @rdname delay_density_helpers
#'
#' @description Internal helper function that check whether a function passed to
#' the `delay_density` argument in `cfr_*()` or [estimate_outcomes()] meet the
#' requirements of package methods.
#'
#' `test_fn_req_args()` checks whether the function has only the expected number
#' of required arguments, i.e., arguments without default values. Defaults to
#' checking for a single required argument.
#'
#' `test_fn_num_out()` checks whether the function returns a numeric output
#' consistent with evaluating the probability density or probability mass
#' function of a distribution over a sequence of values.
#' Expects that the function returns a numeric vector of finite values
#' \eqn{\geq} 0.0, that no values are missing, and that the output vector
#' is the same length as the input vector.
#'
#' @param fn A function. This is expected to be a function evaluating the
#' density of a distribution at numeric values, and suitable to be passed to
#' `delay_density` in `cfr_*()`.
#' @param n_req_args The number of required arguments, i.e., arguments without
#' default values.
#' @param n The number of elements over which to evaluate the function `fn`.
#' Defaults to 10, and `fn` is evaluated over `seq(n)`.
#'
#' @return A logical for whether the function `fn` fulfils conditions specified
#' in the respective checks.
#' @keywords internal
test_fn_req_args <- function(fn, n_req_args = 1) {
  checkmate::assert_count(n_req_args, positive = TRUE)
  # NOTE: using formals(args(fn)) to allow checking args of builtin primitives
  # for which formals(fn) would return NULL and cause the check to error
  # NOTE: errors non-informatively for specials such as `if`
  checkmate::test_function(fn) &&
    sum(mapply(function(x, y) {
      is.name(x) && y != "..."
    }, formals(args(fn)), names(formals(args(fn))))) == n_req_args
}

#' @name delay_density_helpers
test_fn_num_out <- function(fn, n = 10) {
  # use assert count to easily prevent values < 1
  checkmate::assert_count(n, positive = TRUE)
  checkmate::test_function(fn) &&
    checkmate::test_numeric(
      fn(seq(n)),
      lower = 0,
      any.missing = FALSE, finite = TRUE, len = n
    )
}
