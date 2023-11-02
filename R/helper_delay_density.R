#' Check delay density functions passed to `cfr_*()`
#'
#' Checks whether a function has a given number of required arguments.
#'
#' @param fn A function.
#' @param n_req_args The number of required arguments, i.e., arguments without
#' default values.
#'
#' @return A logical for whether the function `fn` has the number of required
#' arguments specified by the user.
#' @keywords internal
test_fn_req_args <- function(fn, n_req_args = 1) {
  checkmate::assert_count(n_req_args, positive = TRUE)
  checkmate::test_function(fn) &&
#' @name delay_density_helpers
test_fn_num_out <- function(fn, n = 10) {
  # use assert count to easily prevent values < 1
  checkmate::assert_count(n, positive = TRUE)
  checkmate::test_function(fn) &&
    checkmate::test_numeric(
      fn(seq(10)),
      lower = 0,
      any.missing = FALSE, finite = TRUE, len = 10L
    )
}
