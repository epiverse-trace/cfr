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
    identical(
      length(
        Filter(is.name, formals(fn))
      ),
      as.integer(n_req_args)
    )
}
