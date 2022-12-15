#' Neatly format CFR estimates
#'
#' @description Format the CFR MLE and 95% range estimates neatly, giving the
#' estimates to 3 significant figures and presenting the MLE and 95% interval
#' on a single line
#'
#' @param cfr_in A named vector of the format outputted by [estimate_ccfr()]
#' or [static_cfr()]
#'
#' @return Prints the output of [estimate_ccfr()] or [static_cfr()] neatly to
#' the R terminal
#' @export
#'
#' @examples
#' # Get onset to death distribution from epiparameter
#' onset_to_death_ebola <- epiparameter::epidist("ebola", "onset_to_death")$pmf
#'
#' # load Ebola 1976 outbreak data
#' data("ebola1976")
#' # Calculate static naive CFR
#' ncfr <- static_cfr(df_in = ebola1976, correct_for_delays = FALSE)
#'
#' # Calculate static corrected CFRs
#' ccfr <- static_cfr(
#'   df_in = ebola1976,
#'   correct_for_delays = TRUE,
#'   delay_pmf = onset_to_death_ebola
#' )
#'
#' # Formats the output of the CFR data.frames nicely
#' # and prints to the terminal
#' format_cfr_neatly(ncfr)
#' format_cfr_neatly(ccfr)
format_cfr_neatly <- function(cfr_in) {
  if (is.data.frame(cfr_in)) {
    stop("`cfr_in` only works with static estimates - you may have provided a
    rolling CFR estimate")
  }
  stopifnot(
    "`cfr_in` must be numeric vector with the names `cfr_me`, \
    `cfr_low`, `cfr_high`" =
      (is.numeric(cfr_in) &
        all(utils::hasName(cfr_in, c("cfr_me", "cfr_low", "cfr_high")))
      )
  )

  # combining the MLE estimate and the 95% interval neatly and
  # giving each to only 3 significant figures
  sprintf(
    "CFR: %.3f%% (95%% Ci: %.3f%% -- %.3f%%)",
    cfr_in["cfr_me"], cfr_in["cfr_low"], cfr_in["cfr_high"]
  )
}
