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
#' # read epidist for EVD onset to death from {epiparameter}
#' onset_to_death_ebola <- epiparameter::epidist_db(
#'   disease = "Ebola Virus Disease",
#'   epi_dist = "onset_to_death",
#'   author = "Barry_etal"
#' )
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
#'   epi_dist = onset_to_death_ebola
#' )
#'
#' # Formats the output of the CFR data.frames nicely
#' # and prints to the terminal
#' format_cfr_neatly(ncfr)
#' format_cfr_neatly(ccfr)
format_cfr_neatly <- function(cfr_in, type) {
  
  df_out <- data.frame(
    "CFR_estimate" = 
      sprintf("%.2f%% (95%% CI: %.2f%% - %.2f%%)", 
              cfr_in[[1]]*100, 
              cfr_in[[2]]*100, 
              cfr_in[[3]]*100), 
    "Type" = type)
  
  return(df_out)
}
