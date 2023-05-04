#' Neatly format CFR estimates
#'
#' @description Format the CFR MLE and 95% range estimates neatly, giving the
#' estimates to 3 significant figures and presenting the MLE and 95% interval
#' on a single line
#'
#' @param cfr_in A named vector of the format outputted by [estimate_ccfr()]
#' or [static_cfr()]
#'
#' @param type A string describing the type of severity estimate. Typical
#' entries would be CFR, HFR, Naive, Corrected, etc.
#'
#' @return Prints the output of [estimate_ccfr()] or [static_cfr()] neatly to
#' the R terminal
#' 
#' @export
#'
#' @examples
#' data("ebola1976")
#' 
#' df_ebola_subset <- subset(ebola1976, date <= "1976-09-18")
#' 
#' onset_to_death_ebola <- epiparameter::epidist_db(
#'   disease = "Ebola Virus Disease", 
#'   epi_dist = "onset_to_death",
#'   author = "Barry_etal")
#'   
#' static_cfr(
#'   df_ebola_subset,
#'   correct_for_delays = FALSE) |> 
#'   format_cfr_neatly(type = "Naive")
#'   
format_cfr_neatly <- function(cfr_in, type) {
  
  df_out <- data.frame(
    "CFR_estimate" = 
      sprintf("%.2f%% (95%% CI: %.2f%% - %.2f%%)", 
              cfr_in[[1]]*100, 
              cfr_in[[2]]*100, 
              cfr_in[[3]]*100), 
    "Type" = type,
    stringsAsFactors = FALSE)
  
  return(df_out)
}
