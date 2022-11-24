#' Function to format the CFR MLE and 95% range estimates neatly, giving the
#' estimates to 3 significant figures and presenting the MLE and 95% interval 
#' on a single line
#'
#' @param df_in A data.frame of the format outputted by ccfr_uncertainty.R
#'
#' @return Prints the output of ccfr_uncertainty.R neatly to the R terminal
#' @export
#'
#' @examples
#' format_cfr_neatly(data.frame(cfr_me = 0.5, cfr_lo = 0.2, cfr_hi = 0.7))
format_cfr_neatly <- function(df_in) {
  
  # combining the MLE estimate and the 95% interval neatly and 
  # giving each to only 3 significant figures
  paste0(paste0(signif(df_in$cfr_me*100, 3), "% "),
         "(95% Ci: ",
         paste0(signif(df_in$cfr_lo*100, 3), "%"), 
         "--",
         paste0(signif(df_in$cfr_hi*100, 3), "%)"))
  
}
