#' Neatly format CFR estimates
#'
#' @description Format the CFR MLE and 95% range estimates neatly, giving the
#' estimates to 3 significant figures and presenting the MLE and 95% interval
#' on a single line
#'
#' @param cfr_in A named vector of the format outputted by [estimate_ccfr()]
#' or [static_cfr()]
#'
#' @param estimate_type A required string describing whether a severity of
#' reporting estimate is to be formatted. The package calculates both severity
#' and under-ascertainment estimates and uses the same formatting function for
#' both
#'
#' @param type An optional string describing the estimate, typically used to
#' describe whether the estimate has been corrected for delays or not
#'
#' @return Prints the output of [estimate_ccfr()] or [static_cfr()] neatly to
#' the R terminal
#'
#' @export
#'
#' @examples
#' # load Ebola 1976 outbreak data
#' data("ebola1976")
#'
#' # add the location the dataset relates to, for ease of formatting at the end
#' # of this example
#' ebola1976$location <- "Democratic Republic of the Congo"
#'
#' # subset the data so we focus on the earlier half of the outbreak
#' df_ebola_subset <- subset(ebola1976, date <= "1976-09-30")
#'
#' # read epidist for EVD onset to death from {epiparameter}
#' onset_to_death_ebola <- epiparameter::epidist_db(
#'   disease = "Ebola Virus Disease",
#'   epi_dist = "onset_to_death",
#'   author = "Barry_etal"
#' )
#'
#' # Calculate static naive CFR
#' ncfr <- estimate_static(
#'   df_in = df_ebola_subset,
#'   correct_for_delays = FALSE,
#'   group_by = "location"
#' )
#'
#' # Calculate static corrected CFRs
#' ccfr <- estimate_static(
#'   df_in = ebola1976,
#'   correct_for_delays = TRUE,
#'   epi_dist = onset_to_death_ebola,
#'   group_by = "location"
#' )
#'
#' # Formats the output of the CFR data.frames nicely
#' # and prints to the terminal
#' format_output(ncfr, estimate_type = "severity", type = "Naive")
#' format_output(ccfr, estimate_type = "severity", type = "Corrected")
#'
format_output <- function(df_in,
                          estimate_type,
                          type = NULL) {
  stopifnot(
    "estimate_type must be one of:
            severity or reporting" =
      (estimate_type %in% c("severity", "reporting"))
  )

  df_out <- data.frame(
    "Location" = df_in[["location"]],
    "Estimate" =
      sprintf(
        "%.2f%% (95%% CI: %.2f%% - %.2f%%)",
        df_in[[paste0(estimate_type, "_me")]] * 100,
        df_in[[paste0(estimate_type, "_lo")]] * 100,
        df_in[[paste0(estimate_type, "_hi")]] * 100
      ),
    stringsAsFactors = FALSE
  )

  if (!is.null(type)) {
    df_out$Type <- type
  }

  return(df_out)
}
