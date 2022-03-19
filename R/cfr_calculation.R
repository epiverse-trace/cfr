#' Case fatality risk calculation
#'
#' Function to estimate infection incidence from case data.
#' @param case_times Time values corresponding to case incidence
#' @param case_data Time series of case incidence data
#' @param death_data Time series of death incidence data, with same time scale as cases
#' @param onset_to_death PDF onset-to-death 
#' @keywords cases
#' @export
#' @examples man/examples/cfr_calculation_example
#' cfr_calculation()

cfr_calculationn <- function(case_times,case_data,death_data,onset_to_death){

  case_length <- length(case_times)

  # Store outputs of onsets corresponding to death incidence
  onset_matched <- numeric(case_length)

  # Compile onsets
  for(ii in case_length:1){
    
    # Extract deaths
    deaths_ii <- death_data[ii]
    
    # PDF truncated to match time series
    pdf_o2d <- onset_to_death(case_times[1:ii])
  
    # Estimate total onsets associated with deaths
    onset_estimate <- (case_data[1:ii]*rev(pdf_o2d)) %>% sum()
 
    # Collate onsets
    onset_matched[ii] <- c(onset_estimate)

  }

  data.frame(onset_est=onset_matched,deaths=death_data)
  
}
