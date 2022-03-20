#' Cases to infections
#'
#' Function to estimate infection incidence from case data.
#' @param case_times Time values corresponding to case incidence
#' @param case_data Time series of case incidence data
#' @param infection_to_onset Probability mass function for infection-to-onset 
#' @param onset_to_report Probability mass function for onset-to-report 
#' @param pre_window Period to reconstruct before first case data point
#' @keywords cases
#' @export
#' @examples man/examples/cases_to_infections_example
#' cases_to_infections()

cases_to_infections <- function(case_times,case_data,infection_to_onset,onset_to_report,pre_window=20){

  pwin <- pre_window
  case_length <- length(case_times)
  estimate_times <- (1-pre_window):case_length
  
  # Store outputs
  onset_vector <- numeric(length(estimate_times))
  infection_vector <- numeric(length(estimate_times))
  
  # Compile onsets
  for(ii in case_length:1){
    
    # Extract cases
    cases_ii <- case_data[ii]
    
    # PDF truncated to match time series
    pdf_o2s <- onset_to_report(estimate_times[1:(ii+pwin)]+pwin)
    
    # Collate onsets
    onset_vector[1:(ii+pwin)] <- onset_vector[1:(ii+pwin)] + rev(pdf_o2s*cases_ii)

  }
  
  # Compile infections
  for(ii in length(onset_vector):1){
    
    # Extract cases
    onsets_ii <- onset_vector[ii]
    
    # PDF truncated to match time series
    pdf_i2o <- infection_to_onset(estimate_times[1:ii]+pwin)
    
    # Collate onsets
    infection_vector[1:ii] <- infection_vector[1:ii] + rev(pdf_i2o*onsets_ii)

  }
  
  data.frame(infection_times=estimate_times, infection_estimate=infection_vector)
  
}
