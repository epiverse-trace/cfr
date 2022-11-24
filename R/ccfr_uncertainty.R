#' Calculates the maximum likelihood estimate and 95% confidence interval 
#' of a corrected CFR, using the total cases and total cases with known 
#' outcomes, where the latter replaces the total number of deaths in the 
#' standard (naive) CFR definition. We use a binomial likelihood, approximated
#' by a Poisson likelihood for large samples
#'
#' @param total_cases The total number of cases observed over the period of an
#' outbreak of interest. The total number of cases must be greater than or equal 
#' to the total number of deaths
#' 
#' @param total_deaths The total number of deaths observed over the period of an
#' outbreak of interest. The total number of deaths must be less than or equal to
#' the total number of cases
#' 
#' @param u_t The proportion of cases to cases with known outcomes up to the
#' point of an outbreak of interest. Used to correct the total number of deaths
#' for delays between case detection and outcome. Given that its a proportion,
#' it must be between 0 and 1
#'
#' @return A data.frame containing the MLE estimate and 95% confidence interval 
#' of the corrected CFR
#' @export
#'
#' @examples
#' ccfr_uncertainty(1000, 900, 0.2)
ccfr_uncertainty <- function(total_cases, 
                             total_deaths,
                             u_t) {
  
  # throwing error message if total deaths is strictly higher than total cases
  if(total_cases < total_deaths) {
    stop("The total number of cases is required to be equal or higher to the 
  total number of deaths")
  }
  
  # redefining the two input variables to reduce notation size
  C_t <- total_cases
  D_t <- total_deaths
  
  # MLE estimation for corrected CFR
  pprange <- seq(1e-3, 1, 1e-3)
  
  lik <- matrix(NA, nrow = length(pprange))
  
  for(i in 1:length(pprange)){
    p_t <- pprange[i]
    
    # Calculate likelihood - use binomial for small samples and Poisson
    # approximation for larger numbers
    if(C_t < 200) {
      lik[i] <- log(choose(C_t, D_t)) + D_t*log(p_t) + (C_t - D_t)*log(1 - p_t)
    } else {
      lik[i] <- dpois(D_t, p_t*u_t*C_t, log = T)
    }
  }
  
  # MLE estimate
  cfr_me <- pprange[lik == max(lik)] 
  
  # 95% range of likelihood
  cfr_range <- pprange[lik >= (max(lik) - 1.92)]
  
  # lower 95% confidence interval value
  cfr_lo <- min(cfr_range)
  
  # upper 95% confidence interval value
  cfr_hi <- max(cfr_range)
  
  if(is.na(max(lik)) == TRUE) {
    cfr_me <- NA
    cfr_lo <- NA
    cfr_hi <- NA
  }
  
  # putting together the estimates 
  cfr_estimate <- data.frame(cfr_me = cfr_me, 
                             cfr_lo = cfr_lo,
                             cfr_hi = cfr_hi)
  
  # returning data.frame with corrected estimates
  return(cfr_estimate)
}
