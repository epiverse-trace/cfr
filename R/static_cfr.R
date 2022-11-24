#' Calculates the CFR at a single point in time using in the case and death time
#' series supplied. Uses all of the time points supplied by the user and returns
#' an estimate for a single point in time
#'
#' @param df_in A data.frame containing the outbreak data. A daily time series
#' with dates or some other absolute indicator of time (e.g. epiday/epiweek) and 
#' the numbers of new cases and new deaths at each time point
#' 
#' @param correct_for_delays A boolean flag indicating whether the user wishes
#' to correct for the delay between case detection and death. FALSE corresponds
#' to a naive CFR being calculated, TRUE corresponds to the user calculating a
#' corrected CFR
#' 
#' @param delay_pmf The delay distribution used, in the form of a probability 
#' mass function parameterised by time. I.e. f(t) which gives the probability a
#' case has a known outcomes (i.e. death) at time t, parameterised with
#' disease-specific parameters before it is supplied here. A typical example 
#' would be a symptom onset to death delay distribution
#'
#' @return A data.frame containing the MLE and 95% confidence interval of the
#' CFR estimates
#' @export
#'
#' @examples
#' library(lubridate)
#' library(epiparameter)
#' 
#' # read in the dataset in question. 
#' # dataset needs 
#' df_ebola <- read.csv("data/ebola_1976.csv")
#' 
#' df_ebola$date <- ymd(df_ebola$date)
#' 
#' # read in onset to death distribution
#' onset_to_death_ebola <- epidist("ebola","onset_to_death")$pmf
#' 
#' calculate static naive and corrected CFRs
#' ncfr <- calcalate_static_cfr(df_ebola, correct_for_delays = FALSE)
#' ccfr <- calcalate_static_cfr(df_ebola, correct_for_delays = TRUE, onset_to_death_ebola)
#' 
#' # just a function that formats the output of the CFR data.frames 
#' # nicely and prints to the terminal
#' format_cfr_output_neatly(ncfr)
#' format_cfr_output_neatly(ccfr)
static_cfr <- function(df_in,
                       correct_for_delays = TRUE,
                       delay_pmf) {
  
  # returns error message if no delay distribution is supplied, but correction
  # for delays was requested
  if(correct_for_delays == TRUE & missing(delay_pmf)) {
    stop("To correct for the delay between case detection and death,
  please specify an onset-to-death (or similar) probability mass function")
    
  }
  
  # calculating the naive CFR: (total deaths)/(total cases)
  # with the standard binomial 
  if(correct_for_delays == FALSE) {
    
    # calculating the total number of cases (without correecting) and deaths
    total_cases <- sum(df_in$cases)
    total_deaths <- sum(df_in$deaths)
    
    # calculating the central estimate
    cfr_me <- total_deaths/total_cases
    
    # calculating the lower and upper 95% confidence interval using the exact
    # binomial test
    cfr_conf <- binom.test(round(total_deaths), total_cases, p = 1)
    
    # extracting the lower and upper intervals respectively
    cfr_lo <- cfr_conf$conf.int[1]
    cfr_hi <- cfr_conf$conf.int[2]
    
    # putting together the estimates 
    cfr_estimate <- data.frame(cfr_me = cfr_me, 
                               cfr_lo = cfr_lo,
                               cfr_hi = cfr_hi)
  }
  
  # calculating the corrected cfr, corrected for delay between case detection 
  # and outcome
  if(correct_for_delays == TRUE) {
    
    # calculating the number of cases with known outcome, used as a replacement
    # for total deaths in the original cfr formula
    df_corrected <- known_outcomes(df_in,
                                   delay_pmf,
                                   cumulative = FALSE)
    
    # calculating the total number of cases and deaths after correcting for
    # the number of cases with known outcomes and using this estimate as the
    # of deaths
    total_cases <- sum(df_corrected$cases)
    total_deaths <- sum(df_corrected$deaths)
    
    # calculating the proportion of cases with known outcome
    u_t <- sum(df_corrected$known_outcomes)/total_cases
    
    # calculating the maximum likelihood estimate and 95% confidence interval
    # using the binomial likelihood function from Nishiura
    cfr_estimate <- ccfr_uncertainty(total_cases, total_deaths, u_t)
  }
  
  return(cfr_estimate)
}