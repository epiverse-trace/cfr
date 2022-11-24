#' Calculates the CFR at each time point in the case and death time
#' series supplied, using an expanding window of time. I.e. the static CFR is 
#' calculated for each time point, using the time series from the start to each
#' time point increasing the number of time points included by one each
#' iteration
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
#' @return A data.frame containing the same case and death time series supplied
#' with the desired CFR (naive or corrected) appended at each time point
#' as extra columns
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
#' # calculate rolling naive and corrected CFRs
#' df_ncfr <- calcalate_rolling_cfr(df_ebola, correct_for_delays = FALSE)
#' df_ccfr <- calcalate_rolling_cfr(df_ebola, correct_for_delays = TRUE, 
#'                                  onset_to_death_ebola)
rolling_cfr <- function(df_in,
                        correct_for_delays = TRUE,
                        delay_pmf) {
  
  # returns error message if no delay distribution is supplied, but correction
  # for delays was requested
  if(correct_for_delays == TRUE & missing(delay_pmf)) {
    stop("To correct for the delay between case detection and death, please specify an
  onset-to-death (or similar) probability mass function")
    
  }
  
  # calculating the number of days in the time series to be looped over
  n_days <- seq(1:nrow(df_in))
  
  # pre-defining the data frame to be allocated to iteratively in the for loop
  df_cfr <- data.frame()
  
  # calculating the uncorrected CFR rolling over all days
  if(correct_for_delays == FALSE) {
    for(i in 1:max(n_days)) {
      
      out_current <- static_cfr(df_in[1:i, ], 
                                correct_for_delays = FALSE)
      
      df_cfr <- rbind(df_cfr, out_current)
    }
  } # calculating the uncorrected CFR rolling over all days
  else if(correct_for_delays == TRUE) {
    for(i in 1:max(n_days)) {
      out_current <- static_cfr(df_in[1:i, ], 
                                correct_for_delays = TRUE,
                                delay_pmf)
      
      df_cfr <- rbind(df_cfr, out_current)
    }
  }
  
  # attaching the dates, cases and deaths to the CFR estimates
  df_out <- cbind(date = df_in$date, 
                  cases = df_in$cases, 
                  deaths = df_in$deaths, 
                  df_cfr)
  
  return(df_out)
}