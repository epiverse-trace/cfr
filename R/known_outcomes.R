#' Calculates the number of individuals with known outcomes from a case and
#' death time series of data of an outbreak, up to the time point supplied. Can 
#' either calculate the daily new number of known outcomes or the cumulative 
#' number. Uses the probability mass function representing the delay between
#' case detection and death, typically approximated by a symptom onset to death
#' distribution from literature for the disease in question
#'
#' @param df_in A data.frame containing the outbreak data. A daily time series
#' with dates or some other absolute indicator of time (e.g. epiday/epiweek) and 
#' the numbers of new cases and new deaths at each time point
#' 
#' @param delay_dist The delay distribution used, in the form of a probability 
#' mass function parameterised by time. I.e. f(t) which gives the probability a
#' case has a known outcomes (i.e. death) at time t, parameterised with
#' disease-specific parameters before it is supplied here. A typical example 
#' would be a symptom onset to death delay distribution
#' 
#' @param cumulative A boolean flag to indicate whether the user wants the daily
#' or total number of known outcomes
#'
#' @return A data.frame containing the MLE estimate and 95% confidence interval 
#' of the corrected CFR
#' @export
#'
#' @examples
#' read in some data
#' df_ebola <- read.csv("data/ebola_1976.csv")
#' 
#' df_ebola$date <- ymd(df_ebola$date)
#' 
#' access a relevant symptom onset to death distribution
#' onset_to_death_ebola <- epidist("ebola","onset_to_death")$pmf
#' 
#' df_known_outcomes <- known_outcomes(df_ebola, onset_to_death_ebola)
known_outcomes <- function(df_in,
                           delay_pmf,
                           cumulative = TRUE) {
  
  # creating copy of data.frame to output at the end of the function
  df_out <- df_in
  
  # extracting the case time series and removing first element 
  # so the weights and cases match - see Nishiura et al. for definition of
  # correction term used
  cases <- df_out$cases
  
  # numerically evaluate the onset to death distribution over the same
  # number of time points as the length of the case time series
  onset_wts <- delay_pmf(seq(1, length(cases)))
  
  # convolve the onset to death distribution with the reverse of the 
  # case time series. Use the reverse of the case time series to match the
  # formal definition of a convolution (see convolution() documentation for
  # further explanation, using ?convolution() command)
  df_out$known_outcomes <- convolve(onset_wts, rev(cases), type = "c")
  
  # performing the cumulative sum as per the function argument. TRUE by default.
  if(cumulative == TRUE) {
    df_out$known_outcomes <- cumsum(df_out$known_outcomes)
  } 
  
  return(df_out)
}
