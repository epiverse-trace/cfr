#' Estimate known outcomes from case and death time-series data
#'
#' @description Estimates the expected number of individuals with known outcomes
#' from a case and death time series of data of an outbreak, up to the time
#' point supplied.
#' Can either calculate the daily new number of known outcomes or the cumulative
#' number.
#' Uses the probability mass function representing the delay between
#' case detection and death, typically approximated by a symptom onset to death
#' distribution from the literature for the disease in question.
#'
#' @param df_in A data.frame containing the outbreak data. A daily time series
#' with dates or some other absolute indicator of time (e.g. epiday/epiweek) and
#' the numbers of new cases and new deaths at each time point
#'
#' @param epi_dist The delay distribution used, in the form of an
#' [epiparameter::epidist()] object. This is used to obtain a probability
#' mass function parameterised by time; i.e. \eqn{f(t)} which gives the
#' probability a case has a known outcomes (i.e. death) at time \eqn{t},
#' parameterised with disease-specific parameters before it is supplied here.
#' A typical example would be a symptom onset to death delay distribution.
#'
#' @param cumulative A boolean flag to indicate whether the user wants the daily
#' or total number of known outcomes
#'
#' @return A data.frame containing the MLE estimate and 95% confidence interval
#' of the corrected CFR
#' @export
#'
#' @examples
#' # Load Ebola 1976 outbreak data
#' data("ebola1976")
#'
#' # read epidist for EVD onset to death from {epiparameter}
#' onset_to_death_ebola <- epiparameter::epidist_db(
#'   disease = "Ebola Virus Disease",
#'   epi_dist = "onset_to_death",
#'   author = "Barry_etal"
#' )
#'
#' df_known_outcomes <- known_outcomes(df_in = ebola1976, onset_to_death_ebola)
known_outcomes <- function(df_in,
                           epi_dist,
                           cumulative = TRUE) {
  # some input checking
  stopifnot(
    "Case data must be a data.frame" =
      (is.data.frame(df_in)),
    "Case data must contain columns `cases` and `deaths`" =
      (all(c("cases", "deaths") %in% colnames(df_in))),
    "Option `cumulative` must be `TRUE` or `FALSE`" =
      (is.logical(cumulative))
  )
  if (!missing(epi_dist)) {
    stopifnot(
      "`epi_dist` must be an `epidist` object" =
        (epiparameter::is_epidist(epi_dist))
    )
  }

  # nolint start
  # Code following https://github.com/adamkucharski/ebola-cfr/
  # blob/e2683eee62fb6fef8140b4b1d9dc13d542c5eacb/R/cfr_function.R#L12-L22
  # nolint end
  pmf_vals <- stats::density(
    epi_dist,
    at = seq(from = 0, to = nrow(df_in) - 1L)
  )
  
  # defining vectors to be used in the main loop
  cases <- df_in$cases
  
  # the times at which cases are reported, in numbers of days (or whichever
  # time units are used) since the first case was reported
  case_times <- as.numeric(df_in$date - min(df_in$date, na.rm = TRUE),
                           units = "days") + 1
  
  # the total number of time points at which cases were reported
  case_length <- length(case_times)
  
  # declaring the outputs of the main loop as vectors within the main 
  # data.frame
  df_in$known_outcomes <- numeric(case_length)
  df_in$u_t <- numeric(case_length)
  
  # main calculation loop
  for(i in case_length:1){
    
    # Delay probability mass function, evaluated at times
    # within the case and death times series 
    delay_pmf_eval <- pmf_vals[case_times[1:i]]
    
    # Estimate the number of onsets associated with deaths
    known_onsets_current <- cases[1:i]*rev(delay_pmf_eval)
    
    # Collecting all current known onset estimates in a new
    # column of the original data.frame
    df_in$known_outcomes[[i]] <- sum(known_onsets_current)
    
    #--- CALCULATE BINOMIAL CONFIDENCE INTERVAL (DIRECTLY)
    #--- HERE FOR TIME-VARYING CFR
    
    # Calculating the proportion of cases with known onset,
    # for use in the simple likelihood function
    df_in$u_t <- cumsum(df_in$known_outcomes)/cumsum(cases)
  }
  
  return(df_in)
  
}
