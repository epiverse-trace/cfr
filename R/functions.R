scale_cfr <- function(cases, 
                      deaths,
                      delay_fun){
  
  cumulative_known_t <- 0 # store cumulative cases with known outcome at time tt
  
  # Sum over cases up to time tt
  for(i in 1:length(cases)){
    known_i <- 0 # store number of cases with known clinical outcome at time ii
    for(j in 0:(i - 1)){ # iterate over cases up to this point
      known_j <- (cases[i - j]*delay_fun(j))
      known_i <- known_i + known_j
    }
    cumulative_known_t <- cumulative_known_t + known_i # tally cumulative known outcomes
  }
  
  D_t <- sum(deaths) # cumulative deaths
  C_t <- sum(cases) # cumulative cases
  
  u_t <- cumulative_known_t/sum(cases) # proportion of cases with known outcome
  
  # - - - 
  # Calculate naive CFR value and 95% binomial interval
  htest <- binom.test(D_t, C_t, p = 1, conf.level = 0.95)
  b_t <- c(D_t/C_t,htest$conf.int[1:2])
  
  # - - - 
  # MLE estimation for corrected CFR
  pprange <- seq(1e-3, 1, 1e-3)
  lik <- matrix(NA, nrow = length(pprange))
  
  for(ii in 1:length(pprange)){
    p_t <- pprange[ii]
    
    # Calculate likelihood - use binomial for small samples and Poisson approximation for larger numbers
    if(C_t<200){
      lik[ii] <- log(choose(round(u_t*C_t), D_t))+ D_t*log(p_t)+(u_t*C_t - D_t)*log(1 - p_t)
    } else {
      lik[ii] <- dpois(D_t,p_t*u_t*C_t, log = T)
    }
  }
  
  mid_val <- pprange[lik == max(lik)] # MLE estimate
  ci_95 <- pprange[lik >= (max(lik) - 1.92)]
  
  if(is.na(max(lik)) == TRUE) {
    mid_val <- NA
    ci_95 <- c(NA, NA)
  }
  
  ncfr_me = D_t/C_t
  ncfr_lo = htest$conf.int[1]
  ncfr_hi = htest$conf.int[2]
  
  dt_out <- data.table(ncfr_me = ncfr_me, 
                       ncfr_lo = ncfr_lo,
                       ncfr_hi = ncfr_hi,
                       ccfr_me = mid_val, 
                       ccfr_lo = min(ci_95),
                       ccfr_hi = max(ci_95))
  
  return(dt_out)  
}


scale_cfr_rolling <- function(dt_in, x) {
  
  dt_out <- scale_cfr(cases = dt_in[date_num %in% 1:x, cases],
                      deaths = dt_in[date_num %in% 1:x, deaths],
                      delay_fun = onset_to_death_ebola)
  
  return(dt_out[])
}

calculate_rolling_cfrs <- function(dt_in, 
                                   smooth_incidence = FALSE,
                                   rolling_window = 7) {
  
  dt_out <- dt_ebola
  rolling_window <- 7
  
  dt_out <- copy(dt_in)
  
  if(smooth_incidence == TRUE) {
    dt_out[, cases_smooth := round(frollmean(cases, n = rolling_window, fill = 0))]
    dt_out[, deaths_smooth := round(frollmean(deaths, n = rolling_window, fill = 0))]
    
    dt_out[, `:=` (ncfr_me = scale_cfr_rolling(dt_out, date_num)[, ncfr_me],
                   ncfr_lo = scale_cfr_rolling(dt_out, date_num)[, ncfr_lo],
                   ncfr_hi = scale_cfr_rolling(dt_out, date_num)[, ncfr_hi],
                   ccfr_me = scale_cfr_rolling(dt_out, date_num)[, ccfr_me],
                   ccfr_lo = scale_cfr_rolling(dt_out, date_num)[, ccfr_lo],
                   ccfr_hi = scale_cfr_rolling(dt_out, date_num)[, ccfr_hi]),
           by = date]
    
    dt_out[, cases := dt_in[, cases]]
    dt_out[, deaths := dt_in[, deaths]]
  } else {
    
    dt_out[, `:=` (ncfr_me = scale_cfr_rolling(dt_out, date_num)[, ncfr_me],
                   ncfr_lo = scale_cfr_rolling(dt_out, date_num)[, ncfr_lo],
                   ncfr_hi = scale_cfr_rolling(dt_out, date_num)[, ncfr_hi],
                   ccfr_me = scale_cfr_rolling(dt_out, date_num)[, ccfr_me],
                   ccfr_lo = scale_cfr_rolling(dt_out, date_num)[, ccfr_lo],
                   ccfr_hi = scale_cfr_rolling(dt_out, date_num)[, ccfr_hi]),
           by = date]
  }
  
  return(dt_out)
}
