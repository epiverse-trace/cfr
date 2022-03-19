# Example code:

# Example estimation with simulated data ----------------------------------


# Simulate case data
case_times <- c(1:365)
case_data <- 100*(1+0.5*sin(4*pi*case_times/365))

# Parameter conversion:
ms_to_logms <- function(mean,sd){c(logmean=log(mean^2/sqrt(sd^2 + mean^2)),logsd=sqrt(log(1 + (sd^2/mean^2))))}

# Define delay distributions
i2o_mean <- 10; i2o_sd <- 5
onset_val <- ms_to_logms(i2o_mean,i2o_sd)
infection_to_onset <- function(x){plnorm(x,meanlog=onset_val[["logmean"]],sdlog=onset_val[["logsd"]]) - 
    plnorm(x-1,meanlog=onset_val[["logmean"]],sdlog=onset_val[["logsd"]])}

o2r_mean <- 5; o2r_sd <- 3
rep_val <- ms_to_logms(o2r_mean,o2r_sd)
onset_to_report <- function(x){plnorm(x,meanlog=rep_val[["logmean"]],sdlog=rep_val[["logsd"]]) - 
    plnorm(x-1,meanlog=rep_val[["logmean"]],sdlog=rep_val[["logsd"]])}

infection_est <- cases_to_infections(case_time,case_data,infection_to_onset,onset_to_report)

# Plot estimates vs simple naive shift
plot(case_times,case_data,ylim=c(0,max(case_data)))
lines(infection_est$infection_times,infection_est$infection_estimate,col="blue")
lines(case_times-(i2o_mean+o2r_mean),case_data,col="red")


