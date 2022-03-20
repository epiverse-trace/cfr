# Example CFR estimation with simulated data ----------------------------------

# Simulate case and death data
case_times <- c(1:365)
case_data <- 100*(1+0.5*sin(4*pi*case_times/365))
cfr_vals <- (1-0.8*(1:length(death_data))/365)*0.05
death_data <- c(rep(0,15),head(case_data,-15))*cfr_vals # CFR=5%

# Parameter conversion:
ms_to_logms <- function(mean,sd){c(logmean=log(mean^2/sqrt(sd^2 + mean^2)),logsd=sqrt(log(1 + (sd^2/mean^2))))}

# Define delay distributions
o2d_mean <- 15; o2d_sd <- 5
rep_val <- ms_to_logms(o2d_mean,o2d_sd)
onset_to_death <- function(x){plnorm(x,meanlog=rep_val[["logmean"]],sdlog=rep_val[["logsd"]]) - 
    plnorm(x-1,meanlog=rep_val[["logmean"]],sdlog=rep_val[["logsd"]])}

cfr_est <- cfr_calculationn(case_time,case_data,death_data,onset_to_death)

# Plot estimates vs simple naive CFR calculation
plot(case_times,cfr_vals,ylim=c(0,0.1))
lines(case_times,cfr_est$deaths/cfr_est$onset_est,col="blue",lwd=2)
lines(case_times,death_data/case_data,col="red")



