library(data.table)
library(lubridate)
library(patchwork)
library(epiparameter)
library(ggplot2)

# reading in the data
dt_ebola <- fread("data/ebola_1976.csv")

# sourcing the functions to perform the calculations and plots
source("R/functions.R")
source("R/plot.R")

# munging the data so all variables are of the right class
dt_ebola[, cases := as.numeric(cases)]
dt_ebola[, deaths := as.numeric(deaths)]
dt_ebola[, date := ymd(date)]
dt_ebola[, date_num := .GRP, by = "date"]

# using the package epiparameter to access the probability mass function
onset_to_death_ebola <- epidist("ebola","onset_to_death")$pmf

# calculating both the naive and corrected (for delays) CFRs, 
# following the methods of Nishura
# with a simple binomial likelihood for the confidence intervals
dt_ebola_cfr <- calculate_rolling_cfrs(dt_ebola, 
                                       smooth_incidence = TRUE, 
                                       rolling_window = 7)

# putting the data in a slightly easier form to plot
dt_ebola_ncfr <- dt_ebola_cfr[, c("date", "ncfr_me", "ncfr_lo", "ncfr_hi")][, type := "ncfr"]
dt_ebola_ccfr <- dt_ebola_cfr[, c("date", "ccfr_me", "ccfr_lo", "ccfr_hi")][, type := "ccfr"]
             
setnames(dt_ebola_ncfr, c("ncfr_me", "ncfr_lo", "ncfr_hi"), c("me", "lo", "hi"))
setnames(dt_ebola_ccfr, c("ccfr_me", "ccfr_lo", "ccfr_hi"), c("me", "lo", "hi"))

dt_ebola_cfrs <- rbind(dt_ebola_ncfr, dt_ebola_ccfr)

dt_ebola_long <- melt(dt_ebola_cfr[, c("date", "cases", "deaths", 
                                       "cases_smooth", "deaths_smooth")],
                      id.vars = "date")

# plotting the data and estimates
p_ebola <- plot_cfr_and_data(dt_ebola_long)

# ggsave("plots/ebola_1976.pdf",
#        p_ebola,
#        width = 10,
#        heigh = 6)

ggsave("plots/ebola_1976.png",
       p_ebola,
       width = 10,
       heigh = 6)
