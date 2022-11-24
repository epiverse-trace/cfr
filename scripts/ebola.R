library(datadelay)
library(lubridate)
library(epiparameter)

# read in the dataset in question. 
# dataset needs 
df_ebola <- read.csv("data/ebola_1976.csv")

df_ebola$date <- ymd(df_ebola$date)

# read in onset to death distribution
onset_to_death_ebola <- epidist("ebola","onset_to_death")$pmf

# calculate static naive and corrected CFRs
ncfr <- static_cfr(df_ebola, correct_for_delays = FALSE)
ccfr <- static_cfr(df_ebola, correct_for_delays = TRUE, onset_to_death_ebola)

# just a function that formats the output of the CFR data.frames 
# nicely and prints to the terminal
format_cfr_neatly(ncfr)
format_cfr_neatly(ccfr)

# calculate rolling naive and corrected CFRs
df_ncfr <- rolling_cfr(df_ebola, correct_for_delays = FALSE)
df_ccfr <- rolling_cfr(df_ebola, correct_for_delays = TRUE, 
                       onset_to_death_ebola)

# plotting rolling estimates
plot_data_and_cfr(df_ncfr, df_ccfr)
