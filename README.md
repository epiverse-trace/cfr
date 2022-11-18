# datadelay

`R` package to transform surveillance data based on epidemiological delay distributions. This initial version outlines functions that will be refined/superceded as pipelines develop.

## Installation

The easiest way to install the development version of `datadelay` is to use the `devtools` package:

```
# install.packages("devtools")
library(devtools)
install_github("epiverse-trace/datadelay")
library(datadelay)
```

## Quick start

The example below loads COVID-19 case and death data from the United States using the `covidregionaldata` package, then estimates case fatality risk using parameters from the `epiparameter` package.

```r
# Load dependencies
# install.packages("devtools")
# install.packages("data.table")
# install.packages("ggplot2")
# library(devtools)
# install_github("epiverse-trace/epiparameter")
# install_github("epiverse-trace/datadelay")
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

![plot]("plots/ebola_1976.png")

```

