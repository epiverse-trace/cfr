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
# install.packages("covidregionaldata")
# install.packages("devtools")
# library(devtools)
# install_github("epiverse-trace/epiparameter")
# install_github("epiverse-trace/datadelay")
library(covidregionaldata)
library(epiparameter)
library(datadelay)


# Format incidence - - - - - - - - 
# Extract probability mass function for incubation period
incubation_covid <- prob_f(pathogen="SARS_CoV_2",type="incubation")
onset_delay <- function(x){ifelse(x==1,1,0)} # One day delay

# Example with US case data
covid_data_us <- get_national_data("united states",source="who")
case_time <- covid_data_us$date - min(covid_data_us$date)

# Calculate case fatality risk - - - - - - - - 

# Extract probability mass function for onset-to-death
onset2death_covid <- prob_f(pathogen="SARS_CoV_2",type="onset_to_death")

# Adjust for delay between new onsets and new deaths
timing_est <- cfr_calculation(case_time,covid_data_us$cases_new,covid_data_us$deaths_new,onset_to_death = onset2death_covid)
cfr <- timing_est$deaths/timing_est$onset_est

# Plot estimated CFR (blue) vs simple naive CFR calculation (red)
plot(case_times,cfr,col="blue",type="l",lwd=2,ylim=c(0,0.1),ylab="CFR")
lines(case_times,covid_data_us$deaths_new/covid_data_us$cases_new,col="red")

```
