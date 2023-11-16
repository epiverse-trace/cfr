## code to prepare `covid_data` dataset
# Data adapted from the \{covidregionaldata\} package of daily cases and
# deaths from the 19 countries with 100,000 or more deaths over the period
# 2020-01-01 to 2022-12-31. See the **References** for the publication which
# links to data sources made available through \{covidregionaldata\}.
# Included as \{covidregionaldata\} is no longer on CRAN.
# Data are provided as a `<data.frame>`.
#
# Citation:
# Joseph Palmer, Katharine Sherratt, Richard Martin-Nielsen, Jonnie Bevan,
# Hamish Gibbs, Sebastian Funk and Sam Abbott (2021). covidregionaldata:
# Subnational data for COVID-19 epidemiology. \doi{10.21105/joss.03290}
covid_data <- read.csv(
  system.file(
    "extdata", "covid_data.csv",
    package = "cfr",
    mustWork = TRUE
  )
)
covid_data$date <- as.Date(covid_data$date)

usethis::use_data(covid_data, overwrite = TRUE)
