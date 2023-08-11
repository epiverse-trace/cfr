## code to prepare `covid_data` dataset goes here

covid_data <- read.csv(
  system.file(
    "extdata", "covid_data.csv",
    package = "cfr",
    mustWork = TRUE
  )
)
covid_data$date <- as.Date(covid_data$date)

usethis::use_data(covid_data, overwrite = TRUE)
