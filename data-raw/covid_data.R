## code to prepare `covid_data` dataset goes here

library(dplyr)

# get data from covidregionaldata
covid_data <- covidregionaldata::get_national_data(
  source = "who", verbose = FALSE
)

# rename columns
covid_data <- rename(covid_data, cases = cases_new, deaths = deaths_new)

# get countries with >= 100,000 deaths over the first three
# years of the pandemic, 2020, 2021, and 2022
covid_data <- covid_data %>%
  filter(date < "2023-01-01") %>%
  group_by(iso_code) %>%
  mutate(total_deaths = max(deaths_total)) %>%
  filter(total_deaths > 100000 & !is.na(country)) %>%
  ungroup()

# select columns
covid_data <- covid_data %>%
  select(date, country, cases, deaths)

# convert to data.frame
covid_data <- as.data.frame(covid_data)

usethis::use_data(covid_data, overwrite = TRUE)
