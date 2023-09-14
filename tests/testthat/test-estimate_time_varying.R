#### Tests for the static CFR function cfr_static() ####
# prepare data and common testing elements

# read epidist for EVD onset to death from {epiparameter}
onset_to_death_ebola <- epiparameter::epidist_db(
  disease = "Ebola Virus Disease",
  epi_dist = "onset_to_death",
  author = "Barry_etal",
  single_epidist = TRUE
)

# Load ebola 1976 outbreak data
data("ebola1976")

# Calculate naive time-varying CFR
tvcfr_naive <- cfr_time_varying(
  ebola1976
)

# Calculate corrected time-varying
tvcfr_corrected <- cfr_time_varying(
  ebola1976,
  epidist = onset_to_death_ebola,
  burn_in_value = 1
)

# Basic expectations
test_that("`Time varying CFR, basic expectations", {
  # expect dataframes with specific columns
  expect_s3_class(tvcfr_naive, "data.frame")
  expect_s3_class(tvcfr_corrected, "data.frame")

  # expected names
  expected_names <- c(
    "date", "cases", "deaths",
    "severity_mean", "severity_low", "severity_high"
  )
  # expect named columns
  expect_named(
    tvcfr_naive, expected_names
  )
  expect_named(
    tvcfr_corrected, expected_names
  )

  # snapshot tests for naive and corrected static CFR
  expect_snapshot(head(tvcfr_naive, 15))
  expect_snapshot(tail(tvcfr_corrected, 15))

  # expect that all columns in naive static CFR have values between 0 and 1
  invisible(
    apply(
      tvcfr_naive[, grepl("severity", colnames(tvcfr_naive), fixed = TRUE)],
      2, function(x) {
        expect_true(all((x >= 0.0 & x <= 1.0) | is.na(x))) # allow for NAs
      }
    )
  )

  # expect that all columns in corrected static CFR have values between 0 and 1
  invisible(
    apply(
      tvcfr_corrected[, grepl("severity", colnames(tvcfr_naive), fixed = TRUE)],
      2, function(x) {
        expect_true(all((x >= 0.0 & x <= 1.0) | is.na(x))) # allow for NAs
      }
    )
  )
})

# Expectations when smoothing is applied to the data
data("covid_data")
# subset data
covid_uk <- covid_data[covid_data$country == "United Kingdom" &
  covid_data$date < "2021-01-01" & covid_data$date > "2020-05-01", ]

# read epidist for EVD onset to death from {epiparameter}
onset_to_death_covid <- epiparameter::epidist_db(
  disease = "Covid-19",
  epi_dist = "onset_to_death",
  author = "Linton_etal",
  single_epidist = TRUE
)

# Calculate naive time-varying CFR
tvcfr_naive_smoothed_3 <- cfr_time_varying(
  covid_uk,
  smoothing_window = 3,
  burn_in_value = 1
)

tvcfr_naive_smoothed_7 <- cfr_time_varying(
  covid_uk,
  smoothing_window = 7,
  burn_in_value = 1
)

test_that("Time-varying CFR with smoothing and burn in", {
  # expect that different smoothing produces different estimates
  expect_error(
    expect_identical(
      head(tvcfr_naive_smoothed_7),
      head(tvcfr_naive_smoothed_3)
    )
  )

  # expect that burn in gives NAs for burn_in - 1 rows
  burn_in <- 7L
  tvcfr_burnin_7 <- cfr_time_varying(
    covid_uk,
    burn_in_value = burn_in
  )

  expect_length(
    which(is.na(tvcfr_burnin_7$severity_mean)),
    burn_in - 1L
  )
})
