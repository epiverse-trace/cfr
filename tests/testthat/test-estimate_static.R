#### Tests for the static CFR function estimate_static() ####
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

# Calculate static naive CFR
scfr_naive <- estimate_static(data = ebola1976, correct_for_delays = FALSE)

# Calculate static corrected CFRs
scfr_corrected <- estimate_static(
  data = ebola1976,
  correct_for_delays = TRUE,
  epidist = onset_to_death_ebola
)

# Basic expectations
test_that("`estimate_static`: Basic expectations", {
  # expect dataframes with specific columns
  expect_s3_class(scfr_naive, "data.frame")
  expect_s3_class(scfr_corrected, "data.frame")

  # expected names
  expected_names <- c("severity_me", "severity_lo", "severity_hi")
  # expect named columns
  expect_named(
    scfr_naive,
    expected_names
  )
  expect_named(
    scfr_corrected, expected_names
  )

  # snapshot tests for naive and corrected static CFR
  expect_snapshot(scfr_naive)
  expect_snapshot(scfr_corrected)

  # expect that all columns in naive static CFR have values between 0 and 1
  invisible(
    apply(scfr_naive, 2, function(x) {
      expect_true(all(x >= 0.0 & x <= 1.0))
    })
  )

  # expect that all columns in corrected static CFR have values between 0 and 1
  invisible(
    apply(scfr_corrected, 2, function(x) {
      expect_true(all(x >= 0.0 & x <= 1.0))
    })
  )
})

test_that("`estimate_static`: Errors and messages", {
  # expect error when corrected CFR requested without delay PMF
  expect_error(
    estimate_static(
      data = ebola1976,
      correct_for_delays = TRUE
    )
  )

  # expect error when columns are missing
  expect_error(
    estimate_static(
      data = ebola1976[, c("date", "cases")],
      correct_for_delays = FALSE
    )
  )

  # Input df_in is not a data.frame
  expect_error(
    estimate_static(c("cases" = 10, "deaths" = 2, "date" = as.Date(Sys.time())))
  )

  # Input dataframe has wrong column names
  df_in_malformed <- ebola1976
  df_in_malformed$date_time <- df_in_malformed$date
  df_in_malformed$date <- NULL

  expect_error(
    estimate_static(data = df_in_malformed, correct_for_delays = FALSE)
  )

  # Input dataframe `date` column has wrong class; POSIXct instead of Date
  df_in_malformed <- ebola1976
  df_in_malformed$date <- as.POSIXct(df_in_malformed$date)

  expect_error(
    estimate_static(data = df_in_malformed, correct_for_delays = FALSE)
  )

  # Input dataframe has non-sequential dates
  df_in_malformed <- ebola1976
  df_in_malformed <- df_in_malformed[-seq(10, 30), ]

  expect_error(
    estimate_static(data = df_in_malformed, correct_for_delays = FALSE),
    regexp = "(Input data must have sequential dates)*(none missing)*duplicated"
  )
})
