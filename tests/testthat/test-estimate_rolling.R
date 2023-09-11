#### Tests for the rolling static CFR function estimate_rolling() ####
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

# Calculate rolling static naive CFR
rolling_scfr_naive <- estimate_rolling(
  data = ebola1976, correct_for_delays = FALSE
)

# Calculate static corrected CFRs
rolling_scfr_corrected <- estimate_rolling(
  data = ebola1976,
  correct_for_delays = TRUE,
  epidist = onset_to_death_ebola
)

# Basic expectations
test_that("`estimate_static`: Basic expectations", {
  # expect dataframes with specific columns
  expect_s3_class(rolling_scfr_naive, "data.frame")
  expect_s3_class(rolling_scfr_corrected, "data.frame")

  # expect rows are identical to each method and to original data
  expect_identical(
    nrow(rolling_scfr_naive),
    nrow(rolling_scfr_corrected)
  )
  expect_identical(
    nrow(rolling_scfr_naive),
    nrow(ebola1976)
  )

  # expected names
  expected_names <- c("severity_me", "severity_lo", "severity_hi")
  # expect named columns
  expect_named(
    rolling_scfr_naive,
    expected_names
  )
  expect_named(
    rolling_scfr_corrected, expected_names
  )

  # snapshot tests for naive and corrected static CFR
  rows <- 15L
  expect_snapshot(head(rolling_scfr_naive, rows))
  expect_snapshot(head(rolling_scfr_corrected, rows))

  # expect that all columns in naive static CFR have values between 0 and 1
  invisible(
    apply(rolling_scfr_naive, 2, function(x) {
      expect_true(all((x >= 0.0 & x <= 1.0) | is.na(x)))
    })
  )

  # expect that all columns in corrected static CFR have values between 0 and 1
  invisible(
    apply(rolling_scfr_corrected, 2, function(x) {
      expect_true(all((x >= 0.0 & x <= 1.0) | is.na(x)))
    })
  )
})

# Statistical correctness of estimate_rolling()
# the final value should be the same as estimate_static()
# for the corresponding value of corrected_for_delays
test_that("`estimate_rolling`: Comparison with `estimate_static()`", {
  expect_equal(
    tail(rolling_scfr_naive, 1),
    estimate_static(
      ebola1976
    ),
    ignore_attr = TRUE
  )

  expect_equal(
    tail(rolling_scfr_corrected, 1),
    estimate_static(
      ebola1976,
      correct_for_delays = TRUE,
      epidist = onset_to_death_ebola
    ),
    ignore_attr = TRUE
  )
})

test_that("`estimate_static`: Errors and messages", {
  # expect error when corrected CFR requested without delay PMF
  expect_error(
    estimate_rolling(
      data = ebola1976,
      correct_for_delays = TRUE
    )
  )

  # expect error when columns are missing
  expect_error(
    estimate_rolling(
      data = ebola1976[, c("date", "cases")],
      correct_for_delays = FALSE
    )
  )

  # Input df_in is not a data.frame
  expect_error(
    estimate_rolling(
      c("cases" = 10, "deaths" = 2, "date" = as.Date(Sys.time()))
    )
  )

  # Input dataframe has wrong column names
  df_in_malformed <- ebola1976
  df_in_malformed$date_time <- df_in_malformed$date
  df_in_malformed$date <- NULL

  expect_error(
    estimate_rolling(data = df_in_malformed, correct_for_delays = FALSE)
  )

  # Input dataframe `date` column has wrong class; POSIXct instead of Date
  df_in_malformed <- ebola1976
  df_in_malformed$date <- as.POSIXct(df_in_malformed$date)

  expect_error(
    estimate_rolling(data = df_in_malformed, correct_for_delays = FALSE)
  )

  # Input dataframe has non-sequential dates
  df_in_malformed <- ebola1976
  df_in_malformed <- df_in_malformed[-seq(10, 30), ]

  expect_error(
    estimate_rolling(data = df_in_malformed, correct_for_delays = FALSE),
    regexp = "(Input data must have sequential dates)*(none missing)*duplicated"
  )
})
