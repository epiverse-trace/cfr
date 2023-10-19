#### Tests for the static CFR function cfr_static() ####
# prepare data and common testing elements

# Ebola onset to death distribution comes from Barry et al. 2018
# a gamma distribution with k = 2.40, theta = 3.33

# Load ebola 1976 outbreak data
data("ebola1976")

# Calculate static naive CFR
scfr_naive <- cfr_static(data = ebola1976)

# Calculate static corrected CFRs
scfr_corrected <- cfr_static(
  data = ebola1976,
  delay_density = function(x) dgamma(x, shape = 2.40, scale = 3.33)
)

# Basic expectations
test_that("Static CFR estimate, basic expectations", {
  # expect dataframes with specific columns
  expect_s3_class(scfr_naive, "data.frame")
  expect_s3_class(scfr_corrected, "data.frame")

  # expected names
  expected_names <- c("severity_mean", "severity_low", "severity_high")
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

test_that("Static CFR estimate, errors and messages", {
  # expect error when columns are missing
  expect_error(
    cfr_static(
      data = ebola1976[, c("date", "cases")]
    )
  )

  # Input df_in is not a data.frame
  expect_error(
    cfr_static(c(cases = 10, deaths = 2, date = as.Date(Sys.time())))
  )

  # Input dataframe has wrong column names
  df_in_malformed <- ebola1976
  df_in_malformed$date_time <- df_in_malformed$date
  df_in_malformed$date <- NULL

  expect_error(
    cfr_static(data = df_in_malformed)
  )

  # Input dataframe `date` column has wrong class; POSIXct instead of Date
  df_in_malformed <- ebola1976
  df_in_malformed$date <- as.POSIXct(df_in_malformed$date)

  expect_error(
    cfr_static(data = df_in_malformed)
  )

  # Input dataframe has non-sequential dates
  df_in_malformed <- ebola1976
  df_in_malformed <- df_in_malformed[-seq(10, 30), ]

  expect_error(
    cfr_static(data = df_in_malformed),
    regexp = "(Input data must have sequential dates)*(none missing)*duplicated"
  )
})
