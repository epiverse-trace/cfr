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
  ebola1976,
  smooth_inputs = FALSE,
  burn_in_value = 1,
  correct_for_delays = FALSE
)

# Calculate corrected time-varying
tvcfr_corrected <- cfr_time_varying(
  ebola1976,
  epidist = onset_to_death_ebola,
  smooth_inputs = FALSE,
  burn_in_value = 1,
  correct_for_delays = TRUE
)

# Basic expectations
test_that("`cfr_time_varying`: Basic expectations", {
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
# Calculate naive time-varying CFR
tvcfr_naive_smoothed <- cfr_time_varying(
  ebola1976,
  smooth_inputs = TRUE,
  smoothing_window = 3,
  burn_in_value = 1,
  correct_for_delays = FALSE
)

# Calculate corrected time-varying
tvcfr_corrected <- cfr_time_varying(
  ebola1976,
  epidist = onset_to_death_ebola,
  smooth_inputs = FALSE,
  smoothing_window = 3,
  burn_in_value = 1,
  correct_for_delays = TRUE
)
