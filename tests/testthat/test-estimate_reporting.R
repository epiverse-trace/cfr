# Tests for estimate_reporting()
# Note that this is an internal function underlying estimate_static()
# when corrected_for_delays is TRUE

# load Ebola 1976 outbreak data
data("ebola1976")

# read epidist for EVD onset to death from {epiparameter}
onset_to_death_ebola <- epiparameter::epidist_db(
  disease = "Ebola Virus Disease",
  epi_dist = "onset_to_death",
  author = "Barry_etal"
)

poisson_threshold <- 100

test_that("`estimate_reporting`: Basic expectations for static method", {
  reporting_estimate <- estimate_reporting(
    data = ebola1976,
    correct_for_delays = FALSE, smooth_inputs = FALSE,
    burn_in_value = 1, smoothing_window = 1,
    type = "static"
  )

  expect_s3_class(reporting_estimate, "data.frame")
  expect_named(
    reporting_estimate, c("reporting_me", "reporting_lo", "reporting_hi")
  )
  expect_true(
    all(
      apply(reporting_estimate, 2, function(x) x >= 0.0 && x <= 1.0)
    )
  )
  expect_true(
    all(
      reporting_estimate$reporting_lo <= reporting_estimate$reporting_me &&
        reporting_estimate$reporting_me <= reporting_estimate$reporting_hi
    )
  )
  # snapshot test
  expect_snapshot(
    reporting_estimate
  )
})

test_that("`estimate_reporting`: Correct for delays for static method", {
  reporting_estimate <- estimate_reporting(
    data = ebola1976,
    correct_for_delays = TRUE, epi_dist = onset_to_death_ebola,
    smooth_inputs = FALSE,
    burn_in_value = 1, smoothing_window = 1,
    type = "static"
  )

  expect_s3_class(reporting_estimate, "data.frame")
  expect_named(
    reporting_estimate, c("reporting_me", "reporting_lo", "reporting_hi")
  )
  expect_true(
    all(
      apply(reporting_estimate, 2, function(x) x >= 0.0 && x <= 1.0)
    )
  )
  expect_true(
    all(
      reporting_estimate$reporting_lo <= reporting_estimate$reporting_me &&
        reporting_estimate$reporting_me <= reporting_estimate$reporting_hi
    )
  )
  # snapshot test
  expect_snapshot(
    reporting_estimate
  )
})

test_that("`estimate_reporting`: Smooth inputs for static method", {
  reporting_estimate <- estimate_reporting(
    data = ebola1976,
    correct_for_delays = FALSE, epi_dist = onset_to_death_ebola,
    smooth_inputs = TRUE,
    burn_in_value = 1, smoothing_window = 7,
    type = "static"
  )

  expect_s3_class(reporting_estimate, "data.frame")
  expect_named(
    reporting_estimate, c("reporting_me", "reporting_lo", "reporting_hi")
  )
  expect_true(
    all(
      apply(reporting_estimate, 2, function(x) x >= 0.0 && x <= 1.0)
    )
  )
  expect_true(
    all(
      reporting_estimate$reporting_lo <= reporting_estimate$reporting_me &&
        reporting_estimate$reporting_me <= reporting_estimate$reporting_hi
    )
  )
  # snapshot test
  expect_snapshot(
    reporting_estimate
  )
})

test_that("`estimate_reporting`: Automatic burn-in value", {
  reporting_estimate <- estimate_reporting(
    data = ebola1976,
    correct_for_delays = FALSE, epi_dist = onset_to_death_ebola,
    smooth_inputs = TRUE,
    smoothing_window = 7,
    type = "static"
  )

  expect_s3_class(reporting_estimate, "data.frame")
  expect_named(
    reporting_estimate, c("reporting_me", "reporting_lo", "reporting_hi")
  )
  expect_true(
    all(
      apply(reporting_estimate, 2, function(x) x >= 0.0 && x <= 1.0)
    )
  )
  expect_true(
    all(
      reporting_estimate$reporting_lo <= reporting_estimate$reporting_me &&
        reporting_estimate$reporting_me <= reporting_estimate$reporting_hi
    )
  )
  # snapshot test
  expect_snapshot(
    reporting_estimate
  )
})

test_that("`estimate_reporting`: Automatic burn-in value for static method", {
  reporting_estimate <- estimate_reporting(
    data = ebola1976,
    correct_for_delays = FALSE, epi_dist = onset_to_death_ebola,
    smooth_inputs = TRUE,
    smoothing_window = 7,
    type = "static"
  )

  expect_s3_class(reporting_estimate, "data.frame")
  expect_named(
    reporting_estimate, c("reporting_me", "reporting_lo", "reporting_hi")
  )
  expect_true(
    all(
      apply(reporting_estimate, 2, function(x) x >= 0.0 && x <= 1.0)
    )
  )
  expect_true(
    all(
      reporting_estimate$reporting_lo <= reporting_estimate$reporting_me &&
        reporting_estimate$reporting_me <= reporting_estimate$reporting_hi
    )
  )
  # snapshot test
  expect_snapshot(
    reporting_estimate
  )
})

test_that("`estimate_reporting`: Basic expectations for time-varying method", {
  reporting_estimate <- estimate_reporting(
    data = ebola1976,
    correct_for_delays = TRUE,
    epi_dist = onset_to_death_ebola,
    smooth_inputs = FALSE,
    type = "varying"
  )

  expect_s3_class(reporting_estimate, "data.frame")
  expect_named(
    reporting_estimate, c("reporting_me", "reporting_lo", "reporting_hi")
  )
  expect_true(
    all(
      apply(reporting_estimate, 2, function(x) x >= 0.0 && x <= 1.0)
    )
  )
  expect_true(
    all(
      reporting_estimate$reporting_lo <= reporting_estimate$reporting_me &&
        reporting_estimate$reporting_me <= reporting_estimate$reporting_hi
    )
  )
  # snapshot test
  expect_snapshot(
    reporting_estimate
  )
})
