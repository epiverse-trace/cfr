# Tests for estimate_ascertainment()
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

test_that("`estimate_ascertainment`: Basic expectations for static method", {
  ascertainment_estimate <- estimate_ascertainment(
    data = ebola1976,
    correct_for_delays = FALSE, smooth_inputs = FALSE,
    burn_in_value = 1, smoothing_window = 1,
    type = "static"
  )

  expect_s3_class(ascertainment_estimate, "data.frame")
  expect_named(
    ascertainment_estimate,
    c("ascertainment_me", "ascertainment_lo", "ascertainment_hi")
  )
  expect_true(
    all(
      apply(ascertainment_estimate, 2, function(x) x >= 0.0 && x <= 1.0)
    )
  )
  expect_true(
    all(
      ascertainment_estimate$ascertainment_lo <=
        ascertainment_estimate$ascertainment_me &&
        ascertainment_estimate$ascertainment_me <=
          ascertainment_estimate$ascertainment_hi
    )
  )
  # snapshot test
  expect_snapshot(
    ascertainment_estimate
  )
})

test_that("`estimate_ascertainment`: Correct for delays for static method", {
  ascertainment_estimate <- estimate_ascertainment(
    data = ebola1976,
    correct_for_delays = TRUE, epidist = onset_to_death_ebola,
    smooth_inputs = FALSE,
    burn_in_value = 1, smoothing_window = 1,
    type = "static"
  )

  expect_s3_class(ascertainment_estimate, "data.frame")
  expect_named(
    ascertainment_estimate,
    c("ascertainment_me", "ascertainment_lo", "ascertainment_hi")
  )
  expect_true(
    all(
      apply(ascertainment_estimate, 2, function(x) x >= 0.0 && x <= 1.0)
    )
  )
  expect_true(
    all(
      ascertainment_estimate$ascertainment_lo <=
        ascertainment_estimate$ascertainment_me &&
        ascertainment_estimate$ascertainment_me <=
          ascertainment_estimate$ascertainment_hi
    )
  )
  # snapshot test
  expect_snapshot(
    ascertainment_estimate
  )
})

test_that("`estimate_ascertainment`: Smooth inputs for static method", {
  ascertainment_estimate <- estimate_ascertainment(
    data = ebola1976,
    correct_for_delays = FALSE, epidist = onset_to_death_ebola,
    smooth_inputs = TRUE,
    burn_in_value = 1, smoothing_window = 7,
    type = "static"
  )

  expect_s3_class(ascertainment_estimate, "data.frame")
  expect_named(
    ascertainment_estimate,
    c("ascertainment_me", "ascertainment_lo", "ascertainment_hi")
  )
  expect_true(
    all(
      apply(ascertainment_estimate, 2, function(x) x >= 0.0 && x <= 1.0)
    )
  )
  expect_true(
    all(
      ascertainment_estimate$ascertainment_lo <=
        ascertainment_estimate$ascertainment_me &&
        ascertainment_estimate$ascertainment_me <=
          ascertainment_estimate$ascertainment_hi
    )
  )
  # snapshot test
  expect_snapshot(
    ascertainment_estimate
  )
})

test_that("`estimate_ascertainment`: Automatic burn-in value", {
  ascertainment_estimate <- estimate_ascertainment(
    data = ebola1976,
    correct_for_delays = FALSE, epidist = onset_to_death_ebola,
    smooth_inputs = TRUE,
    smoothing_window = 7,
    type = "static"
  )

  expect_s3_class(ascertainment_estimate, "data.frame")
  expect_named(
    ascertainment_estimate,
    c("ascertainment_me", "ascertainment_lo", "ascertainment_hi")
  )
  expect_true(
    all(
      apply(ascertainment_estimate, 2, function(x) x >= 0.0 && x <= 1.0)
    )
  )
  expect_true(
    all(
      ascertainment_estimate$ascertainment_lo <=
        ascertainment_estimate$ascertainment_me &&
        ascertainment_estimate$ascertainment_me <=
          ascertainment_estimate$ascertainment_hi
    )
  )
  # snapshot test
  expect_snapshot(
    ascertainment_estimate
  )
})

test_that("`estimate_ascertainment`: Automatic burn-in value for static", {
  ascertainment_estimate <- estimate_ascertainment(
    data = ebola1976,
    correct_for_delays = FALSE, epidist = onset_to_death_ebola,
    smooth_inputs = TRUE,
    smoothing_window = 7,
    type = "static"
  )

  expect_s3_class(ascertainment_estimate, "data.frame")
  expect_named(
    ascertainment_estimate,
    c("ascertainment_me", "ascertainment_lo", "ascertainment_hi")
  )
  expect_true(
    all(
      apply(ascertainment_estimate, 2, function(x) x >= 0.0 && x <= 1.0)
    )
  )
  expect_true(
    all(
      ascertainment_estimate$ascertainment_lo <=
        ascertainment_estimate$ascertainment_me &&
        ascertainment_estimate$ascertainment_me <=
          ascertainment_estimate$ascertainment_hi
    )
  )
  # snapshot test
  expect_snapshot(
    ascertainment_estimate
  )
})

test_that("`estimate_ascertainment`: Basic expectations for time-varying", {
  ascertainment_estimate <- estimate_ascertainment(
    data = ebola1976,
    correct_for_delays = TRUE,
    epidist = onset_to_death_ebola,
    smooth_inputs = FALSE,
    type = "varying"
  )

  expect_s3_class(ascertainment_estimate, "data.frame")
  expect_named(
    ascertainment_estimate,
    c("ascertainment_me", "ascertainment_lo", "ascertainment_hi")
  )
  expect_true(
    all(
      apply(ascertainment_estimate, 2, function(x) x >= 0.0 && x <= 1.0)
    )
  )
  expect_true(
    all(
      ascertainment_estimate$ascertainment_lo <=
        ascertainment_estimate$ascertainment_me &&
        ascertainment_estimate$ascertainment_me <=
          ascertainment_estimate$ascertainment_hi
    )
  )
  # snapshot test
  expect_snapshot(
    ascertainment_estimate
  )
})
