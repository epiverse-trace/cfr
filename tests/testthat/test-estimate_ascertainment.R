# Tests for estimate_ascertainment()

# load Ebola 1976 outbreak data
data("ebola1976")

# define poisson threshold
poisson_threshold <- 100

test_that("Basic expectations for static ascertainment", {
  ascertainment_estimate <- estimate_ascertainment(
    data = ebola1976,
    burn_in = 0,
    severity_baseline = 0.7,
    type = "static"
  )

  expect_s3_class(ascertainment_estimate, "data.frame")
  expect_named(
    ascertainment_estimate,
    c("ascertainment_mean", "ascertainment_low", "ascertainment_high")
  )
  expect_true(
    all(
      apply(ascertainment_estimate, 2, function(x) x >= 0.0 && x <= 1.0)
    )
  )
  expect_true(
    all(
      ascertainment_estimate$ascertainment_low <=
        ascertainment_estimate$ascertainment_mean &&
        ascertainment_estimate$ascertainment_mean <=
          ascertainment_estimate$ascertainment_high
    )
  )
  # snapshot test
  expect_snapshot(
    ascertainment_estimate
  )
})

test_that("Correct for delays for static ascertainment", {
  ascertainment_estimate <- estimate_ascertainment(
    data = ebola1976,
    delay_density = function(x) dgamma(x, shape = 2.40, scale = 3.33),
    burn_in = 0,
    severity_baseline = 0.7,
    type = "static"
  )

  expect_s3_class(ascertainment_estimate, "data.frame")
  expect_named(
    ascertainment_estimate,
    c("ascertainment_mean", "ascertainment_low", "ascertainment_high")
  )
  expect_true(
    all(
      apply(ascertainment_estimate, 2, function(x) x >= 0.0 && x <= 1.0)
    )
  )
  expect_true(
    all(
      ascertainment_estimate$ascertainment_low <=
        ascertainment_estimate$ascertainment_mean &&
        ascertainment_estimate$ascertainment_mean <=
          ascertainment_estimate$ascertainment_high
    )
  )
  # snapshot test
  expect_snapshot(
    ascertainment_estimate
  )
})

test_that("Smooth inputs for static ascertainment", {
  ascertainment_estimate <- estimate_ascertainment(
    data = ebola1976,
    delay_density = function(x) dgamma(x, shape = 2.40, scale = 3.33),
    burn_in = 0, smoothing_window = 7,
    severity_baseline = 0.7,
    type = "static"
  )

  expect_s3_class(ascertainment_estimate, "data.frame")
  expect_named(
    ascertainment_estimate,
    c("ascertainment_mean", "ascertainment_low", "ascertainment_high")
  )
  expect_true(
    all(
      apply(ascertainment_estimate, 2, function(x) x >= 0.0 && x <= 1.0)
    )
  )
  expect_true(
    all(
      ascertainment_estimate$ascertainment_low <=
        ascertainment_estimate$ascertainment_mean &&
        ascertainment_estimate$ascertainment_mean <=
          ascertainment_estimate$ascertainment_high
    )
  )
  # snapshot test
  expect_snapshot(
    ascertainment_estimate
  )
})

test_that("Automatic burn-in for static ascertainment with delay correction", {
  ascertainment_estimate <- estimate_ascertainment(
    data = ebola1976,
    delay_density = function(x) dgamma(x, shape = 2.40, scale = 3.33),
    smoothing_window = 7,
    severity_baseline = 0.7,
    type = "static"
  )

  expect_s3_class(ascertainment_estimate, "data.frame")
  expect_named(
    ascertainment_estimate,
    c("ascertainment_mean", "ascertainment_low", "ascertainment_high")
  )
  expect_true(
    all(
      apply(ascertainment_estimate, 2, function(x) x >= 0.0 && x <= 1.0)
    )
  )
  expect_true(
    all(
      ascertainment_estimate$ascertainment_low <=
        ascertainment_estimate$ascertainment_mean &&
        ascertainment_estimate$ascertainment_mean <=
          ascertainment_estimate$ascertainment_high
    )
  )
  # snapshot test
  expect_snapshot(
    ascertainment_estimate
  )
})

test_that("Automatic burn-in for static ascertainment, no delay correction", {
  ascertainment_estimate <- estimate_ascertainment(
    data = ebola1976,
    smoothing_window = 7,
    severity_baseline = 0.7,
    type = "static"
  )

  expect_s3_class(ascertainment_estimate, "data.frame")
  expect_named(
    ascertainment_estimate,
    c("ascertainment_mean", "ascertainment_low", "ascertainment_high")
  )
  expect_true(
    all(
      apply(ascertainment_estimate, 2, function(x) x >= 0.0 && x <= 1.0)
    )
  )
  expect_true(
    all(
      ascertainment_estimate$ascertainment_low <=
        ascertainment_estimate$ascertainment_mean &&
        ascertainment_estimate$ascertainment_mean <=
          ascertainment_estimate$ascertainment_high
    )
  )
  # snapshot test
  expect_snapshot(
    ascertainment_estimate
  )
})

#### Time varying ascertainment ####

# load covid data
data("covid_data")
# subset data
covid_uk <- covid_data[covid_data$country == "United Kingdom" &
  covid_data$date < "2021-01-01", ]

test_that("Basic expectations for time-varying ascertainment", {
  # provide burn in as mean of lognormal distribution
  distr_lnorm <- distributional::dist_lognormal(mu = 2.577, sigma = 0.440)
  burn_in <- round(mean(distr_lnorm))

  ascertainment_estimate <- estimate_ascertainment(
    data = covid_uk,
    delay_density = function(x) unlist(density(distr_lnorm, x)),
    burn_in = burn_in,
    severity_baseline = 0.02,
    type = "varying"
  )

  expect_s3_class(ascertainment_estimate, "data.frame")
  expect_identical(
    nrow(ascertainment_estimate),
    1L
  )

  expect_named(
    ascertainment_estimate,
    c("ascertainment_mean", "ascertainment_low", "ascertainment_high")
  )
  expect_true(
    all(
      apply(ascertainment_estimate, 2, function(x) x >= 0.0 && x <= 1.0)
    )
  )
  expect_true(
    all(
      ascertainment_estimate$ascertainment_low <=
        ascertainment_estimate$ascertainment_mean &&
        ascertainment_estimate$ascertainment_mean <=
          ascertainment_estimate$ascertainment_high
    )
  )
  # snapshot test
  expect_snapshot(
    ascertainment_estimate
  )
})

# test for a warning from ascertainment ratios > 1.0
test_that("Ascertainment > 1.0 throws a warning", {
  expect_warning(
    estimate_ascertainment(
      data = ebola1976,
      delay_density = function(x) dgamma(x, shape = 2.40, scale = 3.33),
      severity_baseline = 0.7,
      type = "varying"
    ),
    regexp = "Ascertainment ratios > 1.0 detected, setting these values to 1.0"
  )
})
