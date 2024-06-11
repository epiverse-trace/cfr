# Tests for estimate_ascertainment()

# load Ebola 1976 outbreak data
data("ebola1976")

# define poisson threshold
poisson_threshold <- 100

test_that("Basic expectations for static ascertainment", {
  ascertainment_estimate <- estimate_ascertainment(
    data = ebola1976,
    severity_baseline = 0.7
  )

  expect_s3_class(ascertainment_estimate, "data.frame")
  expect_named(
    ascertainment_estimate,
    c("ascertainment_estimate", "ascertainment_low", "ascertainment_high")
  )
  expect_true(
    all(
      apply(ascertainment_estimate, 2, function(x) x >= 0.0 && x <= 1.0)
    )
  )
  expect_true(
    all(
      ascertainment_estimate$ascertainment_low <=
        ascertainment_estimate$ascertainment_estimate &&
        ascertainment_estimate$ascertainment_estimate <=
          ascertainment_estimate$ascertainment_high
    )
  )
  # snapshot test
  expect_snapshot(
    estimate_ascertainment(data = ebola1976, severity_baseline = 0.7)
  )
})

test_that("Correct for delays for static ascertainment", {
  ascertainment_estimate <- estimate_ascertainment(
    data = ebola1976,
    delay_density = function(x) dgamma(x, shape = 2.40, scale = 3.33),
    severity_baseline = 0.7
  )

  expect_s3_class(ascertainment_estimate, "data.frame")
  expect_named(
    ascertainment_estimate,
    c("ascertainment_estimate", "ascertainment_low", "ascertainment_high")
  )
  expect_true(
    all(
      apply(ascertainment_estimate, 2, function(x) x >= 0.0 && x <= 1.0)
    )
  )
  expect_true(
    all(
      ascertainment_estimate$ascertainment_low <=
        ascertainment_estimate$ascertainment_estimate &&
        ascertainment_estimate$ascertainment_estimate <=
          ascertainment_estimate$ascertainment_high
    )
  )
  # snapshot test
  expect_snapshot(
    estimate_ascertainment(
      data = ebola1976,
      delay_density = function(x) dgamma(x, shape = 2.40, scale = 3.33),
      severity_baseline = 0.7
    )
  )
})

# load covid data
data("covid_data")
# subset data
covid_uk <- covid_data[covid_data$country == "United Kingdom" &
  covid_data$date <= "2020-06-30", ]

test_that("Static ascertainment from vignette", {
  expect_snapshot(
    estimate_ascertainment(
      data = covid_uk,
      delay_density = function(x) dlnorm(x, meanlog = 2.577, sdlog = 0.440),
      severity_baseline = 0.014
    )
  )
})

# test for a warning from ascertainment ratios > 1.0
# artificially set baseline severity to be very high
# this is more an issue for infections with lower reporting such as Covid-19
test_that("Ascertainment > 1.0 throws a warning", {
  expect_warning(
    estimate_ascertainment(
      data = ebola1976,
      delay_density = function(x) dgamma(x, shape = 2.40, scale = 3.33),
      severity_baseline = 0.9
    ),
    regexp = "Ascertainment ratios > 1.0 detected, setting these values to 1.0"
  )
})

#### Test statistical correctness of ascertainment ####
test_that("Ascertainment is statistically correct", {
  # simple assumptions
  # assume 1% true CFR
  severity_baseline <- 0.01
  daily_cases <- 500
  daily_deaths <- 10

  data <- data.frame(
    date = as.Date("2020-01-01") + seq(0, 99),
    cases = rep(daily_cases, 100),
    deaths = rep(daily_deaths, 100)
  )

  # exepect estimate is 0.5
  expect_identical(
    estimate_ascertainment(
      data,
      severity_baseline = 0.01
    )$ascertainment_estimate,
    severity_baseline / (daily_deaths / daily_cases)
  )
})
