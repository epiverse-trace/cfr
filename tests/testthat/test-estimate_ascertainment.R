# Tests for estimate_ascertainment()

# load Ebola 1976 outbreak data
data("ebola1976")

# read epidist for EVD onset to death from {epiparameter}
onset_to_death_ebola <- epiparameter::epidist_db(
  disease = "Ebola Virus Disease",
  epi_dist = "onset_to_death",
  author = "Barry_etal",
  single_epidist = TRUE
)

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
    epidist = onset_to_death_ebola,
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
    epidist = onset_to_death_ebola,
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
    epidist = onset_to_death_ebola,
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

# read epidist for EVD onset to death from {epiparameter}
onset_to_death_covid <- epiparameter::epidist_db(
  disease = "Covid-19",
  epi_dist = "onset_to_death",
  author = "Linton_etal",
  single_epidist = TRUE
)

test_that("Basic expectations for time-varying ascertainment", {
  ascertainment_estimate <- estimate_ascertainment(
    data = covid_uk,
    epidist = onset_to_death_covid,
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

# test that the time varying estimate at a specific date X is the same as when
# the data are subset up to the date X
test_that("Time varying ascertainment at a user-specified date", {
  # prepare the date
  max_date <- as.Date("2020-06-01")
  # get the estimate using max_date
  ascertainment_estimate <- estimate_ascertainment(
    data = covid_uk,
    epidist = onset_to_death_covid,
    max_date = max_date,
    severity_baseline = 0.02,
    type = "varying"
  )

  # get the estimate after subsetting the data
  ascertainment_estimate_2 <- estimate_ascertainment(
    data = covid_uk[covid_uk$date <= max_date, ],
    epidist = onset_to_death_covid,
    severity_baseline = 0.02,
    type = "varying"
  )

  expect_identical(
    ascertainment_estimate,
    ascertainment_estimate_2
  )
})

# test for a warning from ascertainment ratios > 1.0
test_that("Ascertainment > 1.0 throws a warning", {
  # get the estimate using max_date
  expect_warning(
    estimate_ascertainment(
      data = ebola1976,
      epidist = onset_to_death_ebola,
      severity_baseline = 0.7,
      type = "varying"
    ),
    regexp = "Ascertainment ratios > 1.0 detected, setting these values to 1.0"
  )
})
