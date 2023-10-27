# Test for delay density passed to cfr_*()
test_that("CFR functions work with delay_density as lambda", {
  # Checks pass and cfr_static function works
  ddens <- function(x) stats::dgamma(x, 5, 1)
  expect_no_condition(
    cfr_static(ebola1976, delay_density = ddens)
  )
  expect_no_condition(
    cfr_rolling(ebola1976, delay_density = ddens)
  )
  expect_no_condition(
    cfr_time_varying(ebola1976, delay_density = ddens)
  )
  expect_no_condition(
    estimate_ascertainment(
      ebola1976,
      delay_density = ddens, severity_baseline = 0.7
    )
  )
})

test_that("CFR functions work with delay_density as <distcrete>", {
  # Checks pass and cfr_static function works with disctcrete
  ddens <- distcrete::distcrete("gamma", 1, shape = 2.4, scale = 3.33)$d
  expect_no_condition(
    cfr_static(ebola1976, delay_density = ddens)
  )
  expect_no_condition(
    cfr_rolling(ebola1976, delay_density = ddens)
  )
  expect_no_condition(
    cfr_time_varying(ebola1976, delay_density = ddens)
  )
  expect_no_condition(
    estimate_ascertainment(
      ebola1976,
      delay_density = ddens, severity_baseline = 0.7
    )
  )
})

# Test error cases
msg <- "(`delay_density` must be)*(function evaluating distribution density)"

test_that("Input checking on delay_density works", {
  # Checks fail on badly specified delay_density fns

  # Function returns NULL
  ddens <- function(x) NULL
  expect_error(
    cfr_static(ebola1976, delay_density = ddens),
    regexp = msg
  )
  expect_error(
    cfr_rolling(ebola1976, delay_density = ddens),
    regexp = msg
  )
  expect_error(
    cfr_time_varying(ebola1976, delay_density = ddens),
    regexp = msg
  )
  expect_error(
    estimate_ascertainment(
      ebola1976,
      delay_density = ddens, severity_baseline = 0.7
    ),
    regexp = msg
  )

  # Function returns non-numeric
  # NOTE the use of seq_along - there are no checks that the fn
  # is statistically correct or a valid density function
  ddens <- function(x) as.character(seq_along(x))
  expect_error(
    cfr_static(ebola1976, delay_density = ddens),
    regexp = msg
  )
  expect_error(
    cfr_rolling(ebola1976, delay_density = ddens),
    regexp = msg
  )
  expect_error(
    cfr_time_varying(ebola1976, delay_density = ddens),
    regexp = msg
  )
  expect_error(
    estimate_ascertainment(
      ebola1976,
      delay_density = ddens, severity_baseline = 0.7
    ),
    regexp = msg
  )

  # Function returns Inf
  ddens <- function(x) rep(Inf, times = length(x))
  expect_error(
    cfr_static(ebola1976, delay_density = ddens),
    regexp = msg
  )
  expect_error(
    cfr_rolling(ebola1976, delay_density = ddens),
    regexp = msg
  )
  expect_error(
    cfr_time_varying(ebola1976, delay_density = ddens),
    regexp = msg
  )
  expect_error(
    estimate_ascertainment(
      ebola1976,
      delay_density = ddens, severity_baseline = 0.7
    ),
    regexp = msg
  )

  # Function returns negative values
  # NOTE the use of seq_along - there are no checks that the fn
  # is statistically correct or a valid density function
  ddens <- function(x) -seq_along(x)
  expect_error(
    cfr_static(ebola1976, delay_density = ddens),
    regexp = msg
  )
  expect_error(
    cfr_rolling(ebola1976, delay_density = ddens),
    regexp = msg
  )
  expect_error(
    cfr_time_varying(ebola1976, delay_density = ddens),
    regexp = msg
  )
  expect_error(
    estimate_ascertainment(
      ebola1976,
      delay_density = ddens, severity_baseline = 0.7
    ),
    regexp = msg
  )

  # Function returns NAs
  # NOTE the use of seq_along - there are no checks that the fn
  # is statistically correct or a valid density function
  ddens <- function(x) {
    y <- seq_along(x)
    y[1] <- NA_real_
  }
  expect_error(
    cfr_static(ebola1976, delay_density = ddens),
    regexp = msg
  )
  expect_error(
    cfr_rolling(ebola1976, delay_density = ddens),
    regexp = msg
  )
  expect_error(
    cfr_time_varying(ebola1976, delay_density = ddens),
    regexp = msg
  )
  expect_error(
    estimate_ascertainment(
      ebola1976,
      delay_density = ddens, severity_baseline = 0.7
    ),
    regexp = msg
  )

  # Function returns wrong length
  # NOTE the use of seq_along - there are no checks that the fn
  # is statistically correct or a valid density function
  ddens <- function(x) {
    head(seq_along(x))
  }
  expect_error(
    cfr_static(ebola1976, delay_density = ddens),
    regexp = msg
  )
  expect_error(
    cfr_rolling(ebola1976, delay_density = ddens),
    regexp = msg
  )
  expect_error(
    cfr_time_varying(ebola1976, delay_density = ddens),
    regexp = msg
  )
  expect_error(
    estimate_ascertainment(
      ebola1976,
      delay_density = ddens, severity_baseline = 0.7
    ),
    regexp = msg
  )
})
