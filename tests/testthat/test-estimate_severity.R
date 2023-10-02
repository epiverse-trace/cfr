# Tests for estimate_severity()
# Note that this is an internal function underlying cfr_static()
# when corrected_for_delays is TRUE

# load Ebola 1976 outbreak data
data("ebola1976")

# read epidist for EVD onset to death from {epiparameter}
onset_to_death_ebola <- epiparameter::epidist_db(
  disease = "Ebola Virus Disease",
  epi_dist = "onset_to_death",
  author = "The-Ebola-Outbreak-Epidemiology-Team",
  single_epidist = TRUE
)

poisson_threshold <- 100

# get the corrected dataframe
df_corrected <- known_outcomes(
  data = ebola1976,
  epidist = onset_to_death_ebola
)

# run estimate_severity
severity_estimate <- estimate_severity(
  total_cases = sum(df_corrected$cases),
  total_deaths = sum(df_corrected$deaths),
  total_outcomes = sum(df_corrected$known_outcomes),
  poisson_threshold = poisson_threshold
)

test_that("`cfr_rolling`: Basic expectations", {
  expect_s3_class(severity_estimate, "data.frame")
  expect_named(
    severity_estimate,
    sprintf("severity_%s", c("mean", "low", "high"))
  )
  # expect within values
  # TODO: account for potential NA values or vectors
  expect_true(
    all(
      apply(severity_estimate, 2, function(x) x >= 0.0 && x <= 1.0)
    )
  )
  # expect that lo, me, and hi are in roughly ascending order
  expect_true(
    all(
      severity_estimate$severity_low < severity_estimate$severity_mean &&
        severity_estimate$severity_mean < severity_estimate$severity_high
    )
  )
  # also check for a snapshot
  expect_snapshot(
    severity_estimate
  )

  # check estimate_severity with higher poisson threshold
  # forcing use of an alternative calculation
  severity_estimate_lt <- estimate_severity(
    total_cases = sum(df_corrected$cases),
    total_deaths = sum(df_corrected$deaths),
    total_outcomes = sum(df_corrected$known_outcomes),
    poisson_threshold = 1000
  )
  # snapshot of severity estimate using alternative method
  expect_snapshot(
    severity_estimate_lt
  )

  # expect that severity is lower when there are fewer cases
  ebola1976$deaths <- ebola1976$deaths - 2
  ebola1976$deaths[ebola1976$deaths < 0] <- 0

  # get the corrected dataframe
  df_corrected <- known_outcomes(
    data = ebola1976,
    epidist = onset_to_death_ebola
  )

  # run estimate_severity
  severity_low_deaths <- estimate_severity(
    total_cases = sum(df_corrected$cases),
    total_deaths = sum(df_corrected$deaths),
    total_outcomes = sum(df_corrected$known_outcomes),
    poisson_threshold = poisson_threshold
  )

  expect_true(
    all(severity_estimate > severity_low_deaths)
  )
})

test_that("`cfr_rolling`: Messages and errors", {
  ebola1976$cases <- 0L
  df_corrected <- known_outcomes(
    data = ebola1976,
    epidist = onset_to_death_ebola
  )

  # expect an error because cases are 0
  expect_error(
    estimate_severity(
      total_cases = sum(df_corrected$cases),
      total_deaths = sum(df_corrected$deaths),
      total_outcomes = sum(df_corrected$known_outcomes),
      poisson_threshold = poisson_threshold
    ),
    regexp = "Assertion on 'total_deaths' failed: Element 1 is not <= 0"
  )
})
