#### Tests for the static CFR function ####
# prepare data and common testing elements

# read epidist for EVD onset to death from {epiparameter}
onset_to_death_ebola <- epiparameter::epidist_db(
  disease = "Ebola Virus Disease",
  epi_dist = "onset_to_death",
  author = "Barry_etal"
)

# Load ebola 1976 outbreak data
data("ebola1976")

# Calculate static naive CFR
scfr_naive <- estimate_static(df_in = ebola1976, correct_for_delays = FALSE)

# Calculate static corrected CFRs
scfr_corrected <- estimate_static(
  df_in = ebola1976,
  correct_for_delays = TRUE,
  epi_dist = onset_to_death_ebola
)

# Basic expectations
test_that("Basic expectations of static_cfr", {
  # expect named vectors
  expect_named(scfr_naive, c("severity_me", "severity_lo", "severity_hi"))
  expect_named(
    scfr_corrected, c("severity_me", "severity_lo", "severity_hi")
  )

  # expect named doubles
  expect_s3_class(scfr_naive, "data.frame")
  expect_s3_class(scfr_corrected, "data.frame")

  expect_snapshot(scfr_naive)
  expect_snapshot(scfr_corrected)

  # expect error when corrected CFR requested without delay PMF
  expect_error(
    estimate_static(
      df_in = ebola1976,
      correct_for_delays = TRUE
    ),
    regexp = paste0(
      "(correct)*(delay case detection and death)*(provide)*",
      "(onset-to-death)*(`epidist`)"
    )
  )

  # expect error when columns are missing
  expect_error(
    estimate_static(
      df_in = ebola1976[, c("date", "cases")],
      correct_for_delays = FALSE
    ),
    regexp = "Case data must contain columns `cases` and `deaths`"
  )
})
