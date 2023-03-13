#### Tests for the rolling CFR function ####
# prepare data and common testing elements

# read epidist for EVD onset to death from {epiparameter}
onset_to_death_ebola <- epiparameter::epidist_db(
  disease = "Ebola Virus Disease",
  epi_dist = "onset_to_death",
  author = "Barry_etal"
)

# Load ebola 1976 outbreak data
data("ebola1976")

# Calculate rolling naive CFR
rcfr_naive <- rolling_cfr(df_in = ebola1976, correct_for_delays = FALSE)

# Calculate rolling corrected CFRs
rcfr_corrected <- rolling_cfr(
  df_in = ebola1976,
  correct_for_delays = TRUE,
  epi_dist = onset_to_death_ebola
)

# Basic expectations
test_that("Basic expectations of rolling_cfr", {
  # expect named vectors
  expect_identical(
    colnames(rcfr_naive),
    c("cfr_me", "cfr_low", "cfr_high", "date", "cases", "deaths")
  )
  expect_identical(
    colnames(rcfr_corrected),
    c("cfr_me", "cfr_low", "cfr_high", "date", "cases", "deaths")
  )

  # expect named doubles
  expect_s3_class(rcfr_naive, "data.frame")
  expect_s3_class(rcfr_corrected, "data.frame")

  expect_snapshot(tail(rcfr_naive)) # test only the final 5 rows
  expect_snapshot(tail(rcfr_corrected))

  # expect error when corrected CFR requested without delay PMF
  expect_error(
    rolling_cfr(
      df_in = ebola1976,
      correct_for_delays = TRUE
    ),
    regexp = paste0(
      "(correct)*(delay case detection and death)*(provide)*",
      "(`epidist`)"
    )
  )

  # expect same number of rows
  expect_identical(
    nrow(rcfr_corrected),
    nrow(ebola1976)
  )
  expect_identical(
    nrow(rcfr_naive),
    nrow(ebola1976)
  )

  # expect error when columns are missing
  expect_error(
    rolling_cfr(
      df_in = ebola1976[, c("date", "cases")],
      correct_for_delays = FALSE
    ),
    regexp = "Case data must contain columns `cases` and `deaths`"
  )
  expect_error(
    rolling_cfr(
      df_in = ebola1976[, c("date", "cases")],
      correct_for_delays = FALSE
    ),
    regexp = "Case data must contain columns `cases` and `deaths`"
  )
})
