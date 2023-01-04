#### Tests for the static CFR function ####
# prepare data and common testing elements

# Get onset to death distribution for ebola from epiparameter
onset_to_death_ebola <- epiparameter::epidist("ebola", "onset_to_death")$pmf

# Load ebola 1976 outbreak data
data("ebola1976")

# Calculate static naive CFR
scfr_naive <- static_cfr(df_in = ebola1976, correct_for_delays = FALSE)

# Calculate static corrected CFRs
scfr_corrected <- static_cfr(
  df_in = ebola1976,
  correct_for_delays = TRUE,
  delay_pmf = onset_to_death_ebola
)

# Basic expectations
test_that("Basic expectations of static_cfr", {
  # expect named vectors
  expect_named(scfr_naive, c("cfr_me", "cfr_low", "cfr_high"))
  expect_named(scfr_corrected, c("cfr_me", "cfr_low", "cfr_high"))

  # expect named doubles
  expect_vector(scfr_naive, ptype = numeric())
  expect_vector(scfr_corrected, ptype = numeric())

  expect_snapshot(scfr_naive)
  expect_snapshot(scfr_corrected)

  # Formats the output of the CFR data.frames nicely
  # and prints to the terminal
  expect_snapshot(
    format_cfr_neatly(scfr_naive)
  )
  expect_snapshot(
    format_cfr_neatly(scfr_corrected)
  )

  # expect error when corrected CFR requested without delay PMF
  expect_error(
    static_cfr(
      df_in = ebola1976,
      correct_for_delays = TRUE
    ),
    regexp = paste0(
      "(correct)*(delay case detection and death)*(specify)*",
      "(probability mass function)"
    )
  )

  # expect error when columns are missing
  expect_error(
    static_cfr(
      df_in = ebola1976[, c("date", "cases")],
      correct_for_delays = FALSE
    ),
    regexp = "Case data must contain columns `cases` and `deaths`"
  )
})
