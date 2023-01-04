#### Check that formatting CFR is correct ####
# prepare some data
data("ebola1976")

# Calculate static naive CFR
ncfr <- static_cfr(df_in = ebola1976, correct_for_delays = FALSE)

# Calculate corrected CFR
ccfr <- estimate_ccfr(
  total_cases = sum(ebola1976$cases),
  total_deaths = sum(ebola1976$deaths),
  u_t = 0.8, # modelled value
  poisson_threshold = 100
)

# Check type and snapshot
test_that("`format_cfr_neatly` basic functionality", {
  expect_type(format_cfr_neatly(ncfr), "character")
  expect_type(format_cfr_neatly(ccfr), "character")

  expect_snapshot(format_cfr_neatly(ncfr))
  expect_snapshot(format_cfr_neatly(ccfr))
})

onset_to_death_ebola <- epiparameter::epidist("ebola", "onset_to_death")$pmf
rolling_cfr_ebola <- rolling_cfr(
  df_in = ebola1976,
  correct_for_delays = TRUE,
  onset_to_death_ebola
)

# Check failure when a dataframe is passed
test_that("`format_cfr_neatly` does not work with data.frames", {
  expect_error(
    format_cfr_neatly(rolling_cfr_ebola),
    regexp = paste0(
      "(`cfr_in`)*(static estimates)*(rolling CFR estimate)"
    )
  )
})
