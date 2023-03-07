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

# create an epidist for EVD onset to death distribution
# taken from parameters in 10.1016/S0140-6736(18)31387-4
onset_to_death_ebola <- epiparameter::epidist(
  disease = "Ebola virus disease",
  pathogen = "Ebolavirus",
  epi_dist = "onset_to_death",
  prob_distribution = "gamma",
  prob_distribution_params = c(
    shape = 2.4, scale = 3.333
  )
)

rolling_cfr_ebola <- rolling_cfr(
  df_in = ebola1976,
  correct_for_delays = TRUE,
  epi_dist = onset_to_death_ebola
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
