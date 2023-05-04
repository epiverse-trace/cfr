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
  expect_type(format_cfr_neatly(ncfr, type = "Naive"), "list")
  expect_type(format_cfr_neatly(ccfr, type = "Corrected"), "list")

  expect_snapshot(format_cfr_neatly(ncfr, type = "Naive"))
  expect_snapshot(format_cfr_neatly(ccfr, type = "Corrected"))
})