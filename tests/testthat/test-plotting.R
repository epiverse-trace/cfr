#### Check that plotting function works ####

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

# load Ebola data
data("ebola1976")

# Calculate rolling naive and corrected CFRs
df_ncfr <- rolling_cfr(ebola1976, correct_for_delays = FALSE)
df_ccfr <- rolling_cfr(ebola1976,
  correct_for_delays = TRUE,
  onset_to_death_ebola
)

# Plotting rolling estimates
this_plot <- plot_data_and_cfr(df_ncfr, df_ccfr)

test_that("`plot_data` function basic functionality", {
  expect_s3_class(
    this_plot, c("ggplot", "gg", "patchwork")
  )
  expect_snapshot(
    this_plot$data
  )
  expect_snapshot(
    this_plot$layers
  )
})
