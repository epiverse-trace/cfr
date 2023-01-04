#### Check that plotting function works ####
# prepare data
# read in onset to death distribution for Ebola
onset_to_death_ebola <- epiparameter::epidist("ebola", "onset_to_death")$pmf

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
