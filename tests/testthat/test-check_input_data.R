test_that("early input validation error doesn't cause uncontrolled failure", {
  
  bad_names_df <- dplyr::rename(ebola1976, dates=date)
  
  cfr_static(bad_names_df)
})
