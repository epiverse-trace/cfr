# tests for get_default_burn_in()

# create an epidist
onset_to_death_ebola <- epiparameter::epidist_db(
  disease = "Ebola Virus Disease",
  epi_dist = "onset_to_death",
  author = "Barry_etal",
  single_epidist = TRUE
)

test_that("Default burn in for time-varying severity", {
  burn_in <- get_default_burn_in(onset_to_death_ebola)

  expect_type(burn_in, "integer")
  expect_identical(
    burn_in,
    as.integer(round(mean(onset_to_death_ebola)))
  )
})

test_that("Default burn in for unparameterised epidist", {
  default_value <- 7L
  dummy_epidist <- epiparameter::epidist(
    disease = "dummy", epi_dist = "onset_to_outcome"
  )
  burn_in <- get_default_burn_in(dummy_epidist)

  expect_type(burn_in, "integer")
  expect_identical(burn_in, default_value)
})
