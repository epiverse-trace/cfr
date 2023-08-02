#### Check that known_outcomes works ####
# prepare example data
# Load Ebola 1976 outbreak data
data("ebola1976")

# read epidist for EVD onset to death from {epiparameter}
onset_to_death_ebola <- epiparameter::epidist_db(
  disease = "Ebola Virus Disease",
  epi_dist = "onset_to_death",
  author = "Barry_etal"
)

df_known_outcomes <- known_outcomes(
  data = ebola1976, epidist = onset_to_death_ebola
)

test_that("`known_outcomes` basic functionality", {
  expect_s3_class(
    df_known_outcomes,
    "data.frame"
  )
  expect_identical(
    colnames(df_known_outcomes),
    c(colnames(ebola1976), "known_outcomes", "u_t")
  )

  # expect that columns u_t is within the 0 -- 1 range
  # TODO: determine whether NA values are allowed
  expect_true(
    all(df_known_outcomes$u_t >= 0.0 & df_known_outcomes$u_t <= 1.0)
  )

  # expect that column known_outcomes is >= 0
  # TODO: determine whether NA values are allowed
  expect_true(
    all(df_known_outcomes$known_outcomes >= 0)
  )

  # expect that known_outcome column is not always increasing
  expect_false(
    all(diff(df_known_outcomes$known_outcomes) > 0)
  )
  expect_snapshot(
    head(df_known_outcomes)
  )
})
