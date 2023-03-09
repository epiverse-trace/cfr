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

df_known_outcomes_raw <- known_outcomes(
  df_in = ebola1976, epi_dist = onset_to_death_ebola,
  cumulative = FALSE
)
df_known_outcomes_cumulative <- known_outcomes(
  df_in = ebola1976, epi_dist = onset_to_death_ebola
)

test_that("`known_outcomes` basic functionality", {
  expect_s3_class(
    df_known_outcomes_raw,
    "data.frame"
  )
  expect_identical(
    colnames(df_known_outcomes_raw),
    c("date", "cases", "deaths", "known_outcomes")
  )

  # expect that cumulative sum column is always increasing for cumulative option
  expect_true(
    all(diff(df_known_outcomes_cumulative$known_outcomes) > 0)
  )
  # expect that known_outcome column is not always increasing
  expect_false(
    all(diff(df_known_outcomes_raw$known_outcomes) > 0)
  )
  expect_snapshot(
    head(df_known_outcomes_raw)
  )
  expect_snapshot(
    head(df_known_outcomes_raw)
  )
})
