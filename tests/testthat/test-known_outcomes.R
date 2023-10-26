#### Check that known_outcomes works ####
# prepare example data
# Load Ebola 1976 outbreak data
data("ebola1976")

# Ebola onset to death distribution comes from Barry et al. 2018
# a gamma distribution with shape = 2.40, scale = 3.33

df_known_outcomes <- known_outcomes(
  data = ebola1976,
  delay_density = function(x) dgamma(x, shape = 2.40, scale = 3.33)
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
  # NA values are provisionally not allowed
  expect_true(
    all(
      df_known_outcomes$u_t >= 0.0 & df_known_outcomes$u_t <= 1.0 &
        !is.na(df_known_outcomes$u_t)
    )
  )

  # expect that column known_outcomes is >= 0
  # NA values are provisionally not allowed
  expect_true(
    all(
      df_known_outcomes$known_outcomes >= 0 &
        !is.na(df_known_outcomes$known_outcomes)
    )
  )

  # expect that known_outcome column is not always increasing
  expect_false(
    all(diff(df_known_outcomes$known_outcomes) > 0)
  )
  expect_snapshot(
    head(df_known_outcomes)
  )
})
