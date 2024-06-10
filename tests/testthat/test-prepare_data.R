# Tests for the prepare_data() generic and incidence2 method
test_that("Prepare `<incidence2>` data, basic expectations", {
  # convert to incidence2 object
  # suppress warnings for NAs from incidence2 v2.3.0 as this is what is
  # tested below
  suppressWarnings(
    covid_uk_incidence <- incidence2::incidence(
      incidence2::covidregionaldataUK,
      date_index = "date",
      counts = c("cases_new", "deaths_new"),
      count_names_to = "count_variable"
    )
  )

  expect_message(
    prepare_data(
      covid_uk_incidence,
      cases_variable = "cases_new", deaths_variable = "deaths_new",
      fill_NA = TRUE
    ),
    regexp = paste0(
      "NAs in cases and deaths are being replaced with 0s: ",
      "Set `fill_NA = FALSE` to prevent this."
    )
  )
  expect_no_condition(
    prepare_data(
      covid_uk_incidence,
      cases_variable = "cases_new", deaths_variable = "deaths_new",
      fill_NA = FALSE
    )
  )

  # snapshot with NAs for fill_NAs = FALSE
  expect_snapshot(
    head(
      prepare_data(
        covid_uk_incidence,
        cases_variable = "cases_new", deaths_variable = "deaths_new",
        fill_NA = FALSE
      )
    )
  )

  # get data for output expectations
  data <- prepare_data(
    covid_uk_incidence,
    cases_variable = "cases_new",
    deaths_variable = "deaths_new",
    fill_NA = TRUE
  )
  expect_s3_class(data, "data.frame")
  expect_named(
    data, c("date", "cases", "deaths")
  )
  expect_s3_class(data$date, "Date")
  expect_vector(data[["cases"]], ptype = numeric())
  expect_vector(data[["deaths"]], ptype = numeric())

  # snapshot of tail as head has zeros
  expect_snapshot(
    tail(data)
  )
})

test_that("Prepare <incidence2> fails if <incidence2> not available", {
  # load some data
  # suppress warnings for NAs from incidence2 v2.3.0 as this is not relevant
  # to the test
  suppressWarnings(
    covid_uk_incidence <- incidence2::incidence(
      incidence2::covidregionaldataUK,
      date_index = "date",
      counts = c("cases_new", "deaths_new"),
      count_names_to = "count_variable"
    )
  )

  # mock .is_pkg_installed() to return FALSE simulating incidence2 not installed
  # this test adapted from
  # https://community.rstudio.com/t/how-can-i-make-testthat-think-i-dont-have-a-package-installed/33441 # nolint line_length_linter
  with_mocked_bindings(
    .is_pkg_installed = function(x) FALSE,
    code = expect_error(
      prepare_data(
        covid_uk_incidence,
        cases_variable = "cases_new", deaths_variable = "deaths_new",
        fill_NA = TRUE
      ),
      regexp = "Install package \\{incidence2\\} to prepare <incidence2> data"
    )
  )
})

test_that("Data preparation errors for data.frame method", {
  expect_error(
    prepare_data(
      as.data.frame(incidence2::covidregionaldataUK),
      cases_variable = "cases_new",
      deaths_variable = "deaths_new"
    ),
    regexp = '(no applicable method)*(\\"data.frame\\")'
  )
})

test_that("Prepare grouped `<incidence2>` data", {
  grouping_variable <- "region"
  # suppress warnings for NAs from incidence2 v2.3.0 as filling is tested here
  suppressWarnings(
    data <- incidence2::incidence(
      incidence2::covidregionaldataUK,
      date_index = "date",
      counts = c("cases_new", "deaths_new"),
      count_names_to = "count_variable",
      groups = grouping_variable
    )
  )

  expect_no_condition(
    prepare_data(
      data,
      cases_variable = "cases_new", deaths_variable = "deaths_new",
      fill_NA = FALSE
    )
  )
  expect_snapshot(
    head(
      prepare_data(
        data,
        cases_variable = "cases_new", deaths_variable = "deaths_new",
        fill_NA = FALSE
      )
    )
  )
  expect_message(
    prepare_data(
      data,
      cases_variable = "cases_new", deaths_variable = "deaths_new",
      fill_NA = TRUE
    ),
    regexp = paste0(
      "NAs in cases and deaths are being replaced with 0s: ",
      "Set `fill_NA = FALSE` to prevent this."
    )
  )

  data <- prepare_data(
    data,
    cases_variable = "cases_new", deaths_variable = "deaths_new",
    fill_NA = TRUE
  )

  expect_s3_class(
    data, "data.frame"
  )
  expect_named(
    data,
    expected = c(
      "date", "cases", "deaths",
      grouping_variable
    ),
    ignore.order = TRUE
  )
})
