# Tests for the prepare_data() generic and incidence2 method

# convert to incidence2 object
covid_uk_incidence <- incidence2::incidence(
  incidence2::covidregionaldataUK,
  date_index = "date",
  counts = c("cases_new", "deaths_new"),
  count_names_to = "count_variable"
)

# View head of prepared data
data <- prepare_data(
  covid_uk_incidence,
  cases_variable = "cases_new",
  deaths_variable = "deaths_new",
  fill_NA = TRUE
)

test_that("`prepare_data()`: Basic expectations for incidence2 method", {
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

  expect_error(
    prepare_data(
      data = incidence2::incidence(
        incidence2::covidregionaldataUK,
        date_index = "date",
        counts = c("cases_new", "deaths_new"),
        count_names_to = "count_variable",
        groups = "region"
      ),
      cases_variable = "cases_new",
      deaths_variable = "deaths_new",
      fill_NA = TRUE
    ),
    regexp = "(`data` has groups defined)*(does not currently support grouped)"
  )
})

test_that("`prepare_data(): Error for data.frame method", {
  expect_error(
    prepare_data(
      as.data.frame(incidence2::covidregionaldataUK),
      cases_variable = "cases_new",
      deaths_variable = "deaths_new"
    ),
    regexp = '(no applicable method)*(\\"data.frame\\")'
  )
})
