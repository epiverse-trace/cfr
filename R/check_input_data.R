.check_input_data <- function(data) {
  coll <- checkmate::makeAssertCollection()

  checkmate::assert_data_frame(
    data,
    min.rows = 1, min.cols = 3,
    add = coll
  )
  # check that input `<data.frame>` has columns date, cases, and deaths
  checkmate::assert_names(
    colnames(data),
    must.include = c("date", "cases", "deaths"),
    add = coll
  )
  # check for any NAs among data
  checkmate::assert_data_frame(
    data[, c("date", "cases", "deaths")],
    types = c("Date", "integerish"),
    any.missing = FALSE,
    add = coll
  )
  # check that data$date is a date column
  checkmate::assert_date(
    data$date,
    any.missing = FALSE,
    all.missing = FALSE,
    add = coll
  )

  # Check count types
  checkmate::assert_integerish(data$cases, lower = 0, add = coll)
  checkmate::assert_integerish(data$deaths, lower = 0, add = coll)

  stopifnot(
    "Input data must have sequential dates with none missing or duplicated" =
      identical(unique(diff(data$date)), 1) # use numeric 1, not integer
    # this solution works when df$date is `Date`
    # this may need more thought for dates that are integers, POSIXct,
    # or other units; consider the units package
  )

  checkmate::reportAssertions(coll)
}
