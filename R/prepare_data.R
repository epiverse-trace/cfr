#' @title Check whether a package is installed
#' @description A small internal function to check whether a required package
#' is installed.
#' This function is primarily intended to be used in mocking when testing for
#' appropriate error messages from functions that rely on a package without
#' that package being imported in {cfr}.
#' The only current use case is [prepare_data.incidence2()].
#' @param x A package name as a string.
#' @return Quietly returns a logical for whether the package passed in `x` is
#' available locally.
#' @keywords internal
.is_pkg_installed <- function(x) {
  requireNamespace(x, quietly = TRUE)
}

#' @title Prepare common epidemiological data formats for CFR estimation
#'
#' @name prepare_data
#' @rdname prepare_data
#'
#' @description
#' This S3 generic has methods for classes commonly used for epidemiological
#' data.
#'
#' Currently, the only supported data format is `<incidence2>` from the
#' \pkg{incidence2} package. See [incidence2::incidence()]. Grouped
#' `<incidence2>` data are supported, see **Details**.
#'
#' @param data A `<data.frame>`-like object. Currently, only `<incidence2>`
#' objects are supported. These may be grouped.
#' @param cases_variable A string for the name of the cases variable in the
#' "count_variable" column of `data`.
#' @param deaths_variable A string for the name of the deaths variable in the
#' "count_variable" column of `data`.
#' @param fill_NA A logical indicating whether `NA`s in the cases and deaths
#' data should be replaced by 0s. The default value is `TRUE`, with a message
#' to make users aware of the replacement.
#' @param ... Currently unused. Passing extra arguments will throw a warning.
#'
#' @details
#' The method for `<incidence2>` data can replace `NA`s in the case and death
#' data with 0s using the `fill_NA` argument, which is `TRUE` by default,
#' meaning that `NA`s are replaced.
#'
#' Keeping `NA`s will cause downstream issues when calling functions such as
#' [cfr_static()] on the data, as they cannot handle `NA`s.
#' Setting `fill_NA = TRUE` resolves this issue.
#'
#' Passing a grouped `<incidence2>` object to `data` will result in the function
#' respecting the grouping and returning grouping variables in separate columns.
#'
#' @return A `<data.frame>` suitable for disease severity estimation functions
#' provided in \pkg{cfr}, with the columns "date", "cases", and "deaths".
#'
#' Additionally, for grouped `<incidence2>` data, columns representing the
#' grouping variables will also be present.
#'
#' The result has a continuous sequence of dates between the start and end date
#' of `data`; this is required if the data is to be passed to functions such as
#' [cfr_static()].
#' @export
#' @examples
#' #### For <incidence2> data ####
#' # load Covid-19 data from incidence2
#' covid_uk <- incidence2::covidregionaldataUK
#'
#' # convert to incidence2 object
#' covid_uk_incidence <- incidence2::incidence(
#'   covid_uk,
#'   date_index = "date",
#'   counts = c("cases_new", "deaths_new"),
#'   count_names_to = "count_variable"
#' )
#'
#' # View tail of prepared data
#' data <- prepare_data(
#'   covid_uk_incidence,
#'   cases_variable = "cases_new",
#'   deaths_variable = "deaths_new"
#' )
#'
#' tail(data)
#'
#' #### For grouped <incidence2> data ####
#' # convert data to incidence2 object grouped by region
#' covid_uk_incidence <- incidence2::incidence(
#'   covid_uk,
#'   date_index = "date",
#'   counts = c("cases_new", "deaths_new"),
#'   count_names_to = "count_variable",
#'   groups = "region"
#' )
#'
#' # View tail of prepared data
#' data <- prepare_data(
#'   covid_uk_incidence,
#'   cases_variable = "cases_new",
#'   deaths_variable = "deaths_new"
#' )
#'
#' tail(data)
#'
prepare_data <- function(data, ...) {
  UseMethod("prepare_data", data)
}

#' Prepare `<incidence2>` objects for severity estimation
#'
#' @name prepare_data
#' @rdname prepare_data
#'
#' @export
prepare_data.incidence2 <- function(data, cases_variable = "cases",
                                    deaths_variable = "deaths",
                                    fill_NA = TRUE,
                                    ...) {
  # Check whether incidence2 is installed using internal function
  stopifnot(
    "Install package <incidence2> to prepare <incidence2> data" =
      .is_pkg_installed("incidence2")
  )
  # assume that installing incidence2 will also install data.table

  # assert that cases and deaths variable are different
  checkmate::assert_string(cases_variable)
  checkmate::assert_string(deaths_variable)
  checkmate::assert_logical(fill_NA, len = 1L, any.missing = FALSE)

  # check dots for extra arguments
  chkDots(...)

  # get column names from incidence2 class members
  count_var_col <- incidence2::get_count_variable_name(data)
  count_col <- incidence2::get_count_value_name(data)
  dates_variable <- incidence2::get_date_index_name(data)

  stopifnot(
    "`cases_variable` and `deaths_variable` should be in
    `count_variable` column of <incidence2> object `data`" =
      all(
        c(cases_variable, deaths_variable) %in%
          unique(data[[count_var_col]])
      )
  )

  # complete dates for all groups in the data and fill any NAs per user input
  data <- incidence2::complete_dates(
    data,
    fill = ifelse(fill_NA, 0, NA_integer_)
  )
  if (fill_NA) {
    message(
      "NAs in cases and deaths are being replaced with 0s: ",
      "Set `fill_NA = FALSE` to prevent this."
    )
  }
  data.table::setDT(data)

  index <- .subset2(data, count_var_col)
  index <- index == cases_variable | index == deaths_variable
  data <- data[index, ]

  formula <- stats::reformulate(count_var_col, "...")
  # cast wide and fill any NAs per user input
  data <- data.table::dcast(
    data, formula,
    value.var = count_col
  )

  # set cases and deaths column names
  data.table::setnames(
    data,
    old = c(dates_variable, cases_variable, deaths_variable),
    new = c("date", "cases", "deaths")
  )

  # return data.frame
  data.table::setDF(data)[]
}
