
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
#' \pkg{incidence2} package. See [incidence2::incidence()].
#' This function does not currently support grouped `<incidence2>` data.
#'
#' @param data A `<data.frame>`-like object. Currently, only `<incidence2>`
#' objects are supported.
#' @param cases_variable A string for the name of the cases variable in the
#' "count_variable" column of `data`.
#' @param deaths_variable A string for the name of the deaths variable in the
#' "count_variable" column of `data`.
#' @param fill_NA A logical indicating whether `NA`s in the cases and deaths
#' data should be replaced by 0s. The default value is `FALSE`. The function
#' will error if `fill_NA = FALSE` but `NA`s are detected in the case or death
#' data.
#' @param ... Currently unused. Passing extra arguments will throw a warning.
#'
#' @details
#' The method for `<incidence2>` data can replace `NA`s in the case and death
#' data with 0s using the `fill_NA` argument, which is `FALSE` by default,
#' meaning that `NA`s are retained.
#' `NA`s could arise if the dataset has non-sequential dates, as the
#' function fills in missing dates between the range of dates in the input data.
#' This is because downstream functions require data with a continuous sequence
#' of dates.
#'
#' Keeping `NA`s will cause downstream issues when calling functions such as
#' [estimate_static()] on the data, as they cannot handle `NA`s.
#' Setting `fill_NA = TRUE` resolves this issue, but must be a conscious choice.
#'
#' @return A `<data.frame>` suitable for disease severity estimation functions
#' provided in \pkg{cfr}, with the columns "date", "cases", and "deaths".
#'
#' Note that groups in `<incidence2>` are not retained, and cases and deaths
#' are summed by date.
#'
#' The result has a continuous sequence of dates between the start and end date
#' of `data`; this is required if the data is to be passed to functions such as
#' [estimate_static()].
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
#'   deaths_variable = "deaths_new",
#'   fill_NA = TRUE
#' )
#'
#' tail(data)
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
                                    fill_NA = FALSE,
                                    ...) {
  # check for {incidence2} and error if not available
  if (!requireNamespace("incidence2", quietly = TRUE)) {
    stop(
      "Package 'incidence2' is required to prepare <incidence2> class data ",
      "but is not installed."
    )
  }

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
  group_variables <- incidence2::get_group_names(data)

  stopifnot(
    "`cases_variable` and `deaths_variable` should be in \
    `count_variable` column of <incidence2> object `data`" =
      all(
        c(cases_variable, deaths_variable) %in%
          unique(data[[count_var_col]])
      ),
    "Grouped `<incidence2>` objects are not supported. \
     Use `incidence2::regroup()` to ungroup the data." =
      length(group_variables) == 0
  )

  # get the unique dates - this is used to reconstruct data
  # in cases where there are dates with no cases or deaths
  # since these dates may simply be missing in the data (e.g. due to filtering)
  range_dates <- range(data[[dates_variable]])
  unique_dates <- data.frame(
    date = seq(range_dates[1], range_dates[2], by = 1)
  )

  # split the dataframe, replacing "count" with the cases or deaths variable
  data <- split(x = data, f = data[[count_var_col]])
  # work on the split dataframe in the order of cases and then deaths
  data <- Map(
    data[c(cases_variable, deaths_variable)], c("cases", "deaths"),
    f = function(df, colname) {
      # select the date and the count
      df <- df[, c(dates_variable, count_col)]

      # rename the count to cases or deaths
      colnames(df) <- c("date", colname)

      # merge with dataframe of dates, keeping all dates
      df <- merge(
        unique_dates, df,
        by = "date", all.x = TRUE
      )

      # fill NA if required
      if (fill_NA) {
        df[is.na(df)] <- 0
      } else {
        if (anyNA(df[[colname]])) {
          stop(
            sprintf(
              "`NA`s present in the '%s' count. Set `fill_NA = TRUE` to use 0s",
              colname
            )
          )
        }
      }
      # return df from this scope
      df
    }
  )
  # merge the two dataframes
  data <- Reduce(x = data, f = merge)

  # return data
  data
}
