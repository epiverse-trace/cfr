
#' Prepare data for CFR estimation
#'
#' @description
#' This S3 generic has no default method. Rather, it has methods for classes
#' commonly found in epidemiological data, such as `<incidence2>` from the
#' `{incidence2}` package. See [incidence2::incidence()].
#'
#' @param data A `data.frame`-like object.
#'
#' @param ... Other arguments passed to methods.
#'
#' @export
prepare_data <- function(data, ...) {
  UseMethod("prepare_data", data)
}

#' Prepare data from `<incidence2>` objects
#'
#' @description
#' Prepares `<incidence2>` objects for disease severity estimates. This function
#' does not currently support grouped `<incidence2>` data. Case and death counts
#' are aggregated by date for the overall dataset.
#'
#'
#' @param data An `<incidence2>` object.
#' @param cases_variable A string for the name of the cases variable in the
#' "count_variable" column of `data`.
#' @param deaths_variable A string for the name of the deaths variable in the
#' "count_variable" column of `data`.
#' @param fill_NA A logical indicating whether `NA`s in the cases and deaths
#' data should be replaced by 0-s. The default value is `TRUE`. Note that this
#' may not be the behaviour expected for some kinds of linelist data.
#' @param ... Other arguments.
#'
#' @export
#' @return A data.frame suitable for disease severity estimation functions
#' provided in `{datadelay}`, with the columns "date", "cases", and "deaths".
#' Note that groups in `<incidence2>` are not retained, and cases and deaths
#' are summed by date.
#' The result has a continuous sequence of dates between the start and end date
#' of `data`; this is required if the data is to be passed to functions such as
#' [estimate_static()].
#' @examples
#' # load Covid-19 data from incidence2
#' covid_uk <- incidence2::covidregionaldataUK
#'
#' # convert to incidence2 object
#' covid_uk_incidence <- incidence2::incidence(
#'   covid_uk,
#'   date_index = "date",
#'   counts = c("cases_new", "deaths_new"),
#'   count_names_to = "count_variable",
#'   groups = "region"
#' )
#'
#' # View head of prepared data
#' head(
#'   prepare_data(
#'     covid_uk_incidence,
#'     cases_variable = "cases_new",
#'     deaths_variable = "deaths_new"
#'   )
#' )
prepare_data.incidence2 <- function(data, cases_variable = "cases",
                                    deaths_variable = "deaths",
                                    fill_NA = TRUE,
                                    ...) {
  # assert that cases and deaths variable are different
  checkmate::assert_string(cases_variable)
  checkmate::assert_string(deaths_variable)
  checkmate::assert_logical(fill_NA, len = 1L, any.missing = FALSE)

  stopifnot(
    "`cases_variable` and `deaths_variable` should be in \
    `count_variable` column of <incidence2> object `data`" =
      all(
        c(cases_variable, deaths_variable) %in%
          unique(data[[attr(data, "count_variable")]])
      )
  )

  # get column names from incidence2 class members
  count_var_col <- attr(data, "count_variable")
  dates_variable <- attr(data, "date_index")
  group_variables <- attr(data, "groups")

  if (length(group_variables) > 0) {
    message(
      "`data` has groups defined - this function does not currently ",
      "support grouped `<incidence2>` objects. Cases and deaths will be ",
      "aggregated for the whole data."
    )
  }

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
      df <- df[, c(dates_variable, "count")]

      # rename the count to cases or deaths
      colnames(df) <- c("date", colname)

      # merge with dataframe of dates, keeping all dates
      df <- merge(
        unique_dates, df,
        by = "date", all.x = TRUE
      )
      # return df from this scope
      df
    }
  )
  # merge the two dataframes
  data <- Reduce(x = data, f = merge)

  # fill NA if required
  if (fill_NA) {
    data[is.na(data)] <- 0
  }

  # aggregate data to remove grouping structure if any
  data <- stats::aggregate(. ~ date, data = data, FUN = sum)

  # return data
  data
}
