
#' Prepare data for CFR estimation
#' @export
prepare_data <- function(data, ...) {
  UseMethod("prepare_data", data)
}

#' Prepare data from <incidence2> objects
#'
#' @export
prepare_data.incidence2 <- function(data, cases_variable = "cases",
                                    deaths_variable = "deaths") {
  # assert that cases and deaths variable are different
  checkmate::assert_string(cases_variable)
  checkmate::assert_string(deaths_variable)
  msg <- paste0(
    "`cases_variable` and `deaths_variable` should be in ",
    "`count_variable` column of <incidence2> object `data`"
  )
  stopifnot(
    msg =
      all(
        c(cases_variable, deaths_variable) %in%
          unique(data[[attr(data, "count_variable")]])
      )
  )

  # get columns
  count_var_col <- attr(data, "count_variable")
  dates <- unique(data[[attr(data, "date_index")]])

  # create dataframe and return
  data <- data[data[[count_var_col]] %in% c(cases_variable, deaths_variable), ]
  data <- data.frame(
    lapply(split(a, f = data[[count_var_col]]), `[[`, "count")
  )
  colnames(data) <- c("cases", "deaths")
  data$date <- dates

  # return data
  data[, c("date", "cases", "deaths")]
}
