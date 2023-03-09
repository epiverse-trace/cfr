#' Plot rolling case and death data with CFR estimates
#'
#' @description Plots the case and death time series used to calculate the CFR
#' estimates in one panel and the resulting rolling naive and corrected CFR
#' estimates in another panel.
#'
#' @param df_ncfr A data.frame containing the rolling naive CFR estimates, of
#' the form returned by rolling_cfr.R
#'
#' @param df_ccfr A data.frame containing the rolling corrected CFR estimates,
#' of the form returned by rolling_cfr.R
#'
#' @return A ggplot object of the case and death time series used to calculate
#' the CFR estimates in one panel (top panel) and the resulting naive and
#' corrected CFR estimates in another panel (bottom panel)
#' @export
#'
#' @examples
#' # read epidist for EVD onset to death from {epiparameter}
#' onset_to_death_ebola <- epiparameter::epidist_db(
#'   disease = "Ebola Virus Disease",
#'   epi_dist = "onset_to_death",
#'   author = "Barry_etal"
#' )
#'
#' # Calculate rolling naive and corrected CFRs
#' df_ncfr <- rolling_cfr(ebola1976, correct_for_delays = FALSE)
#' df_ccfr <- rolling_cfr(ebola1976,
#'   correct_for_delays = TRUE,
#'   onset_to_death_ebola
#' )
#'
#' # Plotting rolling estimates
#' plot_data_and_cfr(df_ncfr, df_ccfr)
#'
plot_data_and_cfr <- function(df_ncfr, df_ccfr) {

  # Some basic input checking
  stopifnot(
    "Naive rolling CFR estimates must have columns `cases` and `deaths`" =
      (all(c("cases", "deaths", "date") %in% colnames(df_ncfr))),
    "Corrected rolling CFR estimates must have columns `cases` and `deaths`" =
      (all(c("cases", "deaths", "date") %in% colnames(df_ccfr)))
  )

  # putting together the data for just the first plot, to be melted
  # into long format (for ggplot)
  df_data_plot <- df_ncfr[, c("date", "cases", "deaths")]
  data.table::setDT(df_data_plot)

  # melting using reshape package into longer format
  df_data_plot_long <- data.table::melt(df_data_plot, id = "date")

  # plotting the top panel of the raw data
  p_1 <- ggplot2::ggplot(
    data = df_data_plot_long,
    ggplot2::aes(
      x = .data$date, y = .data$value,
      colour = .data$variable,
      shape = .data$variable
    )
  ) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::scale_x_date(
      date_labels = "%b-%d\n%Y"
    ) +
    ggplot2::scale_shape_manual(
      values = c(
        cases = 16,
        deaths = 17
      ),
      labels = c(
        cases = "Cases",
        deaths = "Deaths"
      )
    ) +
    ggplot2::scale_colour_discrete(
      labels = c(
        cases = "Cases",
        deaths = "Deaths"
      )
    ) +
    ggplot2::labs(
      x = "Date",
      y = "Counts",
      title = "Case and death data"
    ) +
    ggplot2::guides(
      colour = ggplot2::guide_legend(title = NULL),
      shape = ggplot2::guide_legend(title = NULL)
    ) +
    ggplot2::theme(legend.position = "bottom")

  # putting together the naive CFR estimates for the second panel
  df_ncfr$type <- "ncfr"
  df_ccfr$type <- "ccfr"

  df_cfr_plot <- data.table::rbindlist(list(df_ncfr, df_ccfr))

  # plotting the second panel of CFR estimates
  p_2 <- ggplot2::ggplot(data = df_cfr_plot) +
    ggplot2::geom_ribbon(
      ggplot2::aes(
        x = .data$date,
        ymin = .data$cfr_low,
        ymax = .data$cfr_high,
        fill = .data$type
      ),
      alpha = 0.2
    ) +
    ggplot2::geom_line(
      ggplot2::aes(
        x = .data$date,
        y = .data$cfr_me,
        colour = .data$type
      ),
      linetype = "dashed"
    ) +
    ggplot2::scale_colour_brewer(
      palette = "Dark2",
      labels = c(
        ccfr = "Corrected CFR",
        ncfr = "Naive CFR"
      )
    ) +
    ggplot2::scale_fill_brewer(
      palette = "Dark2",
      labels = c(
        ccfr = "Corrected CFR",
        ncfr = "Naive CFR"
      )
    ) +
    ggplot2::scale_x_date(
      date_labels = "%b-%d\n%Y"
    ) +
    ggplot2::labs(
      x = "Date",
      y = "Case fatality rate",
      title = "Naive and corrected case\nfatality rates"
    ) +
    ggplot2::guides(
      colour = ggplot2::guide_legend(title = NULL),
      fill = ggplot2::guide_legend(title = NULL)
    ) +
    ggplot2::theme(legend.position = "bottom")

  # putting the two panels on top of each other using patchwork
  patchwork::wrap_plots(
    p_1, p_2,
    ncol = 2
  )
}
