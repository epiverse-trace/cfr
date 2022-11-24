#' Plots the case and death time series used to calculate the CFR estimates
#' in one panel (top panel) and the resulting naive and corrected CFR estimates
#' in another panel (bottom panel)
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
#' @importFrom reshape2 melt
#' 
#' @importFrom ggplot2 ggplot aes geom_line geom_point geom_ribbon theme_minimal
#' theme
#' 
#' @importFrom patchwork plot_layout
#'
#' @examples
#' library(lubridate)
#' library(epiparameter)
#' 
#' # read in the dataset in question. 
#' # dataset needs 
#' df_ebola <- read.csv("data/ebola_1976.csv")
#' 
#' df_ebola$date <- ymd(df_ebola$date)
#' 
#' # read in onset to death distribution
#' onset_to_death_ebola <- epidist("ebola","onset_to_death")$pmf
#' 
#' # calculate static naive and corrected CFRs
#' ncfr <- calcalate_static_cfr(df_ebola, correct_for_delays = FALSE)
#' ccfr <- calcalate_static_cfr(df_ebola, correct_for_delays = TRUE, onset_to_death_ebola)
#' 
#' # just a function that formats the output of the CFR data.frames 
#' # nicely and prints to the terminal
#' format_cfr_output_neatly(ncfr)
#' format_cfr_output_neatly(ccfr)
#' 
#' # calculate rolling naive and corrected CFRs
#' df_ncfr <- calcalate_rolling_cfr(df_ebola, correct_for_delays = FALSE)
#' df_ccfr <- calcalate_rolling_cfr(df_ebola, correct_for_delays = TRUE, 
#'                                  onset_to_death_ebola)
#' 
#' 
#' # plotting rolling estimates
#' plot_data_and_cfr(df_ncfr, df_ccfr)

plot_data_and_cfr <- function(df_ncfr, df_ccfr) {
  
  # putting together the data for just the first plot, to be melted
  # into long format (for ggplot)
  df_data_plot <- data.frame(date = df_ncfr$date, 
                             cases = df_ncfr$cases, 
                             deaths = df_ncfr$deaths)
  
  # melting using reshape package into longer format
  df_data_plot_long <- melt(df_data_plot, id = "date")
  
  # plotting the top panel of the raw data
  p_1 <- ggplot(data = df_data_plot_long,
                aes(x = date, y = value, colour = variable)) + 
    geom_line(alpha = 0.2) + 
    geom_point(alpha = 0.5) +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  # putting together the naive CFR estimates for the second panel
  df_ncfr_plot <- data.frame(date = df_ncfr$date, 
                             cfr_me = df_ncfr$cfr_me, 
                             cfr_lo = df_ncfr$cfr_lo,
                             cfr_hi = df_ncfr$cfr_hi,
                             type = factor("ncfr"))
  
  # putting together the corrected CFR estimates for the second panel
  df_ccfr_plot <- data.frame(date = df_ccfr$date,
                             cfr_me = df_ccfr$cfr_me, 
                             cfr_lo = df_ccfr$cfr_lo,
                             cfr_hi = df_ccfr$cfr_hi,
                             type = factor("ccfr"))
  
  # appending the two CFR data.frames for plotting 
  df_cfr_plot <- rbind(df_ncfr_plot, df_ccfr_plot)
  
  # plotting the second panel of CFR estimates
  p_2 <- ggplot(data = df_cfr_plot) + 
    geom_line(aes(x = date, 
                  y = cfr_me,
                  fill = type),
              alpha = 1, linetype = "dashed") + 
    geom_ribbon(aes(x = date, 
                    ymin = cfr_lo, 
                    ymax = cfr_hi,
                    colour = type), 
                alpha = 0.2) + 
    theme_minimal() +
    theme(legend.position = "bottom")
  
  # putting the two panels on top of each other using patchwork
  p_out <- p_1/p_2
  
  return(p_out)
}