#' Plot the raw case and death time-series data
#'
#' @description Produces a simple plot, using base R, of the raw case and death
#' time-series data
#'
#' @param df_in A data.frame containing the outbreak data. A daily time series
#' with dates or some other absolute indicator of time (e.g. epiday/epiweek) and
#' the numbers of new cases and new deaths at each time point
#'
#' @return A plot of the two time-series on the same plot, with a legend
#' 
#' @export
#'
#' @examples
#' # Load Ebola 1976 outbreak data
#' data("ebola1976")
#'
#' plot_raw_data(ebola_1976)

plot_raw_data <- function(df_in,
                          together = TRUE) {
  
  dates <- df_in$date
  cases <- df_in$cases
  deaths <- df_in$deaths
  
  if(together == TRUE) {
    # plot(dates, cases, 
    #      col = "blue", )
    plot(dates, cases, 
         col = "blue", 
         type = "s", lwd = 2,
         xlab = "Date", ylab = "Incidence")
    # points(dates, deaths, col = "red")
    lines(dates, deaths, col = "red", type="s", lwd = 2,)
    legend("topright", 
           legend = c("Cases", "Deaths"), 
           col = c("blue", "red"),
           lty = 1,
           cex = 1)
    
    
    grid(nx = NULL, ny = NULL,
         lty = 2,      # Grid line type
         col = "gray", # Grid line color
         lwd = 2) 
    
  } else {
    
    par(mfrow=c(1, 2)) 
    
    # plot(dates, cases, 
    #      col = "blue")
    plot(dates, cases,
         col = "blue", type = "s", lwd = 2, 
         xlab = "Date", ylab = "Incidence")
    legend("topright", 
           legend = "Cases", 
           col = "blue",
           lty = 1,
           cex = 1)
    
    grid(nx = NULL, ny = NULL,
         lty = 2,      # Grid line type
         col = "gray", # Grid line color
         lwd = 2) 

    plot(dates, deaths,
          col = "red", type="s", lwd = 2,
          xlab = "Date", ylab = "Incidence")
    legend("topright", 
           legend = "Deaths", 
           col = "red",
           lty = 1,
           cex = 1)
    
    grid(nx = NULL, ny = NULL,
         lty = 2,      # Grid line type
         col = "gray", # Grid line color
         lwd = 2) 
    
  }
  
  par(mfrow = c(1, 1))

}
