plot_cfr_and_data <- function(dt_cfr) {
  
  p1 <- dt_ebola_long %>% 
    ggplot() + 
    geom_line(aes(x = date, y = value, colour = variable), alpha = 0.2) + 
    geom_point(aes(x = date, y = value, colour = variable), alpha = 0.5) +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  p2 <- dt_ebola_cfrs %>% 
    ggplot() + 
    geom_line(aes(x = date, y = me, colour = type), linetype = "dashed") +
    geom_ribbon(aes(x = date, ymin = lo, ymax = hi, fill = type), alpha = 0.5) + 
    # coord_cartesian(ylim = c(0, 1.2)) +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  p_out <- p1 + p2
  
  return(p_out)
}

# plot_on_single_plot <- function()
# max_first  <- dt_cfr[, max(value)]   # Specify max of first y axis
# max_second <- dt_cfr[, max(cfr_value, na.rm = TRUE)] # Specify max of second y axis
# min_first  <- dt_cfr[, min(value)]   # Specify min of first y axis
# min_second <- dt_cfr[, min(cfr_value, na.rm = TRUE)] # Specify min of second y axis
# 
# # scale and shift variables calculated based on desired mins and maxes
# scale = (max_second - min_second)/(max_first - min_first)
# shift = min_first - min_second
# 
# # Function to scale secondary axis
# scale_function <- function(x, scale, shift){
#   return ((x)*scale - shift)
# }
# 
# # Function to scale secondary variable values
# inv_scale_function <- function(x, scale, shift){
#   return ((x + shift)/scale)
# }
# 
# p_out <- ggplot(dt_cfr) +
#   geom_point(aes(x = date, 
#                  y = value, 
#                  colour = type), alpha = 0.2) +
#   geom_line(aes(x = date,
#                 y = value,
#                 colour = type), alpha = 0.25) +
#   geom_line(aes(x = date,
#                 y = value_rolling,
#                 colour = type_rolling), alpha = 0.25) +
#   geom_line(aes(x = date, 
#                 y = inv_scale_function(cfr_value, scale, shift),
#                 colour = cfr_type)) +
#   # geom_errorbar(aes(x = date, 
#   #               ymin = inv_scale_function(lo, scale, shift),
#   #               ymax = inv_scale_function(hi, scale, shift),
#   #               colour = cfr_type), alpha = 0.1) +
#   # scale_x_continuous(breaks = seq(0, 336, 24)) +
#   scale_y_continuous(sec.axis = sec_axis(~scale_function(., scale, shift),
#                                          name = "cfr_type", labels = scales::percent)) +
#   theme_minimal() + 
#   labs(x = "Date", y = "Value", colour = "Data type") + 
#   theme(legend.position = "bottom")