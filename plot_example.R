# Create example data frame
data <- data.frame(
  date = as.Date(c(
    "2020-02-05", "2020-02-06", "2020-02-07", "2020-02-08",
    "2020-02-09", "2020-02-10", "2020-02-11"
  )),
  cases = c(10, 10, 41, 3, 6, 65, 0),
  deaths = c(0, 3, 4, 5, 6, 7, 0)
)

# Add 3 weeks of blank rows
last_date <- max(data$date)
future_dates <- seq(last_date + 1, last_date + 21, by = "day")
blank_data <- data.frame(
  date = future_dates,
  cases = 0,
  deaths = 0
)

# Combine original and blank data
data <- rbind(data, blank_data)

# Reshape data from wide to long format
data_long <- tidyr::pivot_longer(
  data,
  cols = c(cases, deaths),
  names_to = "metric",
  values_to = "count"
)

# Load required package
library(ggplot2)

# Create plot
ggplot(data_long, aes(x = date, y = count)) +
  # Add bars
  geom_col(aes(fill = metric)) +
  # Customize colors
  scale_fill_manual(
    values = c("cases" = "#0072B2", "deaths" = "#D55E00"),
    labels = c("Cases", "Deaths")
  ) +
  # Create separate rows for cases and deaths
  facet_grid(metric ~ ., scales = "free_y") +
  # Customize labels
  labs(
    title = "COVID-19 Cases and Deaths Over Time",
    x = "Date",
    y = "Count",
    fill = "Metric"
  ) +
  # Customize theme
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank(),
    strip.text.y = element_text(angle = 0)
  )

# Save plot
ggsave("cases_deaths_plot.png", width = 8, height = 8) 