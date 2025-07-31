# Example BMI plot code copied from Clara
# Arguments:
#   data (in format of bmicategory_proportions.csv)
#   stratification (defaults to Overall),
#   date (defaults to latest date)
# Outputs: ggplot2 plot
#
# Example:
# data <- read_csv(here("data", "bmicategory_proportions.csv"))
# plot_bmi(data, stratification = "Overall", date = max(data$date))

plot_bmi <- function(data,
                     stratification = "Overall",
                     date = NULL) {
  # handle missing arguments
  if (is.null(date)) {date = max(data$date)}

  # filter data
  data <- data |>
    filter(date == max(date) &
             Stratification == !!stratification)
  # plot
  n_total = sum(data$n)
  fig <- data %>%
    ggplot() +
    geom_bar(aes(x = category, y = perc, fill = category), stat = "identity") +
    scale_fill_viridis_d(option = "D") +
    labs(x = "WHO BMI Category",
         y = "Percentage of Survey Participants (%)",
         caption = paste("N =", n_total)) +
    theme_bw() +
    theme(legend.position = "none",
          plot.caption = element_text(hjust = 0, face = "italic"))

  return(fig)
  }
