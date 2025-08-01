#...............................................................................
### ++ MONITORING THE NUTRITIONAL STATUS OF HUMANITARIAN WORKERS IN GAZA +++ ###
#...............................................................................

#...............................................................................
## ------ R SCRIPT TO GENERATE PLOTS OF TIME SERIES SUMMARY STATISTICS ----- ##
#...............................................................................

#...............................................................................
### Preparatory steps
#...............................................................................

#...................................
# Install or load packages from CRAN
pacman::p_load(
  ggplot2,       # Visualise data
  tidyverse)     # Tidyverse suite of packages

#...............................................................................
### Read in current summary statistics data
#...............................................................................

# time_series_stats_filename <- "data/time_series_table.csv"
# time_series_stats <- read.csv(time_series_stats_filename)

#...............................................................................
### Plot
#...............................................................................

plot_time_series_statistics <- function(data, strata = "Overall"){

  # Filter data for the selected option
  data <- data %>%
    filter(Stratification == strata) %>%
    mutate(Variable = factor(Variable,
                             levels = c("% Weight Change from Prewar Value",
                                        "% Weight Change from First Measurement",
                                        "Weight (kg)", "BMI")))

  # Generate plot
  fig <- data %>%
    ggplot(aes(x = Date)) +
    geom_line(aes(y = mean, group = Variable, colour = "Mean"), linetype = "solid") +
    geom_line(aes(y = median, group = Variable, colour = "Median"), linetype = "dashed") +
    geom_ribbon(aes(ymin = q1, ymax = q3, group = Variable, fill = "IQR"), alpha = 0.2, linetype = 0) +
    scale_colour_manual(values = c("Mean" = "darkred", "Median" = "darkblue")) +
    scale_fill_manual(values = c("IQR" = "darkblue")) +
    facet_grid(cols = vars(Group), row = vars(Variable), switch = "y", labeller = label_wrap_gen(width = 25), scales = "free") +
    labs(x = "Date", y = "Value") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
          legend.title     = element_blank(),
          legend.spacing.y = unit(2, "pt"),
          legend.margin    = margin(2, 2, 2, 2))

  return(fig)

}
