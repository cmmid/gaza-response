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
    filter(!Group == "other/prefer not to share") %>%
    filter(Stratification == strata) %>%
    mutate(Group = factor(Group, levels = datadict[[strata]]),
           Variable = factor(Variable, levels = datadict[["variable"]]))

  if (strata == "Overall") {
  # Generate plot
    fig <- data %>%
      ggplot(aes(x = Date)) +
      geom_line(aes(y = mean, group = Variable, colour = "Mean"), linetype = "solid") +
      geom_line(aes(y = median, group = Variable, colour = "Median"), linetype = "dashed") +
      geom_ribbon(aes(ymin = q1, ymax = q3, group = Variable, fill = "IQR"), alpha = 0.2, linetype = 0) +
      scale_colour_manual(values = c("Mean" = "darkred", "Median" = "darkblue")) +
      scale_fill_manual(values = c("IQR" = "darkblue")) +
      facet_wrap(~Variable, ncol = 1,  strip.position = "right", labeller = label_wrap_gen(width = 25), scales = "free") +
      labs(x = "Date", y = "Value") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
            strip.text.x = element_blank(),
            legend.title     = element_blank(),
            legend.spacing.y = unit(2, "pt"),
            legend.margin    = margin(2, 2, 2, 2))

  }

  else {
    fig <- data %>%
      ggplot(aes(x = Date)) +
      geom_line(aes(y = mean,  colour = Group), linetype = "solid", show.legend = F) +
      geom_line(aes(y = median,  colour = Group), linetype = "dashed", show.legend = F) +
      geom_ribbon(aes(ymin = q1, ymax = q3, fill = Group), alpha = 0.2, linetype = 0) +
      facet_wrap(~Variable, ncol = 1, scales = "free_y", strip.position = "right", labeller = label_wrap_gen(width = 25)) +
      labs(x = "Date", y = "Value") +
      labs(fill = strata) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
            legend.spacing.y = unit(2, "pt"),
            legend.margin    = margin(2, 2, 2, 2))

  }

  return(fig)

}
