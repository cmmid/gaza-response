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
  data_filter <- data[[tolower(strata)]] |>
    #dplyr::filter(!sex == "other/prefer not to share") %>%
    pivot_wider(names_from = stat, values_from = value) %>%
    dplyr::filter(!is.na(mean))

  if (strata == "Overall") {
    # Generate plot
    fig <- data_filter %>%
      ggplot(aes(x = date)) +
      geom_line(aes(y = mean, colour = "Mean"), linetype = "solid") +
      geom_line(aes(y = median,  colour = "Median"), linetype = "dashed") +
      geom_ribbon(aes(ymin = q1, ymax = q3,  fill = "IQR"), alpha = 0.2, linetype = 0) +
      scale_colour_manual(values = c("Mean" = "darkred", "Median" = "darkblue")) +
      scale_fill_manual(values = c("IQR" = "darkblue")) +
      facet_wrap(~variable, ncol = 1, scales = "free_y") +
      labs(x = "Date", y = "Value") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
            legend.title     = element_blank(),
            legend.spacing.y = unit(2, "pt"),
            legend.margin    = margin(2, 2, 2, 2))

  }

  else {
    fig <- data_filter %>%
      ggplot(aes(x = date)) +
      geom_line(aes(y = mean,  colour = label), linetype = "solid", show.legend = F) +
      geom_line(aes(y = median,  colour = label), linetype = "dashed", show.legend = F) +
      geom_ribbon(aes(ymin = q1, ymax = q3, fill = label), alpha = 0.2, linetype = 0) +
      facet_wrap(~variable, ncol = 1, scales = "free_y") +
      labs(x = "Date", y = "Value") +
      labs(fill = strata) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
            legend.spacing.y = unit(2, "pt"),
            legend.margin    = margin(2, 2, 2, 2))

  }

  return(fig)

}
