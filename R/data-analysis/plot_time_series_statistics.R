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

### Plot
#...............................................................................
source(here("R", "data-analysis/ggplot_theme.R"))
plot_time_series_statistics <- function(data_timeseries,
                                        strata = "overall"){

  # Filter data for the selected option
  plot_data <- data_timeseries[[tolower(strata)]]

  #plot_data <- rename_with_dictionary(plot_data, data_dictionary, long = TRUE)

  plot_data <- plot_data |>
    filter(variable == "bmi_change_percent_prewar") |>
    pivot_wider(names_from = stat, values_from = value) %>%
    filter(!is.na(median))

  # set caption with N participants
  # caption_n <- plot_data |>
  #   group_by(label) |>
  #   summarise(strata_n = sum(cohort_id_new)) |>
  #   mutate(strata_n = paste0(label, ": N=", strata_n))
  # caption <- paste0("Showing percentage change in units of Body Mass Index (BMI) from self-reported weight before the war, to most recent measurement during study period. \n ",
  #                     paste0(unique(caption_n$strata_n), collapse = "; "))

  # plot
    plot <- plot_data |>
      ggplot(aes(x = date)) +
      geom_linerange(aes(ymin = q1, ymax = q3, color = label),
                     position = position_dodge(1),
                     alpha = 0.3,
                     linewidth = 2) +
      geom_point(aes(y = median, color = label),
                 position = position_dodge(1),
                 alpha = 0.8,
                 size = 2, shape = 15) +
      geom_hline(aes(yintercept = 0), lty = 2,alpha = 0.1) +
      labs(x = NULL, y = NULL,
           fill = "Strata", colour = "Strata",
           #caption = caption
           ) +
      scale_y_continuous(minor_breaks = c(NA, 0, NA)) +
      facet_wrap(~label, scales = "free_y") +
      #gghighlight() +
      theme(lshtm_theme())

  return(plot)

}
