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
    # filter out the duplicate "current" records
    dplyr::filter(date <= Sys.Date()) %>%
    pivot_wider(names_from = stat, values_from = value) %>%
    dplyr::filter(!is.na(mean)) |>
    filter(!grepl("_firstmeasurement", variable)) |>
    filter(!grepl("other|prefer no", label))

  data_filter <- recode_data_table(data_filter)

  if (tolower(strata) == "overall") {

    fig <- data_filter |>
      ggplot(aes(x = date)) +
      geom_linerange(aes(ymin = q1, ymax = q3),
                     color = "#01454f",
                     alpha = 0.3,
                     linewidth = 2) +
      geom_point(aes(y = median),
                 color = "#01454f",
                 alpha = 0.7,
                 size = 2) +
      facet_wrap(~label, scales = "free_y") +
      labs(x = NULL, y = NULL) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
            legend.spacing.y = unit(2, "pt"),
            legend.margin = margin(2, 2, 2, 2))

  } else {
    fig <- data_filter |>
      ggplot(aes(x = date)) +
      geom_linerange(aes(ymin = q1, ymax = q3, color = label),
                     alpha = 0.3,
                     linewidth = 2) +
      geom_point(aes(y = median, color = label),
                 alpha = 0.7,
                 size = 2) +
      facet_wrap(~label,
                 scales = "free_y") +
      labs(x = NULL, y = NULL) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
            legend.spacing.y = unit(2, "pt"),
            legend.margin    = margin(2, 2, 2, 2))

  }

  return(fig)

}
