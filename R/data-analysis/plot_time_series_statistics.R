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
source(here("R", "data-analysis/ggplot_theme.R"))
plot_time_series_statistics <- function(data, summary_variable,
                                        strata = "overall"){

  # Filter data for the selected option
  data_filter <- data[[tolower(strata)]]

  data_filter <- data_filter |>
    filter(variable == summary_variable) |>
    # filter out any duplicate "current" records or "Other" values
    dplyr::filter(date <= Sys.Date()) %>%
    pivot_wider(names_from = stat, values_from = value) %>%
    filter(!grepl("other|prefer no", label)) |>
    filter(!is.na(median))

  data_filter <- recode_data_table(data_filter)

    fig <- data_filter |>
      ggplot(aes(x = date)) +
      geom_linerange(aes(ymin = q1, ymax = q3, color = label),
                     position = position_dodge(1),
                     alpha = 0.3,
                     linewidth = 2) +
      geom_point(aes(y = median, color = label),
                 position = position_dodge(1),
                 alpha = 0.8,
                 size = 2, shape = 15) +
      labs(x = NULL, y = NULL,
           fill = "Strata", colour = "Strata") +
      facet_wrap(~label,
                 scales = "free_y") +
      gghighlight() +
      theme(lshtm_theme())

  return(fig)

}
