#...............................................................................
### ++ MONITORING THE NUTRITIONAL STATUS OF HUMANITARIAN WORKERS IN GAZA +++ ###
#...............................................................................

#...............................................................................
## ------ R SCRIPT TO GENERATE PLOTS OF CURRENT SUMMARY STATISTICS ----- ##
#...............................................................................

#...............................................................................
### Preparatory steps
#...............................................................................

#...................................
## Install or load required R packages
pacman::p_load(
  ggplot2,       # Visualise data
  tidyverse)     # Tidyverse suite of packages

#...............................................................................
### Read in current summary statistics data
#...............................................................................

# current_summary_stats_filename <- "current_summary_stats.csv"
# current_summary_stats <- read.csv(current_summary_stats_filename)

#...............................................................................
### Plot
#...............................................................................

plot_current_summary_stats <- function(data, strata = "overall"){

  # Filter data for the selected option
  data_filter <- data[[tolower(strata)]] |>
    pivot_wider(names_from = stat, values_from = value) %>%
    dplyr::filter(!is.na(mean)) |>
    filter(!grepl("_firstmeasurement", variable)) |>
    filter(!grepl("other|prefer no", label))

  data_filter <- recode_data_table(data_filter)

  if (strata == "overall") {
    # Generate plot
    fig <- data_filter %>%
      ggplot(aes(y = 0)) +
      geom_linerange(aes(xmin = q1, xmax = q3),
                     color = "#01454f", alpha = 0.3,
                     linewidth = 2) +
      geom_point(aes(x = median),
                 color = "#01454f", alpha = 0.8,
                 size = 3) +
      facet_wrap(~variable,
                 nrow = 2,
                 scales = "free",
                 labeller = label_wrap_gen(width = 25)) +
      labs(x = NULL,
           caption = "Median and 25-75% range") +
      theme(axis.text.y  = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank())
  }

  else {
    fig <- data_filter %>%
      ggplot(aes(y = label)) +
      geom_linerange(aes(xmin = q1, xmax = q3,
                         color = label),
                     alpha = 0.3,
                     height = 0,
                     linewidth = 2,
                     show.legend = F) +
      geom_point(aes(x = median, col = label),
                 alpha = 0.8,
                 size = 3,
                 show.legend = F) +
      facet_wrap(~variable,
                 nrow = 2,
                 scales = "free_x",
                 labeller = label_wrap_gen(width = 25)) +
      labs(x = NULL,
           caption = "Median and 25-75% range") +
      theme(axis.title.y = element_blank(),
        legend.position = "bottom")
  }


  return(fig)

}
