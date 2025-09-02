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
### Plot
#...............................................................................
source(here("R", "data-analysis/ggplot_theme.R"))

plot_current_summary_stats <- function(plot_data, stratum = NULL){

  if (!is.null(stratum)) {
    plot_data <- filter(plot_data, stratum == {{ stratum }})
  }

  # set caption with N participants
  plot_data <- plot_data |>
    mutate(stratum_n = paste0(Stratum, ": N=", cohort_id_recorded))
  caption_n <- paste0("Median and 25-75% range. ",
                      paste0(unique(plot_data$stratum_n), collapse = "; "))
  # reshape and rename
  plot_data <- plot_data |>
    pivot_wider(names_from = stat, values_from = value) %>%
    filter(!is.na(median)) |>
    filter(grepl("_prewar", variable)
           #& !grepl("bmi_category_", variable)
           )

    plot <- plot_data |>
      ggplot(aes(y = Stratum)) +
      geom_linerange(aes(xmin = q1, xmax = q3),
                     alpha = 0.3,
                     linewidth = 2,
                     show.legend = F) +
      geom_point(aes(x = median),
                 alpha = 0.8,
                 size = 2,
                 show.legend = F) +
      geom_vline(aes(xintercept = 0), lty = 2,alpha = 0.1) +
      scale_colour_discrete(l = 40) +
      scale_x_continuous(limits = c(NA, 0)) +
      facet_wrap(~Variable,
                 scales = "free_x") +
      labs(x = NULL,
           caption = caption_n) +
      theme(axis.title.y = element_blank()) +
      lshtm_theme()

  return(plot)

}

