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

# set caption with N participants
# caption_n <- plot_data |>
#   group_by(Stratum) |>
#   summarise(strata_n = sum(cohort_id_new)) |>
#   mutate(strata_n = paste0(Stratum, ": N=", strata_n))
# caption <- paste0("Showing percentage change in units of Body Mass Index (BMI) from self-reported weight before the war, to most recent measurement during study period. \n ",
#                     paste0(unique(caption_n$strata_n), collapse = "; "))

#...............................................................................
source(here("R", "data-analysis/ggplot_theme.R"))
plot_time_series_statistics <- function(plot_data) {

  plot_data <- plot_data |>
    filter(variable == "bmi_change_percent_prewar") |>
    pivot_wider(names_from = stat, values_from = value) %>%
    filter(!is.na(median)) |>
    mutate(Participants = cut(cohort_id_recorded, c(0,10,30,50,Inf),
                   labels = c("<10", "10-20", "20-50", ">50"),
                   ordered_result = TRUE))

  # plot
    plot <- plot_data |>
      ggplot(aes(x = date)) +
      geom_linerange(aes(ymin = q1, ymax = q3,
                     alpha = Participants),
                     col = lshtm_palette$lshtm_generic,
                     linewidth = 2) +
      geom_point(aes(y = median,
                     alpha = Participants),
                 col = lshtm_palette$lshtm_generic,
                 size = 2.5) +
      geom_hline(aes(yintercept = 0), lty = 2, alpha = 0.1) +
      labs(x = NULL, y = NULL,
           caption = "<details><summary>About the data</summary>This graph shows the difference between participants' recent weight (the latest measurement within the week for each participant), compared to their pre-war weight (self-reported estimate given at study enrollment). Showing the typical range among participants each week: the median (point) and 25-75% range (linebar).</details>") +
      scale_alpha_discrete(range = c(0.1,0.7)) +
      scale_y_continuous(labels = scales::label_percent(scale = 1),
                         minor_breaks = c(NA, 0, NA)) +
      facet_wrap(~Stratum,
                 scales = "fixed") +
      theme(lshtm_theme())

  return(plot)

}
