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
plot_time_series_statistics <- function(plot_data,
                                        strata = "overall",
                                        window = "week") {

  plot_data <- plot_data |>
    filter(variable == "bmi_change_percent_prewar") |>
    pivot_wider(names_from = stat, values_from = value) %>%
    filter(!is.na(median)) |>
    mutate(Participants = cut(cohort_id_recorded, c(0,10,30,100),
                   labels = c("<10", "10-30", ">30"),
                   ordered_result = TRUE))


  # set caption with N participants
  # caption_n <- plot_data |>
  #   group_by(Stratum) |>
  #   summarise(strata_n = sum(cohort_id_new)) |>
  #   mutate(strata_n = paste0(Stratum, ": N=", strata_n))
  # caption <- paste0("Showing percentage change in units of Body Mass Index (BMI) from self-reported weight before the war, to most recent measurement during study period. \n ",
  #                     paste0(unique(caption_n$strata_n), collapse = "; "))

  # plot
    plot <- plot_data |>
      ggplot(aes(x = date)) +
      geom_linerange(aes(ymin = q1, ymax = q3,
                     alpha = Participants),
                     position = position_dodge(1),
                     linewidth = 2) +
      geom_point(aes(y = median,
                     alpha = Participants),
                 position = position_dodge(1),
                 size = 2) +
      geom_hline(aes(yintercept = 0), lty = 2, alpha = 0.1) +
      labs(x = NULL, y = NULL) +
      scale_y_continuous(minor_breaks = c(NA, 0, NA)) +
      facet_wrap(~Stratum,
                 scales = "fixed") +
      #gghighlight() +
      theme(lshtm_theme())

  return(plot)

}
