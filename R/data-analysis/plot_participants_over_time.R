#...............................................................................
### ++ MONITORING THE NUTRITIONAL STATUS OF HUMANITARIAN WORKERS IN GAZA +++ ###
#...............................................................................

#...............................................................................
## ------ R SCRIPT TO GENERATE PLOTS OF PARTICIPANTS OVER TIME ----- ##
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
plot_participants_over_time <- function(plot_data){

  # reshape data
  plot_data <- plot_data |>
    filter(!is.na(date)) |>
    filter(variable == "weight_daily" & stat == "median") |>
    dplyr::select(-value) |>
    group_by(organisation, strata, stratum) |>
    mutate(cohort_participant_cumsum = cumsum(cohort_id_recorded),
           cohort_record_cumsum = cumsum(cohort_obs_recorded)) |>
    group_by(date, organisation, strata, stratum) |>
    pivot_longer(cols = c("cohort_id_new",
                          "cohort_id_followup_record")) |>
    mutate(Participant = fct_rev(fct_recode(name,
                                            "New joiner"="cohort_id_new",
                                            "Returning"="cohort_id_followup_record"))) |>
    ungroup()

  # plot
    plot <- plot_data |>
      ggplot(aes(x = date)) +
      geom_col(aes(y = value,
                   fill = Participant)) +
      geom_area(aes(y = cohort_record_cumsum), alpha = 0.1) +
      geom_line(aes(y = cohort_participant_cumsum), alpha = 0.1) +
      scale_fill_manual(values = c("Missing" = "grey50",
                                   "New joiner" = "#709b28",
                                   "Returning" = "#01454f")) +
      labs(x = NULL, y = "Participant measurements") +
      facet_wrap(~Stratum, scales="free_y", ncol=1) +
      #gghighlight() +
      lshtm_theme()

  return(plot)
}
