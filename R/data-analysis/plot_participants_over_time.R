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
plot_participants_over_time <- function(data_timeseries, strata = "overall"){

  plot_data <- data_timeseries[[tolower(strata)]]
  # reshape data
  plot_data <- plot_data |>
    filter(!is.na(date)) |>
    filter(variable == "weight" & stat == "median") |>
    dplyr::select(-value) |>
    group_by(organisation, group, label) |>
    mutate(cohort_participant_cumsum = cumsum(cohort_id_recorded),
           cohort_record_cumsum = cumsum(cohort_obs_recorded)) |>
    group_by(date, organisation, group, label) |>
    pivot_longer(cols = c("cohort_obs_missing",
                          "cohort_id_new",
                          "cohort_id_followup_record")) |>
    mutate(Participant = fct_rev(fct_recode(name,
                                            "Missing"="cohort_obs_missing",
                                            "New"="cohort_id_new",
                                            "Repeated"="cohort_id_followup_record"))) |>
    ungroup()

  # set caption with N participants
  caption <- plot_data |>
    group_by(label) |>
    filter(cohort_participant_cumsum == max(cohort_participant_cumsum)) |>
    mutate(strata_n = paste0(label, ": N=", cohort_participant_cumsum))
  caption_n <- paste0("Unique participants (cumulative total, grey shaded), and weight measurements (cumulative total, grey line). \n Daily measurements shown by participant status (missing, new joiner, or previously enrolled in the study).\n",
    paste0(unique(caption$strata_n), collapse = "; "))

  # plot
    plot <- plot_data |>
      ggplot(aes(x = date)) +
      geom_col(aes(y = value,
                   fill = Participant)) +
      geom_area(aes(y = cohort_record_cumsum), alpha = 0.1) +
      geom_line(aes(y = cohort_participant_cumsum), alpha = 0.1) +
      labs(x = NULL, y = "Participant observations",
           caption = caption_n) +
      facet_wrap(~label, scales="free_y") +
      #gghighlight() +
      lshtm_theme()

  return(plot)
}
