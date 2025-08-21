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
plot_participants_over_time <- function(data, strata = "overall"){

  plot_participants <- data[[tolower(strata)]] |>

    data |>
    filter(!is.na(date)) |>
    filter(variable == "weight" & stat == "median") |>
    dplyr::select(-value) |>
    mutate(cohort_id_followup = cohort_obs_recorded - cohort_id_new) |>
    group_by(date, organisation, group, label) |>
    pivot_longer(cols = c("cohort_obs_missing",
                          "cohort_id_new",
                          "cohort_id_followup")) |>
    mutate(Participant = fct_rev(fct_recode(name,
                                            "Lost to follow up"="cohort_obs_missing",
                                            "New"="cohort_id_new",
                                            "Repeated"="cohort_id_followup"))) |>
    ggplot(aes(x = date)) +
    geom_col(aes(y = value,
                 fill = Participant, alpha = Participant)) +
    labs(x = NULL, y = "Participant data") +
    facet_wrap(~label, scales="free_y") +
    gghighlight() +
    lshtm_theme()

  return(plot_participants)
}
