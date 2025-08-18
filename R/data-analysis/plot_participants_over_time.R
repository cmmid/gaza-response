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
### Read in current summary statistics data
#...............................................................................


#...............................................................................
### Plot
#...............................................................................

plot_participants_over_time <- function(data, strata = "Overall"){

  plot_participants <- data[[tolower(strata)]] |>
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
    labs(x = NULL, y = "Participant data",
         caption = "Daily data availability: showing
         - measurements contributed by newly recruited participants (green)
         - measurements from previously enrolled participants (blue)
         - previously enrolled participants without new data (grey, 'lost to follow up').") +
    facet_wrap(~label, scales="free_y")
  return(plot_participants)
}
