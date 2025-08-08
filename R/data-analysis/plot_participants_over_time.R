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

plot_participants_over_time <- function(data, strata = "Overall",
                                        plot_palette = NULL){

  # Filter data for the selected option
  data_filter <- data[[tolower(strata)]] |>
    pivot_wider(names_from = stat, values_from = value) %>%
    dplyr::filter(!is.na(mean)) |>
    filter(variable == "weight") |>
    full_join(expand.grid(list(date = as.Date(min(data[[tolower(strata)]]$date) - 1),
                               cohort_n = 0,
                               label = unique(data[[tolower(strata)]]$label))))

  if (strata == "Overall") {
    # Generate plot
    fig <- data_filter %>%
      ggplot() +
      geom_area(aes(x = date, y = cohort_n, fill = label, group = label),
                position = "stack", alpha = 0.8, show.legend = F) +
      labs(x = "Date",
           y = "Participants",
           fill = "Demographics",
           caption = "Showing cumulative number of participants in the study over time") +
      scale_fill_manual(values = plot_palette[["lshtm"]]) +
      #theme_bw() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
            strip.text.y = element_blank())
  }

  else {
    fig <- data_filter %>%
      ggplot() +
      geom_area(aes(x = date, y = cohort_n, fill = label, group = label),
                position = "stack", alpha = 0.8) +
      labs(x = "Date",
           y = "Participants",
           fill = "Demographics",
           caption = "Showing cumulative number of participants in the study over time") +
      scale_fill_viridis_d(option = "D") +
      #theme_bw() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
            strip.text.y = element_blank())
  }


  return(fig)

}
