#...............................................................................
### ++ MONITORING THE NUTRITIONAL STATUS OF HUMANITARIAN WORKERS IN GAZA +++ ###
#...............................................................................

#...............................................................................
## ------------- R SCRIPT TO GENERATE KEY INSIGHTS FROM DATA ---------------- ##
#...............................................................................

#...............................................................................
### Preparatory steps
#...............................................................................

#...............................................................................
### Generate list of key insights
#...............................................................................


generate_key_insights <- function(data, strata) {
  data <- data[[tolower(strata)]]

  summary_table <- data |>
    filter(variable %in% c("bmi_unit_change_prewar",
                           "weight_unit_change_prewar",
                           "bmi_percent_change_prewar",
                           "weight_percent_change_prewar")) |>
    pivot_wider(names_from = stat, values_from = value) |>
    dplyr::select(date, organisation, label,
                  variable, median, q1, q3)

  summary_cohort <- data |>
    distinct(organisation,
             cohort_id_enrolled, participant_in_followup,
             group, label) |>
    pivot_longer(contains("cohort"), names_to = "variable")

  summary_insights <- left_join(summary_table, summary_cohort, by = c("label")) |>
    drop_na() |>
    mutate(across(where(is.numeric), ~round(., digits=1))) |>
    mutate("Median (25-75% range)" = paste0(median, " (", q1, "-", q3, ")")) |>
    dplyr::select("Organisation" = organisation,
                  "Strata" = label, "Indicator" = variable, "Median (25-75% range)")

  params <- list(
    # Most recent date
    latest_date = max(data$current_summary_date, na.rm=TRUE),
    # N
    cohort_size = max(data$cohort_id_enrolled, na.rm=TRUE),
    # Observations
    #
    median_change = round(filter(data, stat == "median")$value, 0),
    upper_change = round(filter(data, stat == "q3")$value, 0),
    lower_change = round(filter(data, stat == "q1")$value, 0)
    #

  )

  return(params)
}

