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
  data <- filter(data, group == strata)
  data <- filter(data, variable == "weight_percent_change_prewar")
  params <- list(
    # Most recent date
    latest_date = data$current_summary_date[1],
    # N
    cohort_size = max(data$cohort_id_enrolled, na.rm=TRUE),
    cohort_percent_participating = data$cohort_obs_recorded /
      data$cohort_id_enrolled * 100,
    # Observations
    observations = sum(distinct(filter(data, stat == "mean"))$cohort_obs_recorded),
    median_change = round(filter(data, stat == "median")$value, 1),
    upper_change = round(filter(data, stat == "q3")$value, 1),
    lower_change = round(filter(data, stat == "q1")$value, 1)
  )

  return(params)
}
