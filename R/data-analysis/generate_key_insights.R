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
  data <- filter(data, variable == "weight_percent_change_prewar")
  params <- list(
    # Most recent date
    latest_date = data$current_summary_date[1],
    # N
    cohort_size_current = max(data$cohort_id_enrolled, na.rm=TRUE),
    cohort_size_alltime = max(data$cohort_id_enrolled_alltime, na.rm=TRUE),
    # Observations
    observations = sum(distinct(filter(data, stat == "mean"))$cohort_obs_recorded),

    median_change = round(filter(data, stat == "median")$value, 1),
    upper_change = round(filter(data, stat == "q3")$value, 1),
    lower_change = round(filter(data, stat == "q1")$value, 1)
  )
  params$cohort_percent_currently_participating <- params$cohort_size_current /
    params$cohort_size_alltime * 100

  return(params)
}
