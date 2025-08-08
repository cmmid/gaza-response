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


generate_key_insights <- function(data) {
  params <- list(
    # Most recent date
    latest_date = max(data$overall$date),
    # N
    cohort_size = max(data$overall$cohort_n),
    # Observations
    observations = sum(distinct(filter(data$overall, variable == "weight" & stat == "mean"))$cohort_recorded),

    median_change = round(filter(data$overall, date == max(date) & variable == "weight_percent_change_prewar" & stat == "median")$value, 2)

  )
  return(params)
}

