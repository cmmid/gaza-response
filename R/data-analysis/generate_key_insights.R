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
    latest_date = max(data$time_series_table$Date),
    # N
    observations = data$bmicategory_proportions |>
      filter(Date == max(Date) & Stratification=="Overall") |>
      pull(n) |> sum(),
    # Overall median
    median_change = data$current_summary_stats |>
      filter(Stratification=="Overall" &
               Variable=="% Weight Change from Prewar Value" ) |>
      pull(median) |> round(digits = 1),
    # Stratifications
    strata = unique(data$bmicategory_proportions$Stratification)
  )
  return(params)
}

