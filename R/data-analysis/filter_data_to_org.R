# TODO remove as unused

#...............................................................................
### ++ MONITORING THE NUTRITIONAL STATUS OF HUMANITARIAN WORKERS IN GAZA +++ ###
#...............................................................................

#...............................................................................
## ------------- R SCRIPT TO FILTER DATA TO ORGANISATION -------------------- ##
#...............................................................................

#...............................................................................
### Preparatory steps
#...............................................................................

#...............................................................................
### Generate list of key insights
#...............................................................................


filter_data_to_org <- function(data, organisation) {
  filtered_data <- list(

    bmicategory_proportions = filter(data$bmicategory_proportions, Organisation == organisation),

    current_summary_stats = filter(data$current_summary_stats, Organisation == organisation),

    time_series_table = filter(data$time_series_table, Organisation == organisation)
  )
  return(filtered_data)
}
