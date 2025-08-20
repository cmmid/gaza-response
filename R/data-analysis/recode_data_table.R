#...............................................................................
### ++ MONITORING THE NUTRITIONAL STATUS OF HUMANITARIAN WORKERS IN GAZA +++ ###
#...............................................................................

#...............................................................................
## ------ R SCRIPT TO GENERATE PLOTS OF TIME SERIES SUMMARY STATISTICS ----- ##
#...............................................................................

#...............................................................................
### Preparatory steps
#...............................................................................

#...................................
# Install or load packages from CRAN
pacman::p_load(
  ggplot2,       # Visualise data
  tidyverse,
  forcats)     # Tidyverse suite of packages

#...............................................................................
### Read in current summary statistics data
#...............................................................................

# time_series_stats_filename <- "data/time_series_table.csv"
# time_series_stats <- read.csv(time_series_stats_filename)

#...............................................................................
### Plot
#...............................................................................


recode_data_table <- function(dataframe){
  bmi_levels <- c("Underweight","Normal","Overweight","Obese", "NA")
  variable_levels <- c( "Weight (kg)",
                        "Weight change since start of war (kg)",
                        "Weight change since start of war (%)",
                        "Weight change since enrollment (%)",
                        "Weight rate of change while enrolled (kg/day)",
                        "BMI",
                        "BMI change since start of war",
                        "BMI change since start of war (%)",
                        "BMI change since enrollment (%)",
                        "BMI rate of change while enrolled (units/day)",
                        #
                        paste0("bmi_category_daily_",bmi_levels),
                        paste0("bmi_category_prewar_",bmi_levels),
                        #
                        "cohort_obs_missing" = "Missing records",
                        "cohort_id_enrolled" = "Number of participants",
                        "organisation" = "Organisation",
                        "group" = "Demographic",
                        "label" = "Stratum"
  )

  # Filter data for the selected option
  datarecoded_df <- dataframe |>
    mutate(variable = case_when(
      variable == "weight" ~ "Weight, kg",
      variable == "weight_unit_change_prewar" ~ "Weight change since start of war (kg)",
      variable == "weight_percent_change_prewar" ~ "Weight change since start of war (%)",
      variable == "weight_percent_change_firstmeasurement" ~ "Weight change since enrollment (%)",
      variable == "weight_rate_change_daily" ~ "Weight rate of change while enrolled (kg/day)",
      variable == "bmi" ~ "BMI",
      variable == "bmi_unit_change_prewar" ~ "BMI change since start of war",
      variable == "bmi_percent_change_prewar" ~ "BMI change since start of war (%)",
      variable == "bmi_percent_change_firstmeasurement" ~ "BMI change since enrollment (%)",
      variable == "bmi_rate_change_daily" ~ "BMI rate of change while enrolled (units/day)",
      .default = variable
    ))

  factored_df <- datarecoded_df |>
    mutate(variable = fct(variable,
                          levels = variable_levels[variable_levels %in% unique(datarecoded_df$variable)]))

  final_factored_df <- factored_df |>
    mutate(label = fct(label, levels = unique(factored_df$label)))


  return(final_factored_df)

}





