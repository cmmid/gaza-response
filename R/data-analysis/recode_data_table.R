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
  tidyverse)     # Tidyverse suite of packages

#...............................................................................
### Read in current summary statistics data
#...............................................................................

# time_series_stats_filename <- "data/time_series_table.csv"
# time_series_stats <- read.csv(time_series_stats_filename)

#...............................................................................
### Plot
#...............................................................................


recode_data_table <- function(dataframe){
  variable_levels <- c( "weight",
                        "% change in weight from first measurement",
                        "% change in weight from prewar",
                        "bmi",
                        "% change in bmi from first measurement",
                        "% change in bmi from prewar",
                        "NA",
                        "underweight",
                        "normal",
                        "overweight",
                        "obese")
  agegroup_levels <- c("Under 30 years", "30-44 years", "Over 45 years")

  # Filter data for the selected option
  datarecoded_df <- dataframe |>
    mutate(variable = case_when(
      variable == "weight" ~ "weight",
      variable == "weight_percent_change_firstmeasurement" ~ "% change in weight from first measurement",
      variable == "weight_percent_change_prewar" ~ "% change in weight from prewar",
      variable == "bmi" ~ "bmi",
      variable == "bmi_percent_change_firstmeasurement" ~ "% change in bmi from first measurement",
      variable == "bmi_percent_change_prewar" ~ "% change in bmi from prewar",
      variable == "bmi_category_NA" ~ "NA",
      variable == "bmi_category_normal" ~ "normal",
      variable == "bmi_category_obese" ~ "obese",
      variable == "bmi_category_underweight" ~ "underweight",
      variable == "bmi_category_overweight" ~ "overweight"
    ))

  factored_df <- datarecoded_df |>
    mutate(variable = factor(variable, levels = variable_levels[variable_levels %in% unique(datarecoded_df$variable)]),
           agegroup = factor(agegroup, levels = agegroup_levels[agegroup_levels %in% unique(datarecoded_df$agegroup)])) |>
    arrange(agegroup)

  final_factored_df <- factored_df |>
    mutate(label = factor(label, levels = unique(factored_df$label)))


  return(final_factored_df)

}





