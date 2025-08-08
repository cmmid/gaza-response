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
  variable_levels <- c( "Weight, kg",
                        "% change in weight from first measurement",
                        "% change in weight since October 2023",
                        "BMI",
                        "% change in BMI from first measurement",
                        "% change in BMI since October 2023",
                        "NA",
                        "underweight",
                        "normal",
                        "overweight",
                        "obese")
  agegroup_levels <- c("Under 30 years", "30-44 years", "Over 45 years")

  # Filter data for the selected option
  datarecoded_df <- dataframe |>
    mutate(variable = case_when(
      variable == "weight" ~ "Weight, kg",
      variable == "weight_percent_change_firstmeasurement" ~ "% change in weight from first measurement",
      variable == "weight_percent_change_prewar" ~ "% change in weight since October 2023",
      variable == "bmi" ~ "BMI",
      variable == "bmi_percent_change_firstmeasurement" ~ "% change in BMI from first measurement",
      variable == "bmi_percent_change_prewar" ~ "% change in BMI since October 2023",
      variable == "bmi_category_NA" ~ "NA",
      variable == "bmi_category_normal" ~ "Normal",
      variable == "bmi_category_obese" ~ "Obese",
      variable == "bmi_category_underweight" ~ "Underweight",
      variable == "bmi_category_overweight" ~ "Overweight"
    ))

  factored_df <- datarecoded_df |>
    mutate(variable = factor(variable, levels = variable_levels[variable_levels %in% unique(datarecoded_df$variable)]),
           agegroup = factor(agegroup, levels = agegroup_levels[agegroup_levels %in% unique(datarecoded_df$agegroup)])) |>
    arrange(agegroup)

  final_factored_df <- factored_df |>
    mutate(label = factor(label, levels = unique(factored_df$label)))


  return(final_factored_df)

}





