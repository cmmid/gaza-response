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
  variable_levels <- c( "Weight, kg",
                        "Change in weight since October 2023, kg",
                        "% change in weight from first measurement",
                        "% change in weight since October 2023",
                        "Daily rate of % weight change",
                        "BMI",
                        "Change in BMI since October 2023",
                        "% change in BMI from first measurement",
                        "% change in BMI since October 2023",
                        "Daily rate of change in BMI",
                        #
                        paste0("bmi_category_daily_",bmi_levels),
                        paste0("bmi_category_prewar_",bmi_levels)
  )

  # Filter data for the selected option
  datarecoded_df <- dataframe |>
    mutate(variable = case_when(
      variable == "weight" ~ "Weight, kg",
      variable == "weight_unit_change_prewar" ~ "Change in weight since October 2023, kg",
      variable == "weight_percent_change_firstmeasurement" ~ "% change in weight from first measurement",
      variable == "weight_percent_change_prewar" ~ "% change in weight since October 2023",
      variable == "bmi" ~ "BMI",
      variable == "bmi_unit_change_prewar" ~ "Change in BMI since October 2023",
      variable == "bmi_percent_change_firstmeasurement" ~ "% change in BMI from first measurement",
      variable == "bmi_percent_change_prewar" ~ "% change in BMI since October 2023",
      #
      variable == "bmi_rate_change_daily" ~ "Daily rate of change in BMI",
      variable == "weight_percent_change_daily_rate" ~ "Daily rate of % weight change",
      .default = variable
    ))

  factored_df <- datarecoded_df |>
    mutate(variable = fct(variable,
                          levels = variable_levels[variable_levels %in% unique(datarecoded_df$variable)]))

  final_factored_df <- factored_df |>
    mutate(label = fct(label, levels = unique(factored_df$label)))


  return(final_factored_df)

}





