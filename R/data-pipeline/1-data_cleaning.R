#...............................................................................
### ++ MONITORING THE NUTRITIONAL STATUS OF HUMANITARIAN WORKERS IN GAZA +++ ###
#...............................................................................

#...............................................................................
## ------ R SCRIPT TO CLEAN AND MATCH SIGN UP DATA WITH FOLLOW UP DATA  ----- ##
#...............................................................................

#...............................................................................
### Preparatory steps
#...............................................................................

# Install or load packages from CRAN
pacman::p_load(
  lubridate,
  ggplot2,       # Visualise data
  ggpubr,        # Arrange multiple plots into a single plot
  ggrepel,       # Improve labelling of plots
  MASS,          # Implement various statistical methods
  mgcv,          # Fit generalised additive models
  scales,        # Scale and format data for visualisation
  tidyverse,     # Tidyverse suite of packages
  viridis)       # Colour-blind palette

# example
# # Load data stored locally
# base_data <- readRDS(here("data", "processed", "df_base.RDS"))
# fup_data <- readRDS(here("data", "processed", "df_fup.RDS"))
# data <- clean_data(base_data, fup_data)

#...............................................................................
### Read in and match data
#...............................................................................

clean_data <- function(base_data, fup_data) {

  # parse dates in raw data
  base_data <- base_data |>
    dplyr::mutate(date = parse_date_time(date, orders = c("ymd", "dmy")))
  fup_data <- fup_data |>
    dplyr::mutate(date = parse_date_time(date, orders = c("ymd", "dmy")))

  # Create complete grid of all possible participant dates
  id_date_grid <- expand_grid(
    date = seq.Date(min(as.Date(base_data$date), na.rm = TRUE),
                    max(as.Date(fup_data$date), na.rm = TRUE),
                    by = "day"),
    id = unique(base_data$id))

  # add follow up weight measurements to complete grid
  id_date_grid <- id_date_grid |>
    left_join(fup_data, by = c("id", "date"))

  # add baseline characteristics and weight reading to complete grid
  # - relabel baseline weight measurement
  base_weight <- base_data |>
    rename("date_first_measurement" = date,
           "first_weight_measurement" = weight)
  # - add to grid as new variables
  id_date_grid <- left_join(id_date_grid,
                            base_weight,
                            by = c("id"))

  # use baseline weight reading as first day's weight
  id_date_grid <- id_date_grid |>
    mutate(weight = if_else(date == date_first_measurement,
                           first_weight_measurement, weight)) |>
    # remove records from before enrolment
    filter(date >= date_first_measurement)

# Dates & cohort time -----------------------------------------------------
  observed_data <- id_date_grid |>
    dplyr::group_by(id) |>
    # time in cohort
    dplyr::mutate(
      participant_recorded = !is.na(weight),
      participant_cumulative_days_enrolled = 1 + as.integer(
                    difftime(date, date_first_measurement, units = "days")),
      participant_cumulative_days_recorded = cumsum(!is.na(weight)),
      participant_timepoint = fct(if_else(date == date_first_measurement,
                                          "Study entry", "Follow up"))) |>
    # exclude participants with no weight measurement
    filter(participant_cumulative_days_recorded > 0) |>
    # add latest measure as separate variable
    mutate(last_measurement = participant_cumulative_days_recorded == max(
      participant_cumulative_days_recorded, na.rm = TRUE) & !is.na(weight),
           date_last_measurement = date[which(last_measurement)],
           weight_latest_measurement = weight[which(last_measurement)])

  #...............................................................................
  ### Add BMI and % wt change
  #...............................................................................

  observed_data <- observed_data |>
    group_by(id) |>
    dplyr::mutate(
      # BMI
      bmi = weight / (height/100)^2,
      bmi_prewar = if_else(!is.na(weight),
                           weight_prewar / (height/100)^2,
                           NA),
      first_bmi_measurement = bmi[date == date_first_measurement],
      last_bmi_measurement = bmi[date == date_last_measurement],
      # change since study entry
      weight_percent_change_firstmeasurement = ((weight - first_weight_measurement)/
                                                  first_weight_measurement)*100,
      bmi_percent_change_firstmeasurement = ((bmi - first_bmi_measurement)/
                                               first_bmi_measurement)*100,
      # daily rate of change since study entry
      bmi_rate_change_daily = (bmi - first_bmi_measurement) /
        participant_cumulative_days_enrolled,
      weight_rate_change_daily = (weight - first_weight_measurement) /
        participant_cumulative_days_enrolled,
      # change since prewar
      weight_unit_change_prewar = weight - weight_prewar,
      bmi_unit_change_prewar = bmi - bmi_prewar,
      weight_percent_change_prewar = ((weight - weight_prewar) /
                                        weight_prewar)*100,
      bmi_percent_change_prewar = ((bmi - bmi_prewar) /
                                     bmi_prewar)*100) |>
    ungroup() |>
    # drop 0 percent change on date of first measurement
    mutate(across(contains("_percent_change_firstmeasurement"),
           ~ ifelse(date == date_first_measurement, NA, .x)))

 change_from_previous <- observed_data %>%
    dplyr::arrange(id, date) %>%
    dplyr::group_by(id) %>%
    dplyr::filter(!is.na(weight)) %>%
    dplyr::mutate(previous_weight = lag(weight),
                  previous_bmi = lag(bmi),
                  days_since_previousmeasurement = as.integer(
                    difftime(date, lag(date), units = "days")),
                  weight_percent_change_previousmeasurement = ((weight - previous_weight) /
                                                                 previous_weight)*100,
                  bmi_percent_change_previousmeasurement = ((bmi - previous_bmi) / previous_bmi)*100) |>
    dplyr::select(id, date,
                  days_since_previousmeasurement,
                  weight_percent_change_previousmeasurement,
                  bmi_percent_change_previousmeasurement) |>
    ungroup()

 observed_data <- left_join(observed_data,
                            change_from_previous, by = c("id", "date"))
 observed_data <- ungroup(observed_data)

 observed_data <- observed_data |>
   filter(participant_recorded) |>
   # add weight anomaly factor
   mutate(
     weight_anomaly = as_factor(
       case_when(
         weight < 30 ~ "anomaly",
         weight > 180 ~ "anomaly",
         !between(bmi, 10, 60) ~ "anomaly",
         weight_rate_change_daily >= 10 ~ "anomaly",
         TRUE ~ "valid")
     )
   )

 return(observed_data)
}

# Data quality checks -----------------------------------------------------
# Set factors
set_factors <- function(df, factor_levels,
                        factor_cols = names(factor_levels)) {

  for (col_name in factor_cols) {
    if (col_name %in% names(df)) {

      if (col_name == "weight_anomaly" & is.numeric(df[[col_name]])) {
      }

      # Special cases for numeric variables
      if (col_name == "children_feeding" & is.numeric(df[[col_name]])) {
        df[[col_name]] <- case_when(
          df[[col_name]] < 0 ~ "anomaly",
          df[[col_name]] == 0 ~ "0",
          df[[col_name]] == 1 ~ "1",
          df[[col_name]] == 2 ~ "2",
          df[[col_name]] >= 3 & df[[col_name]] <= 20 ~ "3+",
          df[[col_name]] >= 20 ~ "anomaly",
          .default = NA_character_
        )
      }

      if (col_name == "agegroup" & is.numeric(df[[col_name]])) {
        df[[col_name]] <- case_when(
          df[[col_name]] < 16 ~ "anomaly",
          df[[col_name]] < 30 ~ "Age under 30",
          df[[col_name]] >= 30 & df[[col_name]] <= 45 ~ "Age 30-45",
          df[[col_name]] > 45 ~ "Age over 45",
          df[[col_name]] >= 99 ~ "anomaly",
          .default = NA_character_
        )
      }

      if (grepl("bmi_category", col_name) & is.numeric(df[[col_name]])) {
        df[[col_name]] <- case_when(
          df[[col_name]] <= 10 ~ "anomaly",
          df[[col_name]] < 18.5 ~ "Underweight",
          df[[col_name]] >= 18.5 & df[[col_name]] < 25 ~ "Normal",
          df[[col_name]] >= 25 & df[[col_name]] < 30 ~ "Overweight",
          df[[col_name]] >= 30 ~ "Obese",
          df[[col_name]] >= 60 ~ "anomaly",
          .default = NA_character_
        )
      }

      # Convert to factor first using as_factor to preserve all levels
      df[[col_name]] <- as_factor(df[[col_name]])

      # Handle named vs unnamed vectors differently
      if (is.null(names(factor_levels[[col_name]]))) {
        # Unnamed vector - use fct_relevel to reorder
        df[[col_name]] <- fct_relevel(df[[col_name]],
                                      factor_levels[[col_name]])
      } else {
        # Named vector - recode first, then relevel safely
        df[[col_name]] <- df[[col_name]] |>
          fct_recode(!!!factor_levels[[col_name]])

        # Only relevel with names that exist in the data
        expected_labels <- names(factor_levels[[col_name]])
        existing_labels <- intersect(expected_labels, levels(df[[col_name]]))
        if (length(existing_labels) > 0) {
          df[[col_name]] <- fct_relevel(df[[col_name]], existing_labels)
        }
      }
    }
  }

  return(df)
}
