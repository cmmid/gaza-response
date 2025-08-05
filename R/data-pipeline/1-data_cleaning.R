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
  ggplot2,       # Visualise data
  ggpubr,        # Arrange multiple plots into a single plot
  ggrepel,       # Improve labelling of plots
  MASS,          # Implement various statistical methods
  mgcv,          # Fit generalised additive models
  scales,        # Scale and format data for visualisation
  tidyverse,     # Tidyverse suite of packages
  viridis)       # Colour-blind palette

#...............................................................................
### Read in and match data
#...............................................................................

clean_data <- function(base_data, fup_data) {
  suppressMessages({
    fup_data_expanded <- fup_data %>%
      dplyr::left_join(dplyr::select(base_data, !c(date, weight)),
                       by = "id")
  })

  full_data <- rbind(base_data, fup_data_expanded) %>%
    dplyr::arrange(id, date, desc(!is.na(weight))) %>%  # Put non-NA weights first
    dplyr::distinct(id, date, .keep_all = TRUE)

  # Clean date
  full_data <- full_data |>
    dplyr::mutate(date = parse_date_time(date, orders = c("ymd", "dmy")))

  #...............................................................................
  ### Add BMI and % wt change
  #...............................................................................

  matched_data <- full_data %>%
    dplyr::mutate(bmi = weight / (height/100)^2,
                  bmi_prewar = weight_prewar / (height/100)^2) %>%
    dplyr::group_by(id) %>%
    dplyr::mutate(date_first_measurement = min(date[!is.na(weight)])) %>%
    dplyr::mutate(first_weight_measurement = weight[date == date_first_measurement]) %>%
    dplyr::mutate(percent_change_firstmeasurement = ((weight - first_weight_measurement)/first_weight_measurement)*100,
                  percent_change_prewar = ((weight - weight_prewar)/weight_prewar)*100) |>
    ungroup()

  change_from_previous <- matched_data %>%
    dplyr::arrange(id, date) %>%
    dplyr::group_by(id) %>%
    dplyr::filter(!is.na(weight)) %>%
    dplyr::mutate(previous_weight = lag(weight)) %>%
    dplyr::mutate(percent_change_previousmeasurement = ((weight - previous_weight) / previous_weight)*100) %>%
    dplyr::select(id, date, percent_change_previousmeasurement) |>
    ungroup()

  matched_data <- left_join(matched_data,
                            change_from_previous, by = c("id", "date"))


  # Specify factor variables ------------------------------------------------
  # TODO use data dictionary here
  matched_data <- matched_data |>
    mutate(agegroup = ifelse(age < 30, "Under 30 years",
                             ifelse(age < 45, "30-44 years", "Over 45 years")))

  # Data quality checks -----------------------------------------------------

  # Flag anomalous data: TRUE if anomalous, FALSE if within range
  matched_data <- matched_data |>
    mutate(bmi_anomaly = !between(bmi, 10, 60),
           bmi_prewar_anomaly = !between(bmi_prewar, 10, 60),
           change_anomaly = percent_change_previousmeasurement >= 10)

  # Add BMI categories
  matched_data <- matched_data |>
    mutate(bmi_category = case_when(
      bmi <= 10 ~ NA_character_,
      bmi < 18.5 ~ "underweight",
      bmi >= 18.5 & bmi < 25 ~ "normal",
      bmi >= 25 & bmi < 30 ~ "overweight",
      bmi >= 30 ~ "obese",
      bmi >= 60 ~ NA_character_,
      TRUE ~ NA_character_))

  matched_data <- ungroup(matched_data)

  return(matched_data)
}

#...............................................................................
# saveRDS(matched_data, "./data/cleaned/matched_data.RDS")
