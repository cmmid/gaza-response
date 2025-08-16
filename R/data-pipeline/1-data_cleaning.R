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
    dplyr::mutate(participant_cumulative_days_enrolled = 1 + as.integer(
                    difftime(date, date_first_measurement, units = "days")),
                  participant_cumulative_days_recorded = cumsum(!is.na(weight))
                  ) |>
    # exclude participants with no weight measurement
    filter(participant_cumulative_days_recorded > 0) |>
    # add latest measure as separate variable
    mutate(latest_measurement = participant_cumulative_days_recorded == max(participant_cumulative_days_recorded, na.rm = TRUE) & !is.na(weight),
           date_latest_measurement = date[which(latest_measurement)],
           weight_latest_mesurement = weight[which(latest_measurement)])

  #...............................................................................
  ### Add BMI and % wt change
  #...............................................................................

  observed_data <- observed_data |>
    group_by(id) |>
    dplyr::mutate(
      # BMI
      bmi = weight / (height/100)^2,
      bmi_prewar = weight_prewar / (height/100)^2,
      first_bmi_measurement = bmi[date == date_first_measurement],
        # TODO add bmi categories here for tidiness
      # calculate change since enrolment
      weight_percent_change_firstmeasurement = ((weight - first_weight_measurement)/
                                                  first_weight_measurement)*100,
      bmi_percent_change_firstmeasurement = ((bmi - first_bmi_measurement)/
                                               first_bmi_measurement)*100,
      # change since prewar
      weight_percent_change_prewar = ((weight - weight_prewar)/
                                        weight_prewar)*100,
      bmi_percent_change_prewar = ((bmi - bmi_prewar)/
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
                  weight_percent_change_previousmeasurement = ((weight - previous_weight) /
                                                                 previous_weight)*100,
                  bmi_percent_change_previousmeasurement = ((bmi - previous_bmi) / previous_bmi)*100) %>%
    dplyr::select(id, date,
                  weight_percent_change_previousmeasurement,
                  bmi_percent_change_previousmeasurement) |>
    ungroup()

 observed_data <- left_join(observed_data,
                            change_from_previous, by = c("id", "date"))

  # Data quality checks -----------------------------------------------------
  # Flag anomalous data
  observed_data <- observed_data |>
    mutate(
      weight_anomaly = case_when(
        !between(bmi, 10, 60) ~ TRUE,
        abs(weight_percent_change_previousmeasurement) >= 10 ~ TRUE,
        TRUE ~ FALSE)
    )
  # Set anomalous data to NA
  observed_data <- observed_data |>
    mutate(across(contains(c("weight", "bmi")),
                  ~ ifelse(weight_anomaly, NA, .x)))


  # Specify factor variables ------------------------------------------------
  # TODO use data dictionary here
  observed_data <- observed_data |>
    mutate(agegroup = ifelse(age < 30, "Under 30 years",
                             ifelse(age < 45, "30-44 years", "Over 45 years")))
  # BMI categories
  observed_data <- observed_data |>
    mutate(
      bmi_category = case_when(
        bmi <= 10 ~ NA_character_,
        bmi < 18.5 ~ "underweight",
        bmi >= 18.5 & bmi < 25 ~ "normal",
        bmi >= 25 & bmi < 30 ~ "overweight",
        bmi >= 30 ~ "obese",
        bmi >= 60 ~ NA_character_,
        TRUE ~ NA_character_),
      bmi_category_prewar = case_when(
        bmi_prewar <= 10 ~ NA_character_,
        bmi_prewar < 18.5 ~ "underweight",
        bmi_prewar >= 18.5 & bmi_prewar < 25 ~ "normal",
        bmi_prewar >= 25 & bmi_prewar < 30 ~ "overweight",
        bmi_prewar >= 30 ~ "obese",
        bmi_prewar >= 60 ~ NA_character_,
        TRUE ~ NA_character_)
      )

  # add "overall" variable for total-cohort summaries
  observed_data <- observed_data |>
    mutate(overall = "overall")

  observed_data <- ungroup(observed_data)

  return(observed_data)
}

#...............................................................................
