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

  fup_data_expanded <- fup_data %>%
      dplyr::left_join(dplyr::select(base_data, !c(date, weight)),
                       by = "id")

  full_data <- rbind(base_data, fup_data_expanded) %>%
    dplyr::arrange(id, date, desc(!is.na(weight))) %>%  # Put non-NA weights first
    # TODO remove duplicate records by ID and date - add n to quality checks
    dplyr::distinct(id, date, .keep_all = TRUE)


# Dates & cohort time -----------------------------------------------------
  full_data <- full_data |>
    dplyr::mutate(date = parse_date_time(date, orders = c("ymd", "dmy")))

  matched_data <- full_data |>
    dplyr::group_by(id) |>
    # time in cohort
    dplyr::mutate(date_first_measurement = min(date[!is.na(weight)]),
                  participant_cumulative_days_enrolled = 1 + as.integer(
                    difftime(date, date_first_measurement, units = "days")),
                  participant_cumulative_days_recorded = cumsum(!is.na(weight))) |>
    # remove records from before enrolment
    filter(participant_cumulative_days_enrolled >= 1) |>
    ungroup()

  #...............................................................................
  ### Add BMI and % wt change
  #...............................................................................

  matched_data <- matched_data |>
    group_by(id) |>
    dplyr::mutate(
      # daily absolute number
      bmi = weight / (height/100)^2,
      bmi_prewar = weight_prewar / (height/100)^2,
      # prewar absolute
      first_weight_measurement = weight[date == date_first_measurement],
      first_bmi_measurement = bmi[date == date_first_measurement],
      # change since enrolment
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

 change_from_previous <- matched_data %>%
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

  matched_data <- left_join(matched_data,
                            change_from_previous, by = c("id", "date"))

  # Specify factor variables ------------------------------------------------
  # TODO use data dictionary here
  matched_data <- matched_data |>
    mutate(agegroup = ifelse(age < 30, "Under 30 years",
                             ifelse(age < 45, "30-44 years", "Over 45 years")))
  # BMI categories
  matched_data <- matched_data |>
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
  matched_data <- matched_data |>
    mutate(overall = "overall")

  # Data quality checks -----------------------------------------------------

  # Flag anomalous data
  matched_data <- matched_data |>
    mutate(
      weight_anomaly = case_when(
        !between(bmi, 10, 60) ~ FALSE,
        weight_percent_change_previousmeasurement >= 10 ~ FALSE,
        TRUE ~ TRUE)
    )

  matched_data <- ungroup(matched_data)

  return(matched_data)
}

#...............................................................................
