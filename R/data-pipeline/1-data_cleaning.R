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
    rename("date_enrol" = date,
           "weight_enrol" = weight)
  # - add to grid as new variables
  fup_data_expanded <- left_join(id_date_grid,
                            base_weight,
                            by = c("id"))

  # use baseline weight reading as first day's weight
  fup_data_expanded <- fup_data_expanded |>
    mutate(weight_latest = ifelse(date == enrol_date,
                           weight_enrol, weight_latest)) |>
    # remove records from before enrolment
    filter(date >= enrol_date)


# Dates & cohort time -----------------------------------------------------
  matched_data <- fup_data_expanded |>
    dplyr::group_by(id) |>
    # time in cohort
    dplyr::mutate(participant_cumulative_days_enrolled = 1 + as.integer(
                    difftime(date, date_enrol, units = "days")),
                  participant_cumulative_days_recorded = cumsum(!is.na(weight_latest)),
                  is_last_measurement = participant_cumulative_days_recorded ==
                    max(participant_cumulative_days_recorded, na.rm = TRUE) &
                    !is.na(weight_latest))

  #...............................................................................
  ### Add BMI and % wt change
  #...............................................................................

  matched_data <- matched_data |>
    group_by(id) |>
    dplyr::mutate(
      # calculate BMI
      bmi_latest = weight_latest / (height/100)^2,
      bmi_prewar = weight_prewar / (height/100)^2,
      bmi_enrol = bmi_latest[date == date_enrol],
      # TODO add bmi categories here for tidiness
      # calculate change over time
      bmi_latest_enrol_change_abs = bmi_latest - bmi_enrol,
      bmi_latest_enrol_change_percent = ((bmi_latest - bmi_enrol) /
                                               bmi_enrol)*100,
      bmi_latest_prewar_change_abs = bmi_latest - bmi_prewar,
      bmi_latest_prewar_change_percent = ((bmi_latest - bmi_prewar) /
                                            bmi_prewar)*100,
      weight_latest_enrol_change_abs = weight_latest - weight_enrol_measurement,
      weight_latest_enrol_change_percent = ((weight_latest - weight_enrol) /
                                              weight_enrol)*100,
      weight_latest_prewar_change_abs = weight_latest - weight_prewar,
      weight_latest_prewar_change_percent = ((weight_latest - weight_prewar) /
                                               weight_prewar)*100
      ) |>
    ungroup() |>
    # drop 0 percent change on date of first measurement
    mutate(across(matches("_change_"),
           ~ ifelse(date == date_enrol, NA, .x)))

 change_from_previous <- matched_data %>%
    dplyr::arrange(id, date) %>%
    dplyr::group_by(id) %>%
    dplyr::filter(!is.na(weight_latest)) %>%
    dplyr::mutate(weight_previous = lag(weight_latest),
                  bmi_previous = lag(bmi_latest),
                  weight_latest_previous_change_percent = ((weight_latest - weight_previous) /
                                                                 weight_previous)*100,
                  bmi_latest_previous_change_percent = ((bmi_latest - bmi_previous) /
                                                          bmi_previous)*100) %>%
    dplyr::select(id, date,
                  weight_latest_previous_change_percent,
                  bmi_latest_previous_change_percent) |>
    ungroup()

  matched_data <- left_join(matched_data,
                            change_from_previous, by = c("id", "date"))

  # Data quality checks -----------------------------------------------------
  # Flag anomalous data
  matched_data <- matched_data |>
    mutate(
      weight_anomaly = case_when(
        !between(bmi_latest, 10, 60) ~ TRUE,
        abs(weight_latest_previous_change_percent) >= 10 ~ TRUE,
        TRUE ~ FALSE)
    )
  # Set anomalous data to NA
  matched_data <- matched_data |>
    mutate(across(matches("weight_latest|bmi_latest"),
                  ~ ifelse(weight_anomaly, NA, .x)))


  # Specify factor variables ------------------------------------------------
  # TODO use data dictionary here
  matched_data <- matched_data |>
    mutate(agegroup = ifelse(age < 30, "Under 30 years",
                             ifelse(age < 45, "30-44 years", "Over 45 years")))
  # BMI categories
  matched_data <- matched_data |>
    mutate(
      bmi_enrol_category = case_when(
        bmi_enrol <= 10 ~ NA_character_,
        bmi_enrol < 18.5 ~ "underweight",
        bmi_enrol >= 18.5 & bmi_enrol < 25 ~ "normal",
        bmi_enrol >= 25 & bmi_enrol < 30 ~ "overweight",
        bmi_enrol >= 30 ~ "obese",
        bmi_enrol >= 60 ~ NA_character_,
        TRUE ~ NA_character_),
      bmi_latest_category = case_when(
        bmi_latest <= 10 ~ NA_character_,
        bmi_latest < 18.5 ~ "underweight",
        bmi_latest >= 18.5 & bmi_latest < 25 ~ "normal",
        bmi_latest >= 25 & bmi_latest < 30 ~ "overweight",
        bmi_latest >= 30 ~ "obese",
        bmi_latest >= 60 ~ NA_character_,
        TRUE ~ NA_character_),
      bmi_prewar_category = case_when(
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

  matched_data <- ungroup(matched_data)

  return(matched_data)
}

#...............................................................................
