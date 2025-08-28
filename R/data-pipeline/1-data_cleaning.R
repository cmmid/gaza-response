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
  dplyr,
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
# source(here("R/data-pipeline/0-data-dictionary.R")) # get helper fn for factors
# data_dictionary <- readRDS(here("data", "data-dictionary.RDS"))
# base_data <- readRDS(here("data", "processed", "df_base.RDS"))
# fup_data <- readRDS(here("data", "processed", "df_fup.RDS"))
# data <- clean_data(base_data, fup_data, data_dictionary)

#...............................................................................
### Read in and match data
#...............................................................................

clean_data <- function(base_data, fup_data, data_dictionary) {

  # parse dates in raw data
  base_data <- base_data |>
    dplyr::mutate(date = parse_date_time(date, orders = c("ymd", "dmy")))
  fup_data <- fup_data |>
    dplyr::mutate(date = parse_date_time(date, orders = c("ymd", "dmy")))

  # Create complete grid of all possible participant dates
  id_date_grid <- expand_grid(
    date = seq.Date(min(c(as.Date(base_data$date),
                          as.Date(fup_data$date)), na.rm = TRUE),
                    max(as.Date(fup_data$date), na.rm = TRUE),
                    by = "day"),
    id = unique(c(base_data$id, fup_data$id)))

  # add follow up weight measurements to complete grid
  id_date_grid <- id_date_grid |>
    left_join(fup_data, by = c("id", "date")) |>
    rename("weight_followup" = weight)

  # add baseline characteristics and weight reading to complete grid
  # - relabel baseline weight measurement
  base_weight <- base_data |>
    rename("date_entry" = date, "weight_entry" = weight)
  # - add to grid as new variables
  id_date_grid <- left_join(id_date_grid,
                            base_weight,
                            by = c("id"))

  # remove records from before enrolment
  id_date_grid <- id_date_grid |>
    filter(date >= date_entry)

  # error catching -------------------------------------------------------
  # weight recorded in follow up survey on same date as baseline survey
  double_records <- id_date_grid |>
    dplyr::select(id, date_followup = date, date_entry,
           weight_followup, weight_entry, weight_prewar) |>
    filter(date_followup == date_entry &
             !is.na(weight_followup))

  # --- correct entry weights
  clean_weight_entry <- double_records |>
    mutate(
      clean_weight_entry = case_when(
        # double record, missing entry weight ~ use followup wt
        is.na(weight_entry) ~ weight_followup,
        # double record, mismatch, and entry weight unlikely ~ use followup wt
        weight_entry != weight_followup &
          weight_entry == weight_prewar ~ weight_followup,
        .default = weight_entry)) |>
    dplyr::select(date = date_entry, id, clean_weight_entry)

  # validated joined dataset --------------------------
  observed_data <- id_date_grid |>
    # set first day's weight to baseline measurement
    mutate(weight_daily = if_else(date == date_entry & is.na(weight_followup),
                            weight_entry, weight_followup)) |>
  # replace incorrect with clean weights
    left_join(clean_weight_entry, by = c("date", "id")) |>
    mutate(weight_daily = if_else(!is.na(clean_weight_entry),
                            clean_weight_entry, weight_daily),
           anomaly = if_else(date == date_entry &
                                      !is.na(weight_followup) &
                                      weight_followup != clean_weight_entry &
                                      weight_followup != weight_prewar,
                                    "excluded_conflict", NA)) |>
    dplyr::select(-c(weight_followup, clean_weight_entry))

# Dates & cohort time -----------------------------------------------------
  observed_data <- observed_data |>
    group_by(id) |>
    # time in cohort
    mutate(
      participant_recorded = !is.na(weight_daily),
      participant_cumulative_days_enrolled = 1 + as.integer(
                    difftime(date, date_entry, units = "days")),
      participant_cumulative_days_recorded = cumsum(!is.na(weight_daily)),
      participant_in_followup = any(participant_cumulative_days_recorded > 1),
      participant_timepoint = fct(if_else(date == date_entry,
                                          "Study entry", "Follow up"))) |>
    # exclude participants with no weight measurement
    filter(participant_cumulative_days_recorded > 0) |>
    # add latest measure as separate variable
    mutate(last_measurement = participant_cumulative_days_recorded == max(
      participant_cumulative_days_recorded, na.rm = TRUE) & participant_recorded,
           date_last = date[which(last_measurement)],
           weight_last = weight_daily[which(last_measurement)])

  #...............................................................................
  ### Add BMI and % wt change
  #...............................................................................

  observed_data <- observed_data |>
    group_by(id) |>
    dplyr::mutate(
      # BMI
      bmi_daily = weight_daily / (height/100)^2,
      bmi_prewar = if_else(!is.na(weight_daily),
                           weight_prewar / (height/100)^2,
                           NA),
      bmi_entry = bmi_daily[date == date_entry],
      bmi_last = bmi_daily[date == date_last],
      # unit change since study entry
      weight_change_unit_entry = weight_daily - weight_entry,
      bmi_change_unit_entry = bmi_daily - bmi_entry,
      # percent change since study entry
      weight_change_percent_entry = ((weight_daily - weight_entry) / weight_entry)*100,
      bmi_change_percent_entry = ((bmi_daily - bmi_entry) / bmi_entry)*100,
      # percent daily rate of change since study entry
      weight_change_percent_daily_rate_entry = weight_change_percent_entry /
        participant_cumulative_days_enrolled,
      bmi_change_percent_daily_rate_entry = bmi_change_percent_entry /
        participant_cumulative_days_enrolled,
      # change since prewar
      weight_change_unit_prewar = weight_daily - weight_prewar,
      bmi_change_unit_prewar = bmi_daily - bmi_prewar,
      weight_change_percent_prewar = ((weight_daily - weight_prewar) /
                                        weight_prewar)*100,
      bmi_change_percent_prewar = ((bmi_daily - bmi_prewar) /
                                     bmi_prewar)*100) |>
    ungroup() |>
    # drop 0 percent change on date of first measurement
    mutate(across(matches("(.*)_change_(.*)_entry"),
           ~ ifelse(date == date_entry, NA, .x)))

 change_from_previous <- observed_data %>%
    dplyr::arrange(id, date) %>%
    dplyr::group_by(id) %>%
    dplyr::filter(!is.na(weight_daily)) %>%
    dplyr::mutate(previous_weight = lag(weight_daily),
                  previous_bmi = lag(bmi_daily),
                  participant_days_since_previousmeasurement = as.integer(
                    difftime(date, lag(date), units = "days")),
                  weight_change_percent_previousmeasurement = ((weight_daily - previous_weight) /
                                                                 previous_weight)*100,
                  bmi_change_percent_previousmeasurement = ((bmi_daily - previous_bmi) /
                                                              previous_bmi)*100) |>
    dplyr::select(id, date,
                  participant_days_since_previousmeasurement,
                  weight_change_percent_previousmeasurement,
                  bmi_change_percent_previousmeasurement) |>
    ungroup()

 observed_data <- left_join(observed_data,
                            change_from_previous, by = c("id", "date"))
 observed_data <- ungroup(observed_data)

 # Drop full grid of ID/date combinations, keep only observed records
 observed_data <- observed_data |>
   filter(participant_recorded)

 # add weight anomaly criteria
 observed_data <- observed_data |>
   mutate(
     anomaly = case_when(
         is.na(weight_daily) ~ "missing",
         !between(weight_daily, 30, 180) ~ "excluded_weight",
         !between(bmi_daily, 10, 60) ~ "excluded_bmi",
         weight_change_percent_daily_rate_entry >= 10 ~ "excluded_rate",
         !is.na(anomaly) ~ anomaly,
         TRUE ~ "included")
   ) |>
   # Replace anomaly measurements as missing
   mutate(across(contains(c("weight", "bmi")),
                 ~ if_else(anomaly != "included",
                           NA, .x)))

 # Handle factors ----------------------------------------------------------
 observed_data <- observed_data |>
   mutate(age = case_when(
     age < 16 ~ NA,
     age < 30 ~ "Age under 30",
     age >= 30 & age <= 45 ~ "Age 30-45",
     age > 45 ~ "Age over 45",
     age >= 99 ~ NA,
     .default = NA
   ))

 observed_data <- observed_data |>
   mutate(children_feeding = case_when(
       children_feeding < 0 ~ NA,
       children_feeding == 0 ~ "0",
       children_feeding == 1 ~ "1",
       children_feeding == 2 ~ "2",
       children_feeding >= 3 & children_feeding <= 20 ~ "3+",
       children_feeding >= 20 ~ NA,
       .default = NA
     ))

 observed_data <- observed_data |>
   mutate(bmi_category_daily = bmi_daily,
          bmi_category_prewar = bmi_prewar) |>
   mutate(across(starts_with("bmi_category"),
          ~ case_when(
            .x <= 10 ~ NA,
            .x < 18.5 ~ "Underweight",
            .x >= 18.5 & .x < 25 ~ "Normal",
            .x >= 25 & .x < 30 ~ "Overweight",
            .x >= 30 ~ "Obese",
            .x >= 60 ~ NA,
            .default = NA
          )))


 # Convert to factor
 suppressWarnings(
   clean_data <- observed_data |>
     mutate(across(any_of(data_dictionary$factor_cols),
                   ~ as_factor(.))) |> # preserves unexpected values
     mutate(across(any_of(data_dictionary$factor_cols),
                   ~ fct_recode(., !!!data_dictionary$data_levels)))
 )

 # neaten the df
 clean_data <- clean_data |>
   dplyr::select(date, id, date_entry, date_last,
          anomaly, last_measurement,
          starts_with("participant"),
          any_of(data_dictionary$factor_cols),
          height,
          starts_with("weight"),
          starts_with("bmi"),
          everything()
          )

 return(clean_data)
}
