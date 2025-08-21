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
    mutate(weight = if_else(date == date_entry & is.na(weight_followup),
                            weight_entry, weight_followup)) |>
  # replace incorrect with clean weights
    left_join(clean_weight_entry, by = c("date", "id")) |>
    mutate(weight = if_else(!is.na(clean_weight_entry),
                            clean_weight_entry, weight),
           anomaly = if_else(date == date_entry &
                                      !is.na(weight_followup) &
                                      weight_followup != clean_weight_entry &
                                      weight_followup != weight_prewar,
                                    "Included (note: conflicting double record at study entry)", NA))

# Dates & cohort time -----------------------------------------------------
  observed_data <- observed_data |>
    dplyr::group_by(id) |>
    # time in cohort
    dplyr::mutate(
      participant_recorded = !is.na(weight),
      participant_cumulative_days_enrolled = 1 + as.integer(
                    difftime(date, date_entry, units = "days")),
      participant_cumulative_days_recorded = cumsum(!is.na(weight)),
      participant_in_followup = any(participant_cumulative_days_recorded > 1),
      participant_timepoint = fct(if_else(date == date_entry,
                                          "Study entry", "Follow up"))) |>
    # exclude participants with no weight measurement
    filter(participant_cumulative_days_recorded > 0) |>
    # add latest measure as separate variable
    mutate(last_measurement = participant_cumulative_days_recorded == max(
      participant_cumulative_days_recorded, na.rm = TRUE) & participant_recorded,
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
      first_bmi_measurement = bmi[date == date_entry],
      last_bmi_measurement = bmi[date == date_last_measurement],
      # change since study entry
      weight_percent_change_firstmeasurement = ((weight - weight_entry)/
                                                  weight_entry)*100,
      bmi_percent_change_firstmeasurement = ((bmi - first_bmi_measurement)/
                                               first_bmi_measurement)*100,
      # daily rate of change since study entry
      bmi_rate_change_daily = (bmi - first_bmi_measurement) /
        participant_cumulative_days_enrolled,
      weight_rate_change_daily = (weight - weight_entry) /
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
           ~ ifelse(date == date_entry, NA, .x)))

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
                  bmi_percent_change_previousmeasurement = ((bmi - previous_bmi) /
                                                              previous_bmi)*100) |>
    dplyr::select(id, date,
                  days_since_previousmeasurement,
                  weight_percent_change_previousmeasurement,
                  bmi_percent_change_previousmeasurement) |>
    ungroup()

 observed_data <- left_join(observed_data,
                            change_from_previous, by = c("id", "date"))
 observed_data <- ungroup(observed_data)

 # Drop full grid of ID/date combinations, keep only observed records
 observed_data <- observed_data |>
   filter(participant_recorded) |>
   # add weight anomaly criteria
   mutate(
     anomaly = as_factor(
       case_when(
         is.na(weight) ~ "Missing",
         !between(weight, 30, 180) ~ "Excluded (weight <30kg or >180kg)",
         !between(bmi, 10, 60) ~ "Excluded (BMI <10 or >60)",
         weight_rate_change_daily >= 10 ~ "Excluded (>10% daily rate of weight change since entry)",
         !is.na(anomaly) ~ anomaly,
         TRUE ~ "Included")
     )
   ) |>
   # Replace anomaly measurements as missing
   mutate(across(contains(c("weight", "bmi")),
                 ~ if_else(anomaly != "Included",
                           NA, .x)))

 # Recode factors
 observed_data <- observed_data |>
   mutate(bmi_category_daily = bmi,
          bmi_category_prewar = bmi_prewar)
 observed_data <- set_factors(df = observed_data,
                              factor_levels = c(data_dictionary$data_levels))

 # Drop empty levels of organisation
 observed_data <- observed_data |>
   mutate(organisation = fct_drop(organisation))

 return(observed_data)
}
