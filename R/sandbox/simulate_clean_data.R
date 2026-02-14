#...............................................................................
### SIMULATE CLEANED DATA FROM DATA_CLEANING.R OUTPUT
#...............................................................................
# This script creates simulated data matching the structure of cleaned data
# from 1-data_cleaning.R, using empirical means and SDs where possible

library(dplyr)
library(tidyr)
library(lubridate)
library(forcats)

# Load real data to extract parameters ----------------------------------------
source(here::here("R/data-pipeline/0-data-dictionary.R"))
source(here::here("R/data-pipeline/1-data_cleaning.R"))

base_data <- readRDS(here::here("data/processed/df_base.RDS"))
fup_data <- readRDS(here::here("data/processed/df_fup.RDS"))
data_dictionary <- set_data_dictionary()
real_data <- clean_data(base_data, fup_data, data_dictionary)

# Extract empirical parameters -------------------------------------------------
cat("Extracting parameters from real data...\n")

# Get only included observations
real_included <- real_data %>% filter(anomaly == "included")

# NUMERIC PARAMETERS
numeric_params <- list(
  # Heights by sex
  height_male_mean = mean(real_included$height[real_included$sex == "male"], na.rm = TRUE),
  height_male_sd = sd(real_included$height[real_included$sex == "male"], na.rm = TRUE),
  height_female_mean = mean(real_included$height[real_included$sex == "female"], na.rm = TRUE),
  height_female_sd = sd(real_included$height[real_included$sex == "female"], na.rm = TRUE),

  # Weights
  weight_daily_mean = mean(real_included$weight_daily, na.rm = TRUE),
  weight_daily_sd = sd(real_included$weight_daily, na.rm = TRUE),
  weight_prewar_mean = mean(real_included$weight_prewar, na.rm = TRUE),
  weight_prewar_sd = sd(real_included$weight_prewar, na.rm = TRUE),

  # BMI
  bmi_daily_mean = mean(real_included$bmi_daily, na.rm = TRUE),
  bmi_daily_sd = sd(real_included$bmi_daily, na.rm = TRUE),
  bmi_prewar_mean = mean(real_included$bmi_prewar, na.rm = TRUE),
  bmi_prewar_sd = sd(real_included$bmi_prewar, na.rm = TRUE),

  # Weight change metrics
  weight_change_percent_prewar_mean = mean(real_included$weight_change_percent_prewar, na.rm = TRUE),
  weight_change_percent_prewar_sd = sd(real_included$weight_change_percent_prewar, na.rm = TRUE),

  # Time metrics
  days_enrolled_mean = mean(real_included$participant_cumulative_days_enrolled, na.rm = TRUE),
  days_enrolled_sd = sd(real_included$participant_cumulative_days_enrolled, na.rm = TRUE)
)

# FACTOR PROPORTIONS
get_proportions <- function(data, var) {
  prop.table(table(data[[var]], useNA = "no"))
}

factor_params <- list(
  sex = get_proportions(real_included, "sex"),
  age = get_proportions(real_included, "age"),
  children_feeding = get_proportions(real_included, "children_feeding"),
  role = get_proportions(real_included, "role"),
  governorate = get_proportions(real_included, "governorate"),
  organisation = get_proportions(real_included, "organisation"),
  bmi_category_daily = get_proportions(real_included, "bmi_category_daily"),
  bmi_category_prewar = get_proportions(real_included, "bmi_category_prewar")
)

# COHORT PARAMETERS
cohort_params <- list(
  n_participants = length(unique(real_data$id)),
  proportion_with_followup = mean(real_included$participant_in_followup, na.rm = TRUE),
  measurements_per_participant = real_data %>%
    group_by(id) %>%
    summarise(n_meas = n()) %>%
    pull(n_meas) %>%
    {list(mean = mean(.), sd = sd(.))},
  study_start_date = min(real_data$date_entry, na.rm = TRUE),
  study_end_date = max(real_data$date, na.rm = TRUE)
)

cat("\nNUMERIC PARAMETERS:\n")
print(unlist(numeric_params))
cat("\n\nFACTOR PROPORTIONS:\n")
print(factor_params)
cat("\n\nCOHORT PARAMETERS:\n")
print(unlist(cohort_params))

# Simulation function ----------------------------------------------------------

simulate_clean_data <- function(n_participants = 100,
                                seed = 123,
                                numeric_params,
                                factor_params,
                                cohort_params) {

  set.seed(seed)

  cat("\nSimulating data for", n_participants, "participants...\n")

  # 1. Simulate baseline characteristics --------------------------------------

  # Sample factor variables
  sex <- sample(names(factor_params$sex),
                n_participants,
                replace = TRUE,
                prob = factor_params$sex)

  age <- sample(names(factor_params$age),
                n_participants,
                replace = TRUE,
                prob = factor_params$age)

  children_feeding <- sample(names(factor_params$children_feeding),
                            n_participants,
                            replace = TRUE,
                            prob = factor_params$children_feeding)

  role <- sample(names(factor_params$role),
                n_participants,
                replace = TRUE,
                prob = factor_params$role)

  governorate <- sample(names(factor_params$governorate),
                       n_participants,
                       replace = TRUE,
                       prob = factor_params$governorate)

  organisation <- sample(names(factor_params$organisation),
                        n_participants,
                        replace = TRUE,
                        prob = factor_params$organisation)

  # Sample heights (sex-dependent)
  height <- ifelse(sex == "Male",
                  rnorm(n_participants,
                        numeric_params$height_male_mean,
                        numeric_params$height_male_sd),
                  rnorm(n_participants,
                        numeric_params$height_female_mean,
                        numeric_params$height_female_sd))
  height <- round(pmax(140, pmin(220, height)))  # Constrain to realistic range

  # Simulate enrollment dates
  date_entry <- sample(seq.Date(cohort_params$study_start_date,
                                cohort_params$study_end_date - 30,
                                by = "day"),
                      n_participants,
                      replace = TRUE)

  # Create baseline data frame
  baseline <- tibble(
    id = 1:n_participants,
    date_entry = date_entry,
    organisation = organisation,
    sex = sex,
    age = age,
    children_feeding = children_feeding,
    role = role,
    governorate = governorate,
    height = height
  )

  # 2. Simulate weight trajectories --------------------------------------------

  # Simulate pre-war BMI category first
  bmi_category_prewar <- sample(names(factor_params$bmi_category_prewar),
                               n_participants,
                               replace = TRUE,
                               prob = factor_params$bmi_category_prewar)

  # Generate pre-war BMI values consistent with categories
  bmi_prewar <- sapply(bmi_category_prewar, function(cat) {
    switch(cat,
           "Underweight" = runif(1, 16, 18.5),
           "Normal" = runif(1, 18.5, 25),
           "Overweight" = runif(1, 25, 30),
           "Obese" = runif(1, 30, 40))
  })

  # Calculate pre-war weight from BMI
  weight_prewar <- bmi_prewar * (height/100)^2

  # Simulate weight loss from pre-war to enrollment
  # Using empirical change statistics
  weight_loss_percent <- rnorm(n_participants,
                               numeric_params$weight_change_percent_prewar_mean,
                               numeric_params$weight_change_percent_prewar_sd)
  weight_loss_percent <- pmax(-30, pmin(5, weight_loss_percent))  # Cap extreme values

  weight_entry <- weight_prewar * (1 + weight_loss_percent/100)
  weight_entry <- pmax(30, pmin(180, weight_entry))  # Apply data quality constraints

  # 3. Simulate follow-up measurements -----------------------------------------

  # Determine who has follow-up
  has_followup <- rbinom(n_participants, 1, cohort_params$proportion_with_followup) == 1

  # Simulate number of measurements per participant
  n_measurements <- rpois(n_participants, cohort_params$measurements_per_participant$mean - 1) + 1
  n_measurements <- pmin(n_measurements, 30)  # Cap at reasonable maximum

  # For those without followup, set to 1 measurement only
  n_measurements[!has_followup] <- 1

  # Create long format data
  simulated_data <- baseline %>%
    slice(rep(1:n(), n_measurements)) %>%
    group_by(id) %>%
    mutate(
      # Generate measurement dates
      measurement_number = row_number(),
      days_since_entry = if_else(measurement_number == 1,
                                 0L,
                                 cumsum(c(0, rpois(n()-1, 7)))),  # ~weekly follow-ups
      date = date_entry + days(days_since_entry),

      # Participant timeline variables
      participant_cumulative_days_enrolled = as.integer(days_since_entry) + 1L,
      participant_cumulative_days_recorded = measurement_number,
      participant_recorded = TRUE,
      participant_in_followup = n() > 1,
      participant_timepoint = factor(if_else(measurement_number == 1,
                                            "Study entry", "Follow up")),
      last_measurement = measurement_number == max(measurement_number),
      date_last = date[last_measurement]
    ) %>%
    ungroup()

  # 4. Simulate weight and BMI trajectories ------------------------------------

  simulated_data <- simulated_data %>%
    mutate(
      # Add baseline weights
      weight_prewar = rep(weight_prewar, n_measurements),
      weight_entry = rep(weight_entry, n_measurements),

      # Simulate gradual weight change over time
      # Using a random walk with slight negative drift
      weight_daily = weight_entry + cumsum(c(0, rnorm(n()-1, mean = -0.05, sd = 0.5)))
    ) %>%
    group_by(id) %>%
    mutate(
      # Set first measurement to entry weight
      weight_daily = if_else(measurement_number == 1, weight_entry, weight_daily),

      # Calculate BMI
      bmi_daily = weight_daily / (height/100)^2,
      bmi_prewar = weight_prewar / (height/100)^2,
      bmi_entry = weight_entry / (height/100)^2,
      bmi_last = bmi_daily[last_measurement],
      weight_last = weight_daily[last_measurement],

      # Weight changes
      weight_change_unit_entry = weight_daily - weight_entry,
      weight_change_unit_prewar = weight_daily - weight_prewar,
      weight_change_percent_entry = ((weight_daily - weight_entry) / weight_entry) * 100,
      weight_change_percent_prewar = ((weight_daily - weight_prewar) / weight_prewar) * 100,
      weight_change_percent_daily_rate_entry = weight_change_percent_entry / participant_cumulative_days_enrolled,

      # BMI changes
      bmi_change_unit_entry = bmi_daily - bmi_entry,
      bmi_change_unit_prewar = bmi_daily - bmi_prewar,
      bmi_change_percent_entry = ((bmi_daily - bmi_entry) / bmi_entry) * 100,
      bmi_change_percent_prewar = ((bmi_daily - bmi_prewar) / bmi_prewar) * 100,
      bmi_change_percent_daily_rate_entry = bmi_change_percent_entry / participant_cumulative_days_enrolled,

      # Previous measurement changes
      participant_days_since_previousmeasurement = as.integer(date - lag(date)),
      weight_change_percent_previousmeasurement = ((weight_daily - lag(weight_daily)) / lag(weight_daily)) * 100,
      bmi_change_percent_previousmeasurement = ((bmi_daily - lag(bmi_daily)) / lag(bmi_daily)) * 100
    ) %>%
    # Set change variables to NA for first measurement
    mutate(across(matches("(.*)_change_(.*)_entry"),
                 ~ if_else(measurement_number == 1, NA_real_, .x))) %>%
    ungroup()

  # 5. Add BMI categories and anomaly classification ---------------------------

  simulated_data <- simulated_data %>%
    mutate(
      bmi_category_prewar = case_when(
        bmi_prewar < 18.5 ~ "Underweight",
        bmi_prewar >= 18.5 & bmi_prewar < 25 ~ "Normal",
        bmi_prewar >= 25 & bmi_prewar < 30 ~ "Overweight",
        bmi_prewar >= 30 ~ "Obese"
      ),
      bmi_category_daily = case_when(
        bmi_daily < 18.5 ~ "Underweight",
        bmi_daily >= 18.5 & bmi_daily < 25 ~ "Normal",
        bmi_daily >= 25 & bmi_daily < 30 ~ "Overweight",
        bmi_daily >= 30 ~ "Obese"
      ),

      # Anomaly classification
      anomaly = case_when(
        is.na(weight_daily) ~ "missing",
        !between(weight_daily, 30, 180) ~ "excluded_weight",
        !between(bmi_daily, 10, 60) ~ "excluded_bmi",
        abs(weight_change_percent_daily_rate_entry) >= 10 ~ "excluded_rate",
        TRUE ~ "included"
      )
    )

  # 6. Convert to factors using data dictionary --------------------------------

  simulated_data <- simulated_data %>%
    mutate(across(any_of(data_dictionary$factor_cols),
                 ~ factor(.)))

  # 7. Order columns as in real data -------------------------------------------

  simulated_data <- simulated_data %>%
    select(date, id, date_entry, date_last,
           anomaly, last_measurement,
           starts_with("participant"),
           organisation, sex, age, children_feeding, role, governorate,
           height,
           starts_with("weight"),
           starts_with("bmi"),
           everything(),
           -measurement_number, -days_since_entry)  # Remove helper variables

  cat("Simulation complete!\n")
  cat("Generated", nrow(simulated_data), "observations for", n_participants, "participants\n")
  cat("Proportion with follow-up:", mean(simulated_data$participant_in_followup), "\n")

  return(simulated_data)
}

# Run simulation ---------------------------------------------------------------

# Example: simulate 100 participants
sim_data <- simulate_clean_data(
  n_participants = 300,
  seed = 123,
  numeric_params = numeric_params,
  factor_params = factor_params,
  cohort_params = cohort_params
)

# Save simulated data
saveRDS(sim_data, here::here("data/public/simulated_clean_data.RDS"))
