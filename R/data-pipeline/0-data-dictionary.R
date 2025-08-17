#...............................................................................
### Making the data dictionary
### Structured as a named list which gets stored as an R object on the public repo
### and can be used to properly factor the data when rendering the dashboard
#...............................................................................

library(forcats)
library(dplyr)

# Set levels for character factors in the raw data --------------
get_factor_character <- function() {
  factor_levels_raw <- list(
    sex = c(
      "Female" = "female",
      "Male" = "male",
      "Other" = "other/prefer not to answer"),
    governorate = c(
      "North Gaza",
      "Gaza City",
      "Deir Al Balah",
      "Khan Yunis",
      "Rafah"),
    organisation = c(
      "Overall" = "all",
      "UNRWA" = "UNRWA",
      "Save the Children International" = "Save the Children International",
      "MSF-OCBA" = "MSF-OCBA"),
    role = c("Staff: expatriate" = "expatriate",
             "Staff: national" = "national staff member",
             "Contractor" = "consultant or contractor",
             "Casual/daily" = "casual staff/daily worker",
             "Other" = "other",
             "Other" = "prefer not to answer"))
  names(factor_levels_raw$governorate) <- factor_levels_raw$governorate
  return(factor_levels_raw)
}

# Set levels for created variable ------------------------
get_factor_created_variable <- function() {
  # Tidy names for created variable
  created_variable <- c(
    "weight" = "Weight, kg",
    "weight_percent_change_firstmeasurement" = "% change in weight from first measurement",
    "weight_percent_change_prewar" = "% change in weight since Autumn 2023",
    "bmi" = "BMI",
    "bmi_percent_change_firstmeasurement" = "% change in BMI from first measurement",
    "bmi_percent_change_prewar" = "% change in BMI since Autumn 2023")
  return(created_variable)
}

# Set factors in a dataframe ----------------------------------------------
recode_factor_variable <- function(variable) {

  factor_character <- get_factor_character()
  factor_created_variable <- get_factor_created_variable()

  if (any(variable %in% names(factor_character))) {
    variable <- forcats::fct_recode(variable,
                                    levels = !!!factor_character[[variable]])
  } else if (variable == "children_feeding") {
    variable <- case_when(
      variable == 999 ~ NA_character_,
      variable == 0 ~ "0",
      variable == 1 ~ "1",
      variable == 2 ~ "2",
      variable >= 3 ~ "3+",
      .default = NA_character_)
  } else if (variable == "agegroup") {
    variable <- case_when(
      variable == 999 ~ NA_character_,
      variable <= 30 ~ "Age under 30",
      between(variable, 30, 44) ~ "Age 30-44",
      variable >= 45 ~ "Age 45+",
      .default = NA_character_)
  } else if (grepl("bmi", variable) & is.numeric(variable)) {
    variable <- case_when(
      variable <= 10 ~ NA_character_,
      variable < 18.5 ~ "Underweight",
      variable >= 18.5 & variable < 25 ~ "Normal",
      variable >= 25 & variable < 30 ~ "Overweight",
      variable >= 30 ~ "Obese",
      variable >= 60 ~ NA_character_,
      TRUE ~ NA_character_)
  }
  return(variable)
}


# test <- data[["all"]][["children_feeding-role"]] |>
#   mutate(role = forcats::fct_recode(role,
#                                 levels = factor_character$role))
