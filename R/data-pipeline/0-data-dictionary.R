#...............................................................................
### Making the data dictionary
### Structured as a named list which gets stored as an R object on the public repo
### and can be used to properly factor the data when rendering the dashboard
#...............................................................................
# examples:
# df$variable <- forcats::fct_recode(df$variable,
#                                   !!!data_dictionary$variable_names))
# dictionary <- fct_unify(set_data_dictionary())

library(forcats)
library(dplyr)

set_data_dictionary <- function() {

  dictionary <- list()

  # BMI categories ---------------------------------------
  bmi_levels <- c("Underweight","Normal","Overweight","Obese", "NA")
  bmi_category <- list("Current BMI" = "bmi_category_daily",
               "Pre-war BMI" = "bmi_category_prewar")
  dictionary$bmi_category <- bmi_category

  bmi_category_labels <- paste0(rep(bmi_category, length(bmi_levels)),
                                "_", bmi_levels)
  names(bmi_category_labels) <- paste(rep(names(bmi_category), length(bmi_levels)),
                                      bmi_levels)
  dictionary$bmi_category_labels <- bmi_category_labels

  # Categories within data -------------------------
  dictionary$data_levels <- list(
    sex = c("Male" = "male",
            "Female" = "female",
            "Other" = "other/prefer not to answer"),
    age = c("Age under 30",
                        "Age 30-45",
                        "Age over 45",
                        "anomaly"),
    children_feeding = c("0", "1", "2", "3+", "anomaly"),
    role = c("Staff expatriate" = "expatriate",
             "Staff national" = "national staff member",
             "Staff contractor" = "consultant or contractor",
             "Casual worker" = "casual staff/daily worker",
             "Other" = "other",
             "Other" = "prefer not to answer"),
    organisation = c("UNRWA", "Save the Children International"),
    governorate = c("North Gaza",
                    "Gaza City",
                    "Deir Al Balah",
                    "Khan Yunis",
                    "Rafah"),
    # derived data
    bmi_category_prewar = bmi_levels,
    bmi_category_daily = bmi_levels
  )

# Variable name labels for display ---------------------------
  dictionary$variable_names <- list(
    # cohort participation
    participation = list(
    "Demographic" = "group",
    "Strata" = "label",
    "Number of participants" = "cohort_id_enrolled",
    "Number of newly enrolled participants" = "cohort_id_new",
    "Number of participants in follow up" = "cohort_id_followup",
    "Number of participants lost to follow up" = "cohort_obs_missing",
    "Number of participant measurements" = "cohort_obs_recorded",
    "Participant in follow-up" = "participant_in_followup",
    "Participant days since study entry" = "participant_cumulative_days_enrolled",
    "Participant days with recorded weight" = "participant_cumulative_days_recorded",
    "Exclusion criteria" = "anomaly"
    ),
    # demographics
    demographic = list(
    "Sex" = "sex",
    "Age" = "age",
    "Dependent children" = "children_feeding",
    "Staff role" = "role",
    "Organisation" = "organisation",
    "Governorate" = "governorate"
    ),
    measurement = list(
    # weight
      "Weight, kg" = "weight",
      "Weight change since start of war (kg)" = "weight_unit_change_prewar",
      "Weight change since start of war (%)" = "weight_percent_change_prewar",
      "Weight change since study entry (%)" = "weight_percent_change_firstmeasurement",
      "Weight daily rate of change since study entry (kg/day)" = "weight_rate_change_daily",
    # BMI
      "BMI" = "bmi",
      "BMI change since start of war" = "bmi_unit_change_prewar",
      "BMI change since start of war (%)" = "bmi_percent_change_prewar",
      "BMI change since enrollment (%)" = "bmi_percent_change_firstmeasurement",
      "BMI daily rate of change since study entry (units/day)" = "bmi_rate_change_daily",
    "Current BMI" = "bmi_category_daily",
    "Pre-war BMI" = "bmi_category_prewar"
    )
    )
  saveRDS(dictionary, file = here("data", "data-dictionary.RDS"))

    return(dictionary)
  }

# Set factors
set_factors <- function(df, factor_levels,
                        factor_cols = names(factor_levels)) {

  for (col_name in factor_cols) {
    if (col_name %in% names(df)) {

      # Special cases for numeric variables
      if (col_name == "children_feeding" & is.numeric(df[[col_name]])) {
        df[[col_name]] <- case_when(
          df[[col_name]] < 0 ~ NA,
          df[[col_name]] == 0 ~ "0",
          df[[col_name]] == 1 ~ "1",
          df[[col_name]] == 2 ~ "2",
          df[[col_name]] >= 3 & df[[col_name]] <= 20 ~ "3+",
          df[[col_name]] >= 20 ~ NA,
          .default = NA
        )
      }

      if (col_name == "age" & is.numeric(df[[col_name]])) {
        df[[col_name]] <- case_when(
          df[[col_name]] < 16 ~ NA,
          df[[col_name]] < 30 ~ "Age under 30",
          df[[col_name]] >= 30 & df[[col_name]] <= 45 ~ "Age 30-45",
          df[[col_name]] > 45 ~ "Age over 45",
          df[[col_name]] >= 99 ~ NA,
          .default = NA
        )
      }

      if (grepl("bmi_category_prewar|bmi_category_daily", col_name) &
          is.numeric(df[[col_name]])) {
        df[[col_name]] <- case_when(
          df[[col_name]] <= 10 ~ NA,
          df[[col_name]] < 18.5 ~ "Underweight",
          df[[col_name]] >= 18.5 & df[[col_name]] < 25 ~ "Normal",
          df[[col_name]] >= 25 & df[[col_name]] < 30 ~ "Overweight",
          df[[col_name]] >= 30 ~ "Obese",
          df[[col_name]] >= 60 ~ NA,
          .default = NA
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

# count factor levels
count_factors <- function(df){
  count <- df |>
    dplyr::select(where(is.factor)) |>
    map_dfr(~ fct_count(.x, prop = TRUE) |>
              add_row(f = "Missing", n = sum(is.na(.x))),
            .id = "variable")
  return(count)
}
