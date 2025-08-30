#...............................................................................
### Making the data dictionary
### Structured as a named list which gets stored as an R object on the public repo
### and can be used to properly factor the data when rendering the dashboard
#...............................................................................
# examples:
# df$variable <- forcats::fct_recode(df$variable,
#                                   !!!data_dictionary$variable_names))

library(forcats)
library(dplyr)

set_data_dictionary <- function() {

  dictionary <- list(
    factor_cols = c("overall", "organisation",
                    "sex", "age", "children_feeding", "role", "governorate",
                    "bmi_category_daily", "bmi_category_prewar",
                    "strata", "stratum", "variable_name", "anomaly"),
    data_levels = as_factor(c(
  # Categories within data -------------------------
    # sex
    "Male" = "male",
    "Female" = "female",
    "Other" = "other/prefer not to answer",
    # age
    "Age under 30" = "Age under 30",
    "Age 30-45" = "Age 30-45",
    "Age over 45" = "Age over 45",
    "Missing" = "anomaly",
    # dependants
    "0" = "0",
    "1" = "1",
    "2" = "2",
    "3+" = "3+",
    "Missing" = "NA",
    # role
    "Staff expatriate" = "expatriate",
    "Staff national" = "national staff member",
    "Staff contractor" = "consultant or contractor",
    "Casual worker" = "casual staff/daily worker",
    "Other" = "other",
    "Other" = "prefer not to answer",
    # organisation
    "UNRWA" = "UNRWA",
    "Save the Children International" = "Save the Children International",
    # governorate
    "North Gaza" = "North Gaza",
    "Gaza City" = "Gaza City",
    "Deir Al Balah" = "Deir Al Balah",
    "Khan Yunis" = "Khan Yunis",
    "Rafah" = "Rafah",
    # ----- created data ------------------
    # BMI categories
    "Underweight" = "Underweight",
    "Normal" = "Normal",
    "Overweight" = "Overweight",
    "Obese" = "Obese",
    "Missing" = "NA",
    # anomaly classification
    "Excluded (>10% daily rate of weight change since entry)" = "excluded_rate",
    "Excluded (BMI <10 or >60)" = "excluded_bmi",
    "Excluded (weight <30kg or >180kg)" = "excluded_weight",
    "Missing" = "missing",
    "Excluded (conflicting double record at study entry)" = "excluded_conflict",
    "Included" = "included",
    # ---------- Variable name labels -----------------------
    # strata names
    "Overall" = "overall",
    "Organisation" = "organisation",
    "Sex" = "sex",
    "Age" = "age",
    "Dependent children" = "children_feeding",
    "Staff role" = "role",
    "Governorate" = "governorate",
    # -----
    # cohort participation
    "Date" = "date",
    "Date of study entry" = "date_entry",
    "Participant timepoint" = "participant_timepoint",
    "Demographic strata" = "strata",
    "Stratum" = "stratum",
    "Number of participants" = "cohort_id_recorded",
    "Number of measurements" = "cohort_obs_recorded",
    "Number of newly enrolled participants" = "cohort_id_new",
    "Number of follow up measurements" = "cohort_id_followup_record",
    "Number of participants ever in follow up" = "cohort_id_followup_ever",
    "Number of participants missing measurements" = "cohort_obs_missing",
    "Cumulative participant days since entry" = "cohort_persondays",
    "Cumulative participant days since entry among follow-up" = "cohort_persondays_followup",
    "Participant days since study entry" = "participant_cumulative_days_enrolled",
    "Participant days with recorded weight" = "participant_cumulative_days_recorded",
    "Exclusion criteria" = "anomaly",
  # measurements
    # weight
      "Weight, kg" = "weight_daily",
      "Weight change since start of war (kg)" = "weight_change_unit_prewar",
      "Weight change since start of war (%)" = "weight_change_percent_prewar",
      "Weight change since study entry (%)" = "weight_change_percent_entry",
      "Weight daily rate of change since study entry (%/day)" = "weight_change_percent_daily_rate_entry",
    # BMI
      "BMI, kg/m²" = "bmi_daily",
      "BMI change since start of war" = "bmi_change_unit_prewar",
      "BMI change since start of war (%)" = "bmi_change_percent_prewar",
      "BMI change since enrollment (%)" = "bmi_change_percent_entry",
      "BMI daily rate of change since study entry (%/day)" = "bmi_change_percent_daily_rate_entry",
    "Current BMI" = "bmi_category_daily",
    "Pre-war BMI" = "bmi_category_prewar"
    )
    )
  )
    return(dictionary)
  }
