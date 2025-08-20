#...............................................................................
### Making the data dictionary
### Structured as a named list which gets stored as an R object on the public repo
### and can be used to properly factor the data when rendering the dashboard
#...............................................................................
library(forcats)
library(dplyr)

### Set up factor levels ----------------------------------------------------
factor_levels <- list(
  sex = c("Male" = "male",
          "Female" = "female",
          "Other" = "other/prefer not to answer"),
  role = c("Staff expatriate" = "expatriate",
           "Staff national" = "national staff member",
           "Staff contractor" = "consultant or contractor",
           "Casual worker" = "casual staff/daily worker",
           "Other" = "other",
           "Other" = "prefer not to answer"),
  governorate = c("North Gaza",
                  "Gaza City",
                  "Deir Al Balah",
                  "Khan Yunis",
                  "Rafah"),
  organisation = c("UNRWA",
                   "Save the Children International"),
  age = c("Age under 30",
               "Age 30-45",
               "Age over 45",
               "anomaly"),
  children_feeding = c("0", "1", "2", "3+", "anomaly"),
  bmi_category_daily = c("anomaly", "Underweight",
                         "Normal", "Overweight", "Obese"),
  bmi_category_prewar = c("anomaly", "Underweight",
                         "Normal", "Overweight", "Obese"),
  weight_anomaly = c("anomaly", "valid"), # <30 or >180kg
  bmi_anomaly = c("anomaly", "valid") # <10 or >60 BMI
)

saveRDS(factor_levels, here::here("data", "data_dictionary.RDS"))
