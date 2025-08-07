#...............................................................................
### Making the data dictionary
### Structured as a named list which gets stored as an R object on the public repo
### and can be used to properly factor the data when rendering the dashboard
#...............................................................................

data_dict <- list()
#
data_dict[["sex"]] <- c("male", "female", "other/prefer not to answer")
names(data_dict[["sex"]]) <- c("Male", "Female", "Other/no")
#
data_dict[["governorate"]] <- c("North Gaza", "Gaza City", "Deir Al Balah", "Khan Yunis", "Rafah")
names(data_dict[["governorate"]]) <- data_dict[["governorate"]]
#
data_dict[["organisation"]] <- c("Save the Children International", "MSF-OCBA", "UNRWA")
names(data_dict[["organisation"]]) <- data_dict[["organisation"]]
#
data_dict[["agegroup"]] <- c("Under 30 years", "30-44 years", "Over 45 years")
names(data_dict[["agegroup"]]) <- c("Under 30 years", "30-44 years", "45+ years")
#
data_dict[["bmi_category"]] <- c("underweight", "normal", "overweight",
                                 "obese", "anomaly")
names(data_dict[["bmi_category"]]) <- c("Underweight", "Normal", "Overweight",
                                        "Obese", "Anomaly")
#
data_dict[["role"]] <- c("expatriate", "national staff member", "national - contracted", "national = casual worker", "other")
names(data_dict[["role"]]) <- c("Expatriate", "National staff member", "Consultant or contractor", "Casual staff/daily worker", "Other")
#
data_dict[["children_feeding"]] <- seq(0:20)
names(data_dict[["children_feeding"]]) <- c("0","1","2", rep("3+", 20-2))

saveRDS(data_dict, "./data/data_dictionary.RDS")
