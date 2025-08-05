#...............................................................................
### Making the data dictionary
### Structured as a named list which gets stored as an R object on the public repo
### and can be used to properly factor the data when rendering the dashboard
#...............................................................................

data_dict <- list()
data_dict[["sex"]] <- c("male", "female", "other/prefer not to answer")
data_dict[["governorate"]] <- c("North Gaza", "Gaza City", "Deir Al Balah", "Khan Yunis", "Rafah")
data_dict[["organisation"]] <- c("Save the Children International", "MSF-OCBA", "UNRWA")
data_dict[["agegroup"]] <- c("Under 30 years", "30-44 years", "Over 45 years")
data_dict[["category"]] <- c("Underweight", "Normal", "Overweight", "Obese")
data_dict[["role"]] <- c("expatriate", "national staff member", "consultant or contractor", "casual staff/daily worker", "other", "prefer not to answer")

saveRDS(data_dict, "./data/data_dictionary.RDS")

