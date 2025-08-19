# little helper functions

get_column_labels <- function() {
  col_labels <- list("agegroup" =  "Age",
                     "sex" = "Sex",
                     "children_feeding" = "Dependent children",
                     "role" = "Role",
                     "governorate" = "Governorate",
                     #
                     "record_is_followup" = "Participants in follow-up",
                     #
                     "bmi_category_daily" = "Current BMI",
                     "bmi_category_prewar" = "Pre-war BMI",
                     "weight_percent_change_prewar" = "% change in weight from autumn 2023"
  )
return(col_labels)
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
