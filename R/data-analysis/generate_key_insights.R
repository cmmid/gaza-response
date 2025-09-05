#...............................................................................
### Generate list of key insights
#...............................................................................
pacman::p_load(dplyr, tidyr, tibble, purrr, forcats)

generate_key_insights <- function(key_insights_data) {

  bmi_category <- key_insights_data |>
    filter(!is.na(bmi_category)) |>
    expand(bmi_period, bmi_category,
           organisation, strata, cohort_id_recorded) |>
    mutate(stat = "count",
           value = 0,
           variable = paste0(bmi_period, "_", bmi_category)) |>
    filter(!(variable %in% key_insights_data$variable))

  summary_text_tab <- key_insights_data |>
    bind_rows(bmi_category) |>
    mutate(value = if_else(grepl("bmi_category_", variable),
                           round(value / cohort_id_recorded * 100),
                           value),
           stat = if_else(grepl("bmi_category_", variable), "percent", stat),
           unit = case_when(
             grepl("weight_daily|weight_change_unit_", variable) ~ "kg",
             grepl("bmi_daily|bmi_change_unit_", variable) ~ "kg/m2",
             grepl("change_percent_daily_rate", variable) ~ "%/day",
             grepl("change_percent", variable) ~ "%",
             grepl("bmi_category_", variable) ~ "%",
           )) |>
    pivot_wider(names_from = stat, values_from = value) |>
    mutate(across(c(median, q1, q3, percent), ~ round(.))) |>
    mutate(value_text = case_when(
      !is.na(median) ~ paste0(median, unit, " (", q1, unit, " to ", q3, unit, ")"),
      !is.na(percent) ~ paste0(percent, unit)))

  summary_text <- summary_text_tab |>
    dplyr::select(variable, value_text) |>
    deframe()

  key_text <- list(
    # Number of ppts
    n_id = key_insights_data$cohort_id_recorded[1],
    n_followup = key_insights_data$cohort_id_followup_ever[2],
    # summary of change measurements
    weight_change_percent_prewar = summary_text |>  getElement("weight_change_percent_prewar"),
    weight_change_unit_prewar = summary_text |>  getElement("weight_change_unit_prewar"),
    bmi_change_percent_prewar = summary_text |>  getElement("bmi_change_percent_prewar"),
    bmi_change_unit_prewar = summary_text |>  getElement("bmi_change_unit_prewar"),
    bmi_category_daily_Underweight = summary_text |>  getElement("bmi_category_daily_Underweight"),
    bmi_category_prewar_Underweight = summary_text |>  getElement("bmi_category_prewar_Underweight")
  )

  return(key_text)
}
