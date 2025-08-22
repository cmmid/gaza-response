#...............................................................................
### ++ MONITORING THE NUTRITIONAL STATUS OF HUMANITARIAN WORKERS IN GAZA +++ ###
#...............................................................................

#...............................................................................
## ------------- R SCRIPT TO GENERATE KEY INSIGHTS FROM DATA ---------------- ##
#...............................................................................

#...............................................................................
### Preparatory steps
#...............................................................................

#...............................................................................
### Generate list of key insights
#...............................................................................
# eg
# '
# A typical change in weight of `r summary_text[summary_text$variable =="weight_change_unit_prewar","median_iqr"]`kg (median, 25-75% range)
# '

generate_key_insights <- function(data) {
  summary_text_tab <- data |>
    group_by(label) |>
    pivot_wider(names_from = stat, values_from = value) |>
    dplyr::select(date, organisation, label,
                  cohort_obs_recorded, cohort_id_followup_record,
                  variable, median, q1, q3, count) |>
    mutate(across(where(is.numeric), round, 1),
           median_iqr = paste0(median, " (", q1, "-", q3, ")"),
           bmi_pc = paste0(count, " (",
                                round((count / cohort_obs_recorded*100),0), "%)"))

  key_text <- list(
    # Number of ppts (date range)
    cohort_n = max(data$cohort_id_recorded),
    # Number in follow up
    cohort_fup = max(data$cohort_id_followup_ever),
    # date range of current summary
    date_range = paste0(min(log$data_latest$date_of_latest$date),
                        " - ", max(log$data_latest$date_of_latest$date)),
    # Median weight change in kg (IQR) since pre-war
    wt_chg_iqr_kg = summary_text_tab[summary_text_tab$variable == "weight_change_unit_prewar","median_iqr"],
    # Median weight change in %
    wt_chg_iqr_pct = summary_text_tab[summary_text_tab$variable == "weight_change_percent_prewar","median_iqr"],
    # PPts in follow up, % weight change in kg (IQR) since entry
    wt_chg_iqr_kg = summary_text_tab[summary_text_tab$variable == "weight_change_unit_entry","median_iqr"],
    # Median weight change in %
    wt_chg_iqr_pct = summary_text_tab[summary_text_tab$variable == "weight_change_percent_prewar","median_iqr"],
    # % in "underweight" category
    cohort_underwt_pc = summary_text_tab[summary_text_tab$variable == "bmi_category_daily_Underweight","bmi_pc"],
    # % in "underweight" category pre-war
    cohort_underwt_pc_prewar = summary_text_tab[summary_text_tab$variable == "bmi_category_prewar_Underweight","bmi_pc"]
  )

  # average length of time in study
  # = cohort_persondays / cohort_obs_recorded

  # Daily rate of weight loss since study entry

  return(key_text)
}
