# Functions to aggregate data : overall characteristics, and at each intersection of stratifying variables
#
# Example
# base_data <- readRDS(here("data", "processed", "df_base.RDS"))
# fup_data <- readRDS(here("data", "processed", "df_fup.RDS"))
# data_id <- clean_data(base_data, fup_data)

#TODO tidy data: produce dataframe with
# date - group - N - timepoint - indicator (cohort, bmi, weight) - stat - value

pacman::p_load(dplyr, tidyr, purrr, gtsummary, janitor)

# tabulate summary statistics across cohort ----------------------------
# Table 1 summary
tabulate_study <- function(data, data_dictionary) {
  organisations <- unique(data$organisation)

  # Data quality
  quality_vars <- list_simplify(
    data_dictionary[["variable_names"]][["participation"]])
  quality_df <- data |>
    dplyr::select(c("Organisation" = "organisation",
                    "Participant timepoint"="participant_timepoint",
                    any_of(quality_vars))) |>
    rename(any_of(quality_vars))
  tab_quality <- map(organisations,
                   ~ quality_df |>
                     filter(Organisation == .x) |>
                     tbl_summary(
                       include = -c("Organisation",
                                    "Participant in follow-up"),
                       missing = "ifany", missing_text = "Missing") |>
                     as_gt()
  )
  names(tab_quality) <- organisations

  # Demographics
  demog_vars <- list_simplify(
                    data_dictionary[["variable_names"]][["demographic"]])
  demog_df <- data |>
    dplyr::select(c("Organisation" = "organisation",
                    "Participant timepoint"="participant_timepoint",
                    any_of(demog_vars))) |>
    rename(any_of(demog_vars))
  tab_demog <- map(organisations,
                 ~ demog_df |>
                   filter(Organisation == .x) |>
                   tbl_summary(by = "Participant timepoint",
                               percent = "row",
                               include = -c("Organisation"),
                               missing = "ifany",
                               missing_text = "Missing") |>
                   add_overall() |>
                   as_gt()
                   )
  names(tab_demog) <- organisations

    # BMI category crosstab
    tab_bmi <- map(organisations,
          ~ data |>
            filter(organisation == .x) |>
            tbl_cross(row = "bmi_category_prewar",
                      col = "bmi_category_daily",
                      percent = "row",
                      missing = "no",
                      digits = 0,
                      label = list("bmi_category_daily" = "Current BMI",
                                   "bmi_category_prewar" = "Pre-war BMI")) |>
            as_gt()
    )
    names(tab_bmi) <- unique(data$organisation)

    study_tables <- list("data_quality" = tab_quality,
                         "demographic" = tab_demog,
                         "bmi_crosstab" = tab_bmi)
    org_tables <- list_transpose(study_tables)

    return(org_tables)
}

# summarise by any given strata ------------------------------------
summarise_strata <- function(data, group_cols) {
  #if(interactive()) print(group_cols)

  # summarise participants per group -----
  df_participants <- data |>
    group_by(across(all_of(group_cols))) |>
    summarise(
      # participants enrolled ---
      cohort_id_enrolled = length(unique(id)),
      # cohort new joiners ---
      cohort_id_new = sum(date == date_entry),
      # cohort in follow up
      cohort_id_followup = sum(participant_in_followup),
      # daily observations ---
      # number of recorded weights, denominator: cohort_n
      cohort_obs_recorded = sum(participant_recorded),
      # missing weight among all enrolled, denominator: cohort_n
      cohort_obs_missing = sum(is.na(weight)),
      #
      cohort_persondays = sum(participant_cumulative_days_enrolled),
      #
      .groups = "drop"
      )

  # summarise observed metrics -----
  # averages
  df_centraltendency <- data |>
    group_by(across(all_of(group_cols))) |>
    summarise(
      across(c("weight",
               "weight_unit_change_prewar",
               "weight_percent_change_firstmeasurement",
               "weight_percent_change_prewar",
               "weight_rate_change_daily",
               #
               "bmi",
               "bmi_unit_change_prewar",
               "bmi_percent_change_firstmeasurement",
               "bmi_percent_change_prewar",
               "bmi_rate_change_daily",
               ),
             .fns = list(
               mean = ~ mean(., na.rm = TRUE),
               median = ~ median(., na.rm = TRUE),
               q1 = ~ quantile(., probs = 0.25, na.rm = TRUE),
               q3 = ~ quantile(., probs = 0.75, na.rm = TRUE)),
             .names = "{.col}.{.fn}"),
      .groups = "drop"
    ) |>
    pivot_longer(cols = -group_cols) %>%
    separate(name, into = c("variable", "stat"), sep = "\\.")

  # BMI by category
  df_bmi_count <- data |>
    filter(!is.na(bmi)) |>
    group_by(across(all_of(c(group_cols)))) |>
    pivot_longer(cols = contains("bmi_category"),
                 names_to = "bmi_period", values_to = "bmi_category") |>
    group_by(across(all_of(c(group_cols, "bmi_period", "bmi_category")))) |>
    count(name = "value") |>
    mutate(stat = "count",
           variable = paste0(bmi_period, "_", bmi_category))

  # combine summaries -----
  df_summary <- bind_rows(df_centraltendency,
                          df_bmi_count) |>
    left_join(df_participants,
              by = group_cols) |>
    ungroup() |>
    # create single grouping id
    mutate(group = paste(group_cols, collapse = "-"),
            label = pmap_chr(across(all_of(setdiff(group_cols,
                                                   c("date",
                                                     "organisation")))),
                             ~ paste(..., sep = ", ")))

  return(df_summary)
}

clean_aggregated_data <- function(summary_list) {
  summary_df <- list_rbind(summary_list) |>
    ungroup()

  # label current summary date
  summary_df <- summary_df |>
    mutate(date_summarised = Sys.Date())

  # split into a list indexed by organisation
  org_split <- summary_df |>
    mutate(group = str_replace_all(group, "date-organisation-", "")) |>
    mutate(group = str_replace_all(group, "date-", "")) |>
    dplyr::select(-overall)
  org_split <- split(org_split, org_split$organisation)
  org_split <- map(org_split,
                   ~ split(., .$group))

  return(org_split)
}
