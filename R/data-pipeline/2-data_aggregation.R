# Functions to aggregate data : overall characteristics, and at each intersection of stratifying variables
#
# Example
# base_data <- readRDS(here("data", "processed", "df_base.RDS"))
# fup_data <- readRDS(here("data", "processed", "df_fup.RDS"))
# data_id <- clean_data(base_data, fup_data)

#TODO tidy data: produce dataframe with
# date - group - N - timepoint - indicator (cohort, bmi, weight) - stat - value

pacman::p_load(dplyr, tidyr, purrr, gtsummary, janitor, stringr)

# tabulate summary statistics across cohort ----------------------------
# Table 1 summary
tabulate_study <- function(data, strata, data_dictionary) {
  organisations <- unique(data$organisation)

  # Data quality
  quality_df <- data |>
    mutate(participant_cumulative_days_recorded = as.numeric(participant_cumulative_days_recorded)) |>
    dplyr::select("Organisation" = "organisation",
                  "Number of observations by timepoint" = participant_timepoint,
                  "Number of observations per participant" = participant_cumulative_days_recorded,
                  "Excluded observations" = anomaly
                  ) |>
    rename(any_of(c(data_dictionary$data_levels)))

  tab_quality <- map(organisations,
                   ~ quality_df |>
                     filter(Organisation == .x) |>
                     tbl_summary(
                       include = -c("Organisation"),
                       missing = "ifany", missing_text = "Missing") |>
                     modify_header(label ~ "**Observations**") |>
                     modify_source_note(source_note =
                     "Exclusion criteria indicate observation recorded but outside of valid range, and treated as missing.
                     Valid ranges: weight, 30-180kg; BMI: 10-60; daily rate of change in weight since study entry over +/-10%; age, 16-99 years; dependent children, 0-20") |>
                     as_gt()
                   )
  names(tab_quality) <- organisations

  # Demographic strata
  strata_df <- data |>
    dplyr::select("organisation",
                  "Participant timepoint" = "participant_timepoint",
                  any_of(c(!!!strata))) |>
    rename(any_of(c(data_dictionary$data_levels)))
  tab_strata <- map(organisations,
                 ~ strata_df |>
                   filter(Organisation == .x) |>
                   tbl_summary(by = "Participant timepoint",
                               percent = "column",
                               include = -c("Organisation"),
                               missing = "ifany",
                               missing_text = "Missing") |>
                   add_overall() |>
                   as_gt()
                   )
  names(tab_strata) <- organisations

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
                         "demographic_strata" = tab_strata,
                         "bmi_crosstab" = tab_bmi)
    org_tables <- list_transpose(study_tables)

    return(org_tables)
}

# summarise by any given strata ------------------------------------
summarise_strata <- function(data, strata) {
  #if(interactive()) print(group_cols)

  # create single grouping id
  data <- data |>
    mutate(strata = {{ strata }},
           stratum = paste(!!!syms(strata)))

  # summarise participants per group -----
  df_participants <- data |>
    group_by(date, organisation, strata, stratum) |>
    summarise(
      # participants ---
      # -- all observed
      cohort_id_recorded = sum(participant_recorded),
      # -- cohort new joiners
      cohort_id_new = sum(participant_timepoint == "Study entry"),
      # -- cohort in follow up
      cohort_id_followup_record = sum(participant_timepoint == "Follow up"),
      # -- cohort in follow up
      cohort_id_followup_ever = sum(participant_in_followup),
      # -- time in study across all participants
      cohort_persondays = sum(participant_cumulative_days_enrolled),
      # -- time in study among those in follow up
      cohort_persondays_followup = sum(cohort_persondays - cohort_id_new +
                                         cohort_id_followup_ever),
      # daily observations
      # number of recorded weights
      cohort_obs_recorded = sum(!is.na(weight_daily)),
      # missing/anomalous weight among recorded
      cohort_obs_missing = sum(is.na(weight_daily)),
      #
      .groups = "drop"
      )

  # summarise observed metrics -----
  # averages
  df_centraltendency <- data |>
    group_by(date, organisation, strata, stratum) |>
    arrange(date, .by_group = TRUE) |>
    summarise(
      across(any_of(c("weight_daily",
               "weight_change_unit_prewar",
               "weight_change_percent_entry",
               "weight_change_percent_prewar",
               "weight_change_percent_daily_rate_entry",
               #
               "bmi",
               "bmi_change_unit_prewar",
               "bmi_change_percent_entry",
               "bmi_change_percent_prewar",
               "bmi_change_percent_daily_rate_entry",
               #
               "participant_cumulative_days_enrolled"
               )),
             .fns = list(
               mean = ~ mean(., na.rm = TRUE),
               median = ~ median(., na.rm = TRUE),
               q1 = ~ quantile(., probs = 0.25, na.rm = TRUE),
               q3 = ~ quantile(., probs = 0.75, na.rm = TRUE)),
             .names = "{.col}.{.fn}"),
      .groups = "drop"
    ) |>
    pivot_longer(cols = -c(date, organisation, strata, stratum)) %>%
    separate(name, into = c("variable", "stat"), sep = "\\.")

  # BMI by category
  df_bmi_count <- data |>
    filter(!is.na(bmi_daily)) |>
    group_by(date, organisation, strata, stratum) |>
    pivot_longer(cols = contains("bmi_category"),
                 names_to = "bmi_period",
                 values_to = "bmi_category") |>
    group_by(date, organisation, strata, stratum,
             bmi_period, bmi_category) |>
    count(name = "value") |>
    mutate(stat = "count",
           variable = paste0(bmi_period, "_", bmi_category))

  # combine summaries -----
  df_summary <- bind_rows(df_centraltendency,
                          df_bmi_count) |>
    left_join(df_participants,
              by = c("date", "organisation",
                     "strata", "stratum")) |>
    ungroup() |>
    mutate(date_summarised = Sys.Date())

  df_summary <- df_summary |>
    filter(stratum != "NA")

  return(df_summary)
}
