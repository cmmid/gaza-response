# Functions to aggregate data at each intersection of stratifying variables
#
# Example
# base_data <- readRDS(here("data", "processed", "df_base.RDS"))
# fup_data <- readRDS(here("data", "processed", "df_fup.RDS"))
# data_id <- clean_data(base_data, fup_data)

pacman::p_load(dplyr, tidyr, purrr)

# summarise by any given strata ------------------------------------
summarise_ids <- function(data, group_cols) {
  #if(interactive()) print(group_cols)

  # summarise participants per group -----
  df_participants <- data |>
    group_by(across(all_of(group_cols))) |>
    summarise(
      # participants enrolled ---
      cohort_id_enrolled = length(unique(id)),
      # cohort new joiners ---
      cohort_id_new = sum(date == date_first_measurement),
      # daily observations ---
      # number of recorded weights, denominator: cohort_n
      cohort_obs_recorded = sum(!is.na(weight)),
      # missing weight among all enrolled, denominator: cohort_n
      cohort_obs_missing = sum(is.na(weight)),
      # anomalous weight among recorded weights, denominator: cohort_obs_recorded
      cohort_obs_anomalous = sum(weight_anomaly),
      .groups = "drop"
      )

  # summarise observed metrics -----
  ## drop anomalous and missing observations
  # data <- data |>
  #   filter(!is.na(weight) & !weight_anomaly)

  # averages
  df_centraltendency <- data |>
    group_by(across(all_of(group_cols))) |>
    summarise(
      across(c("weight",
               "weight_percent_change_firstmeasurement",
               "weight_percent_change_prewar",
               "bmi",
               "bmi_percent_change_firstmeasurement",
               "bmi_percent_change_prewar"),
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
  # |>
  #   # TODO this might need updating to use the data dictionary, as nesting() only completes based on what's in the data
  #   complete(nesting(!!!syms(group_cols)),
  #            stat, variable,
  #            fill = list(value = 0))

  # combine summaries -----
  df_summary <- bind_rows(df_centraltendency,
                          df_bmi_count) |>
    left_join(df_participants,
              by = group_cols) |>
    ungroup() |>
    # create single grouping id
    mutate(group = paste(group_cols, collapse = "-"),
            label = pmap_chr(across(all_of(setdiff(group_cols, c("date", "organisation")))), ~ paste(..., sep = ", ")))

  return(df_summary)
}

clean_aggregated_data <- function(summary_list, latest_date) {

  summary_df <- list_rbind(summary_list) |>
    ungroup()

  # label summary stats based on the 72h window as the "current_summary_date",
  #   setting "date" to NA (as this is a summary of multiple dates),
  #   and marking these records with "current_summary_date" = latest date in the data
  summary_df <- summary_df |>
    mutate(current_summary_date = as.Date(if_else(date > Sys.Date(),
                                         latest_date, NA)),
           date = replace(date, date > Sys.Date(), NA))


  # drop "other" sex category
  summary_df <- summary_df |>
    filter(!grepl("other/prefer not to share", sex))

  # split into a list indexed by organisation
  org_split <- summary_df |>
    mutate(organisation = if_else(is.na(organisation), "all", organisation)) |>
    mutate(group = str_replace_all(group, "date-organisation-", "")) |>
    mutate(group = str_replace_all(group, "date-", "")) |>
    dplyr::select(-overall)
  org_split <- split(org_split, org_split$organisation)
  org_split <- map(org_split,
                   ~ split(., .$group))

  return(org_split)
}
