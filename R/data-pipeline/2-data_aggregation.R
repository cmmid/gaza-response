# Functions to aggregate data at each intersection of stratifying variables
#
# Example
# base_data <- readRDS(here("data", "processed", "df_base.RDS"))
# fup_data <- readRDS(here("data", "processed", "df_fup.RDS"))
# data_id <- clean_data(base_data, fup_data)

pacman::p_load(dplyr, tidyr, purrr)

# summarise by any given strata ------------------------------------
summarise_ids <- function(data, group_cols) {
  print(group_cols)

  # summarise participants per group -----
  df_participants <- data |>
    group_by(across(all_of(group_cols))) |>
    summarise(
      # participants ---
      cohort_n = length(unique(id)),
      cohort_days_enrolled = n(),
      # data quality ---
      ## % records missing weight observations
      cohort_recorded = sum(!is.na(weight)),
      cohort_missing = sum(is.na(weight)),
      cohort_anomalous = sum(!observation_valid),
      cohort_invalid_percent = (obs_missing + obs_anomalous) /
        cohort_days_enrolled * 100) |>
    ungroup()

  # summarise observed metrics -----
  ## drop anomalous observations
  data <- data |>
    filter(observation_valid)

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
             .names = "{.col}.{.fn}")
    ) |>
    pivot_longer(cols = -group_cols) %>%
    separate(name, into = c("variable", "stat"), sep = "\\.")

  # proportions
  df_props <- data |>
    group_by(across(all_of(c(group_cols,
                             "bmi_category")))) |>
    summarise(count = n()) |>
    left_join(dplyr::select(df_participants,
                            all_of(c(group_cols, "cohort_n")))) |>
    mutate(value = count / cohort_n * 100,
           stat = "percent",
           variable = paste0("bmi_category_", bmi_category)) |>
    ungroup() |>
    dplyr::select(all_of(c(group_cols, "value", "stat", "variable")))

  # combine summaries -----
  df_summary <- bind_rows(df_centraltendency,
                          df_props) |>
    left_join(df_participants,
              by = group_cols) |>
    ungroup() |>
    # create single grouping id
    mutate(group = paste(group_cols, collapse = "-"),
            label = pmap_chr(across(all_of(setdiff(group_cols, c("date", "organisation")))), ~ paste(..., sep = ", ")))

  return(df_summary)
}

clean_aggregated_data <- function(summary_list) {
  # Restructuring into a list by organisation
  summary_df <- list_rbind(summary_list) |>
    ungroup() |>
    mutate(organisation = if_else(is.na(organisation), "all", organisation)) |>
    mutate(group = str_replace_all(group, "date-organisation-", "")) |>
    mutate(group = str_replace_all(group, "date-", "")) |>
    dplyr::select(-overall)
  org_split <- split(summary_df, summary_df$organisation)
  org_split <- map(org_split,
                   ~ split(., .$group))
  return(org_split)
}
