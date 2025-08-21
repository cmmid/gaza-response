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
tabulate_baseline <- function(df, by_group="organisation") {

  return(characteristics)
}

tabulate_metrics <- function(df, by_group="organisation", col_labels) {
  df <- mutate(df, across(where(is.factor), fct_drop))
  col_include <- col_labels[grepl("(cohort*)|(bmi*)|(weight*)",
                                  names(col_labels))]
  metrics <- df |>
    tbl_summary(
      by = by_group,
      include = as.character(col_include),
      label = as.character(col_include) ~ names(col_include)
    )
  return(metrics)
}

# BMI category crosstab per organisation
bmi_crosstab <- function(df, col_labels) {
  org_df <- split(df, df$organisation, drop = TRUE)
  bmi_tab <- tbl_cross(df,
                       row = "bmi_category_prewar",
                       col = "bmi_category_daily",
                       percent = "row",
                       missing = "no",
                       digits = 0,
                       label = list("bmi_category_daily" = "Current BMI",
                                    "bmi_category_prewar" = "Pre-war BMI"))
  return(bmi_tab)
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
      cohort_total_persondays = sum(participant_cumulative_days_enrolled),
      #
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
            label = pmap_chr(across(all_of(setdiff(group_cols,
                                                   c("date",
                                                     "organisation")))),
                             ~ paste(..., sep = ", ")))

  return(df_summary)
}

clean_aggregated_data <- function(summary_list) {
  summary_df <- list_rbind(summary_list) |>
    ungroup()

  # label current summary stats - setting "date" to NA (as this is a summary of multiple dates),
  #   and marking these records with "current_summary_date" = latest date in the data
  latest_date <- max(summary_df |> filter(date <= Sys.Date()) |> pull(date), na.rm=TRUE)
  summary_df <- summary_df |>
    mutate(current_summary_date = as.Date(if_else(date > Sys.Date(),
                                         latest_date, NA)),
           date = replace(date, date > Sys.Date(), NA))

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
