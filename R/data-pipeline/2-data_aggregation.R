# Functions to aggregate data at each intersection of stratifying variables
#
# Example
# base_data <- readRDS(here("data", "processed", "df_base.RDS"))
# fup_data <- readRDS(here("data", "processed", "df_fup.RDS"))
# data_id <- clean_data(base_data, fup_data)

pacman::p_load(dplyr, tidyr, purrr)

# summarise by any given strata ------------------------------------
summarise_ids_by <- function(data, group_cols) {

  # summarise participants per group
  df_participants <- data |>
    group_by(across(all_of(group_cols))) |>
    summarise(
      # participants ---
      participants = length(unique(id)),
      days_in_study = sum(days_in_study, na.rm = TRUE),
      # data quality ---
      ## % participants with anomalous current BMI
      percent_bmi_anomaly = sum(bmi_anomaly, na.rm = TRUE) /
        participants * 100,
      ## % observations since enrolment with anomalous change in daily weight
      percent_change_anomaly = sum(change_anomaly, na.rm = TRUE)  /
        (days_in_study - participants) * 100)

  # drop anomalous observations
  data <- data |>
    filter(!bmi_anomaly | !change_anomaly)

  # summarise observed metrics, excluding anomalous observations
  df_centraltendency <- data |>
    group_by(across(all_of(group_cols))) |>
    summarise(
      across(c("days_in_study",
               "weight",
               "bmi",
               "percent_change_firstmeasurement",
               "percent_change_prewar"),
             .fns = list(mean = ~ mean(., na.rm = TRUE),
                         median = ~ median(., na.rm = TRUE),
                         q1 = ~ quantile(., probs = 0.25, na.rm = TRUE),
                         q3 = ~ quantile(., probs = 0.75, na.rm = TRUE),
                         records = n(),
                         recorded = ~ n() - sum(is.na(.)),
                         missing = ~ sum(is.na(.)),
             )
             .names = "{.col}.{.fn}")
    )

  df_centraltendency <- df_centraltendency |>
    pivot_longer(cols = -group_cols) %>%
    separate(name, into = c("variable", "stat"), sep = "\\.") %>%
    pivot_wider(names_from = stat, values_from = value) %>%
    mutate(group = paste(group_cols, collapse = "-"))

  # combine summaries
  df_summary <- left_join(df_participants, df_centraltendency,
                          by = all_of(group_cols))

  return(df_summary)
}

# summarise by intersection of multiple strata combinations -----
summarise_ids <- function(data,
                             group_cols,
                             min_date = NULL, max_date = NULL) {

  # TODO filter dates if specified
  # if (is.null(min_date)) {min_date <- min(data$date)}
  # if (is.null(max_date)) {max_date <- max(data$date)}
  # data <- data |>
  #   filter(between(date, min_date, max_date))

  # set up groups of stratification
  all_groupings <- map(group_cols,
                       ~ list(
                         .x,  # just by stratification
                         c(.x, "date"), # each stratification by date/org
                         c(.x, "organisation"),
                         c(.x, "organisation", "date")
                       ))
  names(all_groupings) <- group_cols
  all_groupings <- list_flatten(all_groupings)

  # calculate summaries
  summary <- map(all_groupings,
                 ~ data |>
                   summarise_ids_by(group_cols = .x) |>
                   mutate(strata = str_remove_all(group,
                                                 regex("-organisation|-date"))))

  return(summary)
}
