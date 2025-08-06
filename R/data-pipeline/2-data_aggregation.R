# Functions to aggregate data at each intersection of stratifying variables
#
# Example
# base_data <- readRDS(here("data", "processed", "df_base.RDS"))
# fup_data <- readRDS(here("data", "processed", "df_fup.RDS"))
# data_id <- clean_data(base_data, fup_data)

pacman::p_load(dplyr, tidyr, purrr)

# summarise by any given strata ------------------------------------
summarise_ids_by <- function(data, group_cols) {

  # summarise participants per group -----
  df_participants <- data |>
    group_by(across(all_of(group_cols))) |>
    summarise(
      # participants ---
      participant_n = length(unique(id)),
      participant_days_n = n(),
      # data quality ---
      ## % records missing weight observations
      days_observed = sum(!is.na(weight)),
      days_missing = sum(is.na(weight)),
      days_anomalous = sum(!include_observation),
      ## % participants
      days_excluded_percent = (days_missing + days_anomalous) /
        participant_days_n * 100,
      ) |>
    pivot_longer(cols = -group_cols)

  # summarise observed metrics -----
  df_centraltendency <- data |>
  # drop anomalous observations
    filter(include_observation)
  # summarise
    group_by(across(all_of(group_cols))) |>
    summarise(
      across(c("weight",
               "bmi",
               "percent_change_firstmeasurement",
               "percent_change_prewar"),
             .fns = list(
               mean = ~ mean(., na.rm = TRUE),
               median = ~ median(., na.rm = TRUE),
               q1 = ~ quantile(., probs = 0.25, na.rm = TRUE),
               q3 = ~ quantile(., probs = 0.75, na.rm = TRUE)),
             .names = "{.col}.{.fn}")
    ) |>
    pivot_longer(cols = -group_cols) %>%
    separate(name, into = c("variable", "stat"), sep = "\\.")

  # combine summaries -----
  df_summary <- left_join(df_participants, df_centraltendency,
                          by = group_cols) |>
    mutate(group = paste(group_cols, collapse = "-"))

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
