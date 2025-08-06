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
      obs_recorded = sum(!is.na(weight)),
      obs_missing = sum(is.na(weight)),
      obs_anomalous = sum(!include_observation),
      obs_excluded_percent = (obs_missing + obs_anomalous) /
        participant_days_n * 100) |>
    pivot_longer(cols = -group_cols, names_to = "variable") |>
    mutate(stat = ifelse(grepl("_percent", variable),
                         "percent", "count"))

  # summarise observed metrics -----
  ## drop anomalous observations
  data <- data |>
    filter(include_observation)

  # averages
  df_centraltendency <- data |>
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

  # proportions
  df_props <- data |>
    group_by(across(c(all_of(group_cols),
                      "bmi_category"))) |>
    summarise (n = n()) %>%
    mutate(value = n / sum(n) * 100,
           stat = "percent",
           variable = paste0("bmi_category_", bmi_category)) |>
    dplyr::select(-c(n, bmi_category))

  # combine summaries -----
  df_summary <- bind_rows(df_participants,
                          df_centraltendency,
                          df_props) |>
    # create single grouping id
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
