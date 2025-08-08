# Run the full data processing pipeline on a local server hosting raw data, ready to be pushed to public Github

# Set up -------
pacman::p_load(here, purrr, dplyr)

# Load the pipeline functions, locally or from github
#if(interactive()) {
  base <- here("R", "data-pipeline")
#} else {
#  base <- "https://raw.githubusercontent.com/cmmid/gaza-response/main/R/data-pipeline" }

pipeline_functions <- paste0(base,
                             c("/1-data_cleaning.R",
                               "/2-data_aggregation.R"))
walk(pipeline_functions, source)

# Load data stored locally
base_data <- readRDS(here("data", "processed", "df_base.RDS"))
fup_data <- readRDS(here("data", "processed", "df_fup.RDS"))

# Clean data -----
data_id <- clean_data(base_data, fup_data)

# Create 2 levels of stratification for now ----
group_cols <- c("agegroup", "children_feeding", "governorate", "role", "sex")
group_cols <- combn(group_cols, 2, simplify = FALSE)
group_cols <- append(group_cols, as.list(c("overall", "agegroup", "children_feeding", "governorate", "role", "sex")))
group_cols <- append(map(group_cols,
                         ~ c("date", "organisation", sort(.x))),
                     map(group_cols,
                         ~ c("date", sort(.x))))

# Current summary: use most recent observation from participants reporting in most recent x day window -----
# TODO fix this very hacky code
# TODO set this interactively so not fixed to now
current_days <- seq.Date(Sys.Date() - 3, length.out = 3, by = "day")

# filter to current data
data_id_latest <- data_id |>
  group_by(id) |>
  filter(
    # only include observations that are recorded & in valid range
    observation_valid &
      # only within most recent window
      date %in% current_days &
      # only latest for each participant
      date == max(date, na.rm = TRUE)) |>
  ungroup() |>
  # set date to the future to use as a flag that this is the most recent record (noting all group calculations include date so will not be double-counted)
  mutate(date = Sys.Date() + 3650)

# bind latest data with fill time series
data_id_dated <- bind_rows(data_id, data_id_latest)

# summarise by date, organisation, and group -----
summary <- imap(group_cols,
                     ~ data_id_dated |>
                       summarise_ids(group_cols = .x)) |>
  clean_aggregated_data()

# save ----------------------
saveRDS(summary, here("data", "public", "summary-stats.RDS"))

# RDS data pushed to Github public repo
