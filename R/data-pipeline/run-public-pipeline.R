# Run the full data processing pipeline on a local server hosting raw data, ready to be pushed to public Github

# Set up -------
pacman::p_load(here, purrr, dplyr)

# Load the pipeline functions, locally or from github
if(interactive()) {
  base <- here("R", "data-pipeline")
  } else {
  base <- "https://raw.githubusercontent.com/cmmid/gaza-response/main/R/data-pipeline" }

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

# Trends over time: summarise by date, organisation, and group -----
# summarise
summary_date <- imap(group_cols,
               ~ data_id |>
                 summarise_ids(group_cols = .x)) |>
  clean_aggregated_data()
# save
saveRDS(summary_date, here("data", "public", "summary-date.RDS"))

# Current summary: use most recent observation from participants reporting in most recent x day window -----
# set window for current data
current_days <- Sys.Date() - 3 # TODO set this interactively so not fixed to now
# filter to current data
data_id_latest <- data_id |>
  group_by(id) |>
  filter(
    # only include observations that are recorded & in valid range
    observation_valid &
    # only latest for each participant
    cumulative_days_obs_recorded == max(cumulative_days_obs_recorded,
                                         na.rm = TRUE) &
    # only within most recent window
    date >= current_days) |>
  ungroup() |>
  # set all dates to earliest date in recent window
  mutate(date = min(date))

# summarise
summary_current <- imap(group_cols,
                     ~ data_id_latest |>
                       summarise_ids(group_cols = .x)) |>
  clean_aggregated_data()
# save
saveRDS(summary_date, here("data", "public", "summary-current.RDS"))

# RDS data pushed to Github public repo
