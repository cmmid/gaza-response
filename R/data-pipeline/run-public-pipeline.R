# Run the full data processing pipeline on a local server hosting raw data, ready to be pushed to public Github

# ------------------ Set up ------------------------------------
pacman::p_load(here, purrr, dplyr, gtsummary, forcats, tidyr)
set.seed(123)
#do not show summarise message
options(dplyr.summarise.inform = FALSE)

# error logging
log <- list(log_time = Sys.time())

# Get wd from passed argument if ran on server
.args = if(interactive()) here() else commandArgs(trailingOnly = TRUE)
.args = setNames(.args, c("wd"))

base <- sprintf("%s/", .args["wd"])

# Load functions -----
pipeline_functions <- paste0(base,
                             c("R/data-pipeline/0-data-dictionary.R",
                               "R/data-pipeline/1-data_cleaning.R",
                               "R/data-pipeline/2-data_aggregation.R",
                               "R/data-pipeline/helpers.R"
                             ))
walk(pipeline_functions, source)

# Load data stored locally -----
base_data <- readRDS(paste0(base, "data/processed/df_base.RDS"))
fup_data <- readRDS(paste0(base, "data/processed/df_fup.RDS"))
data_dictionary <- set_data_dictionary()

# log raw data validation
log$data_raw <- list()
expected_base <- c("id", "date", "organisation",
                   "age", "sex", "governorate", "role", "children_feeding",
                   "height", "weight_prewar", "weight")
log$data_raw$base_cols_missing <- setdiff(expected_base, colnames(base_data))
expected_fup <- c("id", "date", "weight")
log$data_raw$fup_cols_missing <- setdiff(expected_fup, colnames(fup_data))
log$data_raw$n_participants_baseline <- length(unique(base_data$id))
log$data_raw$max_date <- max(fup_data$date)
log$data_raw$orgs <- unique(base_data$organisation)

# ------------------ Linelist data cleaning ---------------------------------
# Combine baseline and follow up data; calculate BMI and change; set factors
suppressWarnings(
  data_id_daily <- clean_data(base_data, fup_data, data_dictionary)
)
log$data_clean$n_records <- nrow(data_id_daily)
log$data_clean$n_id <- length(unique(data_id_daily$id))
log$data_clean$n_followup <- length(unique(
  filter(data_id_daily, participant_in_followup)$id
))
log$data_clean$exclusion <- data_id_daily |>
  group_by(organisation, anomaly) |>
  summarise(n_records = n(),
            n_participants = n_distinct(id))

# ------------------ Summary tables ------------------------------------
# Pre-processing for summaries ---------------------------------------------
# add "Overall" organisation
data_id_daily <- data_id_daily |>
  mutate(organisation = "Overall") |>
  bind_rows(data_id_daily)

# add "overall" variable as a buffer for stratification --
data_id_daily <- data_id_daily |>
  mutate(overall = "overall")

# set group stratifications
strata <- c("overall", "age", "children_feeding", "governorate", "role", "sex")
log$strata <- strata

# Create 2 levels of stratification
# group_cols <- combn(group_cols, 2, simplify = FALSE)
# group_cols <- append(group_cols, as.list(c("overall", "age",
#                                            "children_feeding", "governorate",
#                                            "role", "sex")))
# group_cols <- append(group_cols, map(group_cols,
#                          ~ c("date", "organisation", sort(.x))))

# SUMMARISE WHOLE COHORT  ---------------------------------------------
# summarise across whole sample using each participants' latest obs ---
# create a df with only last recorded observation by ID
data_id_latest <- data_id_daily |>
  filter(last_measurement)
log$data_latest$date_of_latest <- count(data_id_latest, organisation, date)

## Reset "date" to a single common date (so avoid any grouping by daily date)
data_id_latest <- data_id_latest |>
  mutate(date_observation = date,
         date = max(data_id_latest$date, na.rm = TRUE))

# # tabulate all participants by demography & data quality
tables_latest <- tabulate_study(data_id_latest, strata, data_dictionary)
log$data_latest$summary_tables <- tables_latest$Overall

# Summarise strata at latest record
suppressMessages({
  summary_latest <- map_dfr(strata,
                 ~ data_id_latest |>
                   summarise_strata(strata = .x))
})
log$summary_latest$date_of_latest <- count(data_id_latest,
                                               organisation, date_observation)
log$summary_latest$overall_sample <- summary_latest |>
  filter(organisation == "Overall" & strata == "overall") |>
  slice_sample(prop = 0.1)

# SUMMARISE BY TIME WINDOW ---------------------------------------------
timeseries_window <- "week"
log$timeseries_window <- timeseries_window

data_id_window <- data_id_daily |>
  # set the time window
  mutate(timeseries_window = lubridate::floor_date(date, unit = "week")) |>
  # take only most recent record for each participant within the window
  group_by(id, timeseries_window) |>
  filter(date == max(date, na.rm = TRUE)) |>
  ungroup() |>
  mutate(date = timeseries_window)

# summarise available records at each timestep
suppressMessages({
  summary_timeseries <- map_dfr(strata,
                    ~ data_id_window |>
                      summarise_strata(strata = .x))
  })
log$summary_timeseries$date_of_latest <- count(data_id_window, organisation, date)
log$summary_timeseries$overall_sample <- summary_timeseries |>
  filter(organisation == "Overall" & strata == "overall") |>
  slice_sample(prop = 0.1)


#  --------------------- Save ----------------------
output_dictionary = sprintf("%s/data/data-dictionary.RDS", .args["wd"])
saveRDS(data_dictionary, output_dictionary)

output_tables = sprintf("%s/data/public/summary-tables.RDS", .args["wd"])
saveRDS(tables_latest, output_tables)

output_latest = sprintf("%s/data/public/summary-latest.RDS", .args["wd"])
saveRDS(summary_latest, output_latest)

output_timeseries = sprintf("%s/data/public/summary-timeseries.RDS", .args["wd"])
saveRDS(summary_timeseries, output_timeseries)

output_log = sprintf("%s/data/public/log.RDS", .args["wd"])
saveRDS(log, output_log)
# RDS data pushed to Github public repo


