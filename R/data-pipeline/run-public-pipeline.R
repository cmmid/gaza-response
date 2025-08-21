# Run the full data processing pipeline on a local server hosting raw data, ready to be pushed to public Github

# Set up -------
pacman::p_load(here, purrr, dplyr)
set.seed(123)
#do not show summarise message
options(dplyr.summarise.inform = FALSE)

# error logging
log <- list(log_time = Sys.time())

# Get wd from passed argument if ran on server
.args = if(interactive()) here() else commandArgs(trailingOnly = TRUE)
.args = setNames(.args, c("wd"))

base <- sprintf("%s/", .args["wd"]) #"https://raw.githubusercontent.com/cmmid/gaza-response/main/R/data-pipeline"

# Load functions -----
pipeline_functions <- paste0(base,
                             c("R/data-pipeline/1-data_cleaning.R",
                               "R/data-pipeline/2-data_aggregation.R"))
walk(pipeline_functions, source)

# Load data stored locally -----
base_data <- readRDS(paste0(base, "data/processed/df_base.RDS"))
fup_data <- readRDS(paste0(base, "data/processed/df_fup.RDS"))

# log raw data validation
log$data_raw <- list()
expected_base <- c("id", "date", "organisation", "age", "sex", "governorate",
              "role", "height", "weight_prewar", "weight", "children_feeding")
log$data_raw$base_cols_missing <- setdiff(expected_base, colnames(base_data))
expected_fup <- c("id", "date", "weight")
log$data_raw$fup_cols_missing <- setdiff(expected_fup, colnames(fup_data))
log$data_raw$n_participants_baseline <- length(unique(base_data$id))
log$data_raw$max_date <- max(fup_data$date)
log$data_raw$orgs <- unique(base_data$organisation)

# Clean data -----
data_id_daily <- clean_data(base_data, fup_data)
log$data_clean$nrow <- nrow(data_id_daily)
log$data_clean$n_id <- length(unique(data_id_daily$id))

# Summaries ------------------------------------------------------------
# filter to last recorded observation for all participants
data_id_last <- data_id_daily |>
  filter(last_measurement)

latest_date <- as.Date(max(data_id_daily$date, na.rm = TRUE))
log$data_clean$latest_date <- latest_date

# set date to the future to use as a flag that this is the most recent record
#   (noting all group calculations include date so will not be double-counted)
data_id_last <- data_id_last |>
  mutate(date = Sys.Date() + 3650)

# bind latest observation with full time series
data_id <- bind_rows(data_id_daily, data_id_last)

# summarise by date, organisation, and group -----
# Create 2 levels of stratification
group_cols <- c("agegroup", "children_feeding", "governorate", "role", "sex")
group_cols <- combn(group_cols, 2, simplify = FALSE)
group_cols <- append(group_cols, as.list(c("overall", "agegroup", "children_feeding", "governorate", "role", "sex")))
group_cols <- append(map(group_cols,
                         ~ c("date", "organisation", sort(.x))),
                     map(group_cols,
                         ~ c("date", sort(.x))))
log$data_summary <- list()
log$data_summary$group_cols <- group_cols
#' Do not print all messages
suppressMessages({
    summary <- imap(group_cols,
                    ~ data_id |>
                      summarise_ids(group_cols = .x)) |>
      clean_aggregated_data()
  })
log$data_summary$overall_latest_date <- max(summary$all$overall$date, na.rm=TRUE)
log$data_summary$overall_sample <- dplyr::slice_sample(summary$all$overall,
                                                       prop = 0.1)

# save ----------------------
output_file = sprintf("%s/data/public/summary-stats.RDS", .args["wd"])
saveRDS(summary, output_file)

output_log = sprintf("%s/data/public/log.RDS", .args["wd"])
saveRDS(log, output_log)
# RDS data pushed to Github public repo
