# Run the full data processing pipeline on a local server hosting raw data, ready to be pushed to public Github

# Set up -------
pacman::p_load(here, purrr, dplyr)
set.seed(123)
#do not show summarise message
options(dplyr.summarise.inform = FALSE)

# error logging
log <- list(log_time = Sys.time())


# Load data from local private repo ---------------------------------------
# Get wd from passed argument if ran on server
.args = if(interactive()) here() else commandArgs(trailingOnly = TRUE)
.args = setNames(.args, c("wd"))
public_repo <- sprintf("%s/", .args["wd"])
# get filepath to local private data
private_repo <- gsub("gaza-response", "wt_monitoring_gaza", public_repo)

if (file.exists(paste0(private_repo, "data/processed/df_base.RDS"))) {
  base_data_path <- paste0(private_repo, "data/processed/df_base.RDS")
} else {base_data_path <- paste0(public_repo, "data/processed/df_base.RDS")}

if (file.exists(paste0(private_repo, "data/processed/df_fup.RDS"))) {
  fup_data_path <- paste0(private_repo, "data/processed/df_fup.RDS")
} else {fup_data_path <- paste0(public_repo, "data/processed/df_fup.RDS")}

base_data <- readRDS(base_data_path)
fup_data <- readRDS(fup_data_path)

# log raw data
log$data_raw <- list()
log$data_raw$base_data_source <- base_data_path
log$data_raw$fup_data_source <- fup_data_path
if (file.exists(base_data_path)) {
  log$debug$base_file_timestamp <- file.info(base_data_path)$mtime
  log$debug$base_file_size <- file.info(base_data_path)$size
}
if (file.exists(fup_data_path)) {
  log$debug$fup_file_timestamp <- file.info(fup_data_path)$mtime
  log$debug$fup_file_size <- file.info(fup_data_path)$size
}

expected_base <- c("id", "date", "organisation", "age", "sex", "governorate",
              "role", "height", "weight_prewar", "weight", "children_feeding")
log$data_raw$base_cols_missing <- setdiff(expected_base, colnames(base_data))
expected_fup <- c("id", "date", "weight")
log$data_raw$fup_cols_missing <- setdiff(expected_fup, colnames(fup_data))
log$data_raw$n_participants_baseline <- length(unique(base_data$id))
log$data_raw$max_date <- max(fup_data$date)
log$data_raw$orgs <- unique(base_data$organisation)


# Data processing -----------------------------------------------------------
# Load functions -----
pipeline_functions <- paste0(public_repo,
                             c("R/data-pipeline/1-data_cleaning.R",
                               "R/data-pipeline/2-data_aggregation.R"))
walk(pipeline_functions, source)

# Clean individual level data -----------------------------------------
data_id_daily <- clean_data(base_data, fup_data)
log$data_clean$nrow <- nrow(data_id_daily)
log$data_clean$n_id <- length(unique(data_id_daily$id))

# Summaries ------------------------------------------------------------
# summary 1: last recorded observation for all participants
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

# summary 2: by date, organisation, and group -----
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
log$output_file <- output_file

# -------------------------------------------------------------------------

# extra logging for debugging
log$debug$working_directory <- getwd()
log$debug$private_repo <- private_repo
log$debug$public_repo <- public_repo
log$debug$args_received <- .args

private_dir <- paste0(private_repo, "data/processed/")
log$debug$private_data_processed <- list.files(private_dir, full.names = TRUE)

public_dir <- paste0(public_repo, "data/public/")
log$debug$data_public <- list.files(public_dir, full.names = FALSE)

summary_file <- paste0(public_repo, "data/public/summary-stats.RDS")
log$debug$summary_file_timestamp <- file.info(summary_file)$mtime

output_log = sprintf("%s/data/public/log.RDS", .args["wd"])
saveRDS(log, output_log)
# RDS data pushed to Github public repo
