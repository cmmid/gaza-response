# Run the full data processing pipeline on a local server hosting raw data, ready to be pushed to public Github

# Set up -------
pacman::p_load(here, purrr, dplyr, gtsummary)
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
                             c(
                               "R/data-pipeline/0-data-dictionary.R",
                               "R/data-pipeline/1-data_cleaning.R",
                               "R/data-pipeline/2-data_aggregation.R",
                               "R/data-pipeline/helpers.R"
                               ))
walk(pipeline_functions, source)

# Load data stored locally -----
base_data <- readRDS(paste0(base, "data/processed/df_base.RDS"))
fup_data <- readRDS(paste0(base, "data/processed/df_fup.RDS"))
data_dictionary <- set_data_dictionary()
# col_labels <- get_column_labels()

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

# Clean data ------------------------------------------------------------
# combine baseline and follow up data; calculate BMI and change
suppressWarnings(
  data_id_daily <- clean_data(base_data, fup_data, data_dictionary)
)
log$data_clean$nrow <- nrow(data_id_daily)
log$data_clean$n_id <- length(unique(data_id_daily$id))
log$data_clean$exclusion <- data_id_daily |>
  group_by(organisation, anomaly) |>
  summarise(n_records = n(),
            n_participants = n_distinct(id))

# Pre-processing for summaries ---------------------------------------------
# add "Overall" organisation
data_id_daily <- data_id_daily |>
  mutate(organisation = "Overall") |>
  bind_rows(data_id_daily)

# add "overall" variable as a buffer for stratification --
data_id_daily <- data_id_daily |>
  mutate(overall = "overall")

# Most recent records -----------------------------------------
# create a df with only last recorded observation by ID
data_id_last <- data_id_daily |>
  filter(last_measurement)
log$data_latest$date_of_latest <- count(data_id_last, organisation, date)

# # tabulate all participants by demography & data quality
tables_latest <- tabulate_study(data_id_last, data_dictionary)
log$data_latest$summary_tables <- tables_latest$Overall

# strata summaries over time ------------------------------------------
# set last date to the future to use as a flag that this is the most recent record
#   (noting all group calculations include date so will not be double-counted)
data_id_last <- data_id_last |>
  mutate(date = Sys.Date() + 3650)

# summarise by date, organisation, and group -----
# bind last available record with full time series --
data_id_aggregate <- bind_rows(data_id_daily, data_id_last)

group_cols <- c("overall", "age", "children_feeding",
                "governorate", "role", "sex")
group_cols <- map(group_cols,
                  ~ c("date", "organisation", sort(.x)))

# Create 2 levels of stratification
# group_cols <- combn(group_cols, 2, simplify = FALSE)
# group_cols <- append(group_cols, as.list(c("overall", "age",
#                                            "children_feeding", "governorate",
#                                            "role", "sex")))
# group_cols <- append(group_cols, map(group_cols,
#                          ~ c("date", "organisation", sort(.x))))

#' Do not print all messages
suppressMessages({
  summary <- map(group_cols,
                    ~ data_id_aggregate |>
                      summarise_strata(group_cols = .x)) |>
      clean_aggregated_data()
  })

log$data_summary$overall_latest_date <- max(summary$Overall$overall$date,
                                            na.rm=TRUE)
log$data_summary$overall_sample <- dplyr::slice_sample(summary$Overall$overall, prop = 0.1)

# save ----------------------
output_tables = sprintf("%s/data/public/summary-tables.RDS",.args["wd"])
saveRDS(tables_latest, output_tables)

output_file = sprintf("%s/data/public/summary-stats.RDS", .args["wd"])
saveRDS(summary, output_file)

output_log = sprintf("%s/data/public/log.RDS", .args["wd"])
saveRDS(log, output_log)
# RDS data pushed to Github public repo
