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
data_id_daily <- clean_data(base_data, fup_data)
# Recode factors
data_id_daily <- data_id_daily |>
  mutate(bmi_category_daily = bmi,
         bmi_category_prewar = bmi_prewar)
suppressWarnings(
  {data_id_daily <- set_factors(df = data_id_daily,
                                 factor_levels = c(data_dictionary$data_levels))
  }
)
# Data quality ------------------------------------------------------------
# Replace anomaly measurements as missing
data_id_daily <- data_id_daily |>
  mutate(across(contains(c("weight", "bmi")),
                ~ if_else(weight_anomaly=="anomaly", NA, .x)))

log$data_clean$nrow <- nrow(data_id_daily)
log$data_clean$n_id <- length(unique(data_id_daily$id))

# Replace all anomaly values as missing
data_id_daily <- data_id_daily |>
  mutate(across(where(is.factor),
                ~ if_else(.x == "anomaly", NA, .x)))

# Drop empty levels of organisation
data_id_daily <- data_id_daily |>
  mutate(organisation = fct_drop(organisation))

# Pre-processing for summaries ---------------------------------------------
# add "Overall" organisation
data_id_daily <- data_id_daily |>
  mutate(organisation = "Overall") |>
  bind_rows(data_id_daily)

# add "overall" variable as a buffer for stratification --
data_id_daily <- data_id_daily |>
  mutate(overall = "overall")

# Get only last available record -----------------------------------------
# create a df with only last recorded observation by ID
data_id_last <- data_id_daily |>
  filter(last_measurement)
latest_date <- as.Date(max(data_id_daily$date, na.rm = TRUE))
log$data_clean$latest_date <- latest_date

# set date to the future to use as a flag that this is the most recent record
#   (noting all group calculations include date so will not be double-counted)
data_id_last <- data_id_last |>
  mutate(date = Sys.Date() + 3650)

# table summary by most recent record --------------------------------------
# tally characteristics across all participants
# log$factor_count <- count_factors(data_id_daily)
#
# # tabulate all participants by organisation
#TODO fix labels
data_pretty <- data_id_last |>
  dplyr::select(c("date", "id", "organisation",
                  "age", "sex", "children_feeding",
                  "role", "governorate",
                  "participant_in_followup",
                  "participant_cumulative_days_recorded",
                  "participant_cumulative_days_enrolled")) |>
  mutate(across(where(is.factor), fct_drop),
         participant_cumulative_days_recorded = as.integer(participant_cumulative_days_recorded),
         participant_cumulative_days_enrolled = as.integer(participant_cumulative_days_enrolled)) |>
  rename(any_of(data_dictionary$variable_names))

log$tab_baseline <- data_pretty |>
  tbl_summary(by = "Organisation",
              statistic = list(
                contains("days") ~ "{median} ({p25}, {p75})",
                all_categorical() ~ "{n} ({p}%)"),
              digits = all_continuous() ~ 1) |>
  add_overall()

# BMI categories
log$tab_bmi_categories <- bmi_crosstab(data_id_last,
                                       data_dictionary$bmi_category)

# strata summaries ----------------------------------------------------
# bind last available record with full time series --
data_id_aggregate <- bind_rows(data_id_daily, data_id_last)
# TODO create the "group" and "label" column (at end of data_agg script) here. - tidyr::unite

# summarise by date, organisation, and group -----
# Create 2 levels of stratification
group_cols <- c("overall", "age", "children_feeding", "governorate", "role", "sex")
# group_cols <- combn(group_cols, 2, simplify = FALSE)
# group_cols <- append(group_cols, as.list(c("overall", "age",
#                                            "children_feeding", "governorate",
#                                            "role", "sex")))
group_cols <- append(group_cols, map(group_cols,
                         ~ c("date", "organisation", sort(.x))))

#' Do not print all messages
suppressMessages({
    summary <- map(group_cols,
                    ~ data_id_aggregate |>
                      summarise_strata(group_cols = .x)) |>
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
