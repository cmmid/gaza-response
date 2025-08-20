# Run the full data processing pipeline on a local server hosting raw data, ready to be pushed to public Github

# Set up -------
pacman::p_load(here, purrr, dplyr, gtsummary)
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
                             c("R/data-pipeline/0-data-dictionary.R",
                               "R/data-pipeline/1-data_cleaning.R",
                               "R/data-pipeline/2-data_aggregation.R",
                               "R/data-pipeline/helpers.R"))
walk(pipeline_functions, source)

# Load data stored locally -----
base_data <- readRDS(paste0(base, "data/processed/df_base.RDS"))
fup_data <- readRDS(paste0(base, "data/processed/df_fup.RDS"))
data_dictionary <- readRDS(paste0(base, "data/data_dictionary.RDS"))
col_labels <- get_column_labels()

# log columns are correct
expected_base <- c("id", "date", "organisation", "age", "sex", "governorate",
              "role", "height", "weight_prewar", "weight", "children_feeding")
log$base_cols_missing <- setdiff(expected_base, colnames(base_data))
expected_fup <- c("id", "date", "weight")
log$fup_cols_missing <- setdiff(expected_fup, colnames(fup_data))

# add basic stats to log for checking
log$raw_n_baseline <- length(unique(base_data$id))
log$raw_n_followup <- length(unique(fup_data$id))
log$max_date <- max(fup_data$date)
log$orgs <- unique(base_data$organisation)

# Clean data ------------------------------------------------------------
# combine baseline and follow up data; calculate BMI and change
data_id_daily <- clean_data(base_data, fup_data)
# Recode factors
data_id_daily <- data_id_daily |>
  mutate(bmi_category_daily = bmi,
         bmi_category_prewar = bmi_prewar)
suppressWarnings(
  {data_id_daily <- set_factors(df = data_id_daily,
                                 factor_levels = data_dictionary,
                                 factor_cols = c(names(data_dictionary),
                                                 "bmi_category_daily",
                                                 "bmi_category_prewar"))
  }
)
# Data quality ------------------------------------------------------------
# Replace anomaly measurements as missing
data_id_daily <- data_id_daily |>
  mutate(across(contains(c("weight", "bmi")),
                ~ if_else(weight_anomaly=="anomaly", NA, .x)))

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

# check last record dates by organisation
latest_date <- as.Date(max(data_id_last$date, na.rm = TRUE))
log$date_id_last <- count(data_id_last, organisation, date)
# set date of last record to the future to use as a flag that this is the most recent record
#   (noting all group calculations include date so will not be double-counted)
data_id_last <- data_id_last |>
  mutate(date = Sys.Date() + 3650)

# table summaries -----------------------------------------------------
# tally characteristics across all participants
log$factor_count <- count_factors(data_id_daily)

# tabulate all participants by organisation
log$tab_baseline <- data_id_last |>
  mutate(across(where(is.factor), ~ fct_drop(.x))) |>
  filter(!grepl("Overall", organisation)) |>
  tabulate_baseline(by_group = "organisation",
                    col_labels = col_labels)
# tab by follow up
log$tab_followup <- data_id_last |>
  mutate(across(where(is.factor), ~ fct_drop(.x))) |>
  filter(!grepl("Overall", organisation)) |>
  mutate(participant_in_followup = if_else(participant_in_followup,
                                      "In follow up",
                                      "Baseline only")) |>
  tabulate_baseline(by_group = "participant_in_followup",
                    col_labels = col_labels)

# BMI categories
log$tab_bmi_categories <- bmi_crosstab(data_id_last, col_labels)

# strata summaries ----------------------------------------------------
# bind last available record with full time series --
data_id_aggregate <- bind_rows(data_id_daily, data_id_last)
# TODO create the "group" and "label" column (at end of data_agg script) here.

# summarise by date, organisation, and group -----
# Create 2 levels of stratification
group_cols <- c("age", "children_feeding", "governorate", "role", "sex", "participant_in_followup")
group_cols <- combn(group_cols, 2, simplify = FALSE)
group_cols <- append(group_cols, as.list(c("overall", "age",
                                           "children_feeding", "governorate",
                                           "role", "sex")))
group_cols <- append(map(group_cols,
                         ~ c("date", "organisation", sort(.x))),
                     map(group_cols,
                         ~ c("date", sort(.x))))

#' Do not print all messages
suppressMessages({
    summary <- imap(group_cols,
                    ~ data_id_aggregate |>
                      summarise_strata(group_cols = .x)) |>
      clean_aggregated_data()
  })

# save ----------------------
output_file = sprintf("%s/data/public/summary-stats.RDS", .args["wd"])
saveRDS(summary, output_file)

output_log = sprintf("%s/data/public/log.RDS", .args["wd"])
saveRDS(log, output_log)
# RDS data pushed to Github public repo
