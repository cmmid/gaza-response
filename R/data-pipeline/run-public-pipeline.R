# Run the full data processing pipeline on a local server hosting raw data, ready to be pushed to public Github

# ------------------ Set up ------------------------------------
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
strata <- c("overall",
                "age", "children_feeding", "governorate", "role", "sex")
log$strata <- strata
group_cols <- map(strata,
                  ~ c("date", "organisation", sort(.x)))
# Create 2 levels of stratification
# group_cols <- combn(group_cols, 2, simplify = FALSE)
# group_cols <- append(group_cols, as.list(c("overall", "age",
#                                            "children_feeding", "governorate",
#                                            "role", "sex")))
# group_cols <- append(group_cols, map(group_cols,
#                          ~ c("date", "organisation", sort(.x))))

# SUMMARISE LATEST  ---------------------------------------------
# summarise across whole sample using each participants' latest obs ---
# create a df with only last recorded observation by ID
data_id_latest <- data_id_daily |>
  filter(last_measurement)
log$data_latest$date_of_latest <- count(data_id_latest, organisation, date)

## Reset "date" to a single common date (so avoid any grouping by daily date)
data_id_latest <- data_id_latest |>
  mutate(date_observation = date,
         date = max(data_id_latest$date, na.rm=TRUE))

# # tabulate all participants by demography & data quality
tables_latest <- tabulate_study(data_id_latest, data_dictionary)
log$data_latest$summary_tables <- tables_latest$Overall

# Summarise strata at latest record
suppressMessages({
  summary_cohort_all <- map(group_cols,
                 ~ data_id_latest |>
                   summarise_strata(group_cols = .x)) |>
    clean_aggregated_data()
})
log$summary_cohort_all$date_of_latest <- count(data_id_latest, organisation, date_observation)
log$summary_cohort_all$overall_sample <- slice_sample(summary_cohort_all$Overall$overall,
                                                        prop = 0.1)
# SUMMARISE DAILY CROSS-SECTION  ---------------------------------------------
# summarise sample at each daily timestep ---------------
suppressMessages({
  summary_cohort_daily <- map(group_cols,
                    ~ data_id_daily |>
                      summarise_strata(group_cols = .x)) |>
      clean_aggregated_data()
  })
log$summary_cohort_daily$date_of_latest <- count(data_id_latest, organisation, date)
log$summary_cohort_daily$overall_sample <- slice_sample(summary_cohort_daily$Overall$overall,
                                                       prop = 0.1)

#  --------------------- Save ----------------------
output_tables = sprintf("%s/data/public/summary-tables.RDS",.args["wd"])
saveRDS(tables_latest, output_tables)

output_all = sprintf("%s/data/public/summary-cohort-all.RDS", .args["wd"])
saveRDS(summary_cohort_all, output_all)

output_daily = sprintf("%s/data/public/summary-cohort-daily.RDS", .args["wd"])
saveRDS(summary_cohort_daily, output_daily)

output_log = sprintf("%s/data/public/log.RDS", .args["wd"])
saveRDS(log, output_log)
# RDS data pushed to Github public repo
