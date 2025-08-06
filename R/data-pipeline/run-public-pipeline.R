# Run the full data processing pipeline on a local server hosting raw data, ready to be pushed to public Github

# 0. Set up -------
pacman::p_load(here, purrr, dplyr)

# Load the pipeline functions, locally or from github
if(interactive()) {
  base <- here("R", "data-pipeline")
  } else {
  base <- "https://raw.githubusercontent.com/cmmid/gaza-response/main/R/data-pipeline" }

pipeline_functions <- paste0(base,
                             c("/1-data_cleaning.R",
                               "/2-data_aggregation.R",
                               "/3-data-quality.R"))
walk(pipeline_functions, source)

# Load data stored locally
base_data <- readRDS(here("data", "processed", "df_base.RDS"))
fup_data <- readRDS(here("data", "processed", "df_fup.RDS"))

# 1. Clean data -----
data_id <- clean_data(base_data, fup_data)

# 2. Aggregate and calculate summaries by stratification -----
group_cols <- c("overall", # specify stratifications
                "sex", "agegroup", "governorate", "role", "children_feeding")

# all time
summary_all <- summarise_ids(data = data_id,
                             group_cols = group_cols)

# participants reporting in latest 3 day window

# 3. Save locally ----
saveRDS(current_summary_stats_table, here("results",
                                            "current_summary_stats.RDS"))
# RDS data pushed to Github public repo
