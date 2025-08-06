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
                               "/2-data_aggregation.R"))
walk(pipeline_functions, source)

# Load data stored locally
base_data <- readRDS(here("data", "processed", "df_base.RDS"))
fup_data <- readRDS(here("data", "processed", "df_fup.RDS"))

# 1. Clean data -----
data_id <- clean_data(base_data, fup_data)

# 2. Aggregate and calculate summaries by stratification -----
# only use 2 levels of stratification for now
group_cols <- c("overall", # specify stratifications
                "sex", "agegroup", "governorate", "role", "children_feeding")
group_cols <- combn(group_cols_vec, 2, simplify = FALSE)
group_cols <- map(group_cols_vec,
                  ~ c("date", "organisation", sort(.x)))
# summarise by date, organisation, and group combination
summary <- map(group_cols,
               ~ data |>
                 summarise_ids(group_cols = .x))
summary_org <- clean_aggregated_data(summary)

# participants reporting in latest 3 day window

# 3. Save locally ----
saveRDS(summary, here("data", "public", "summary-stats.RDS"))

# RDS data pushed to Github public repo
