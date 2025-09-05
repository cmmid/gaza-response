## Production of public data

### Public pipeline

This pipeline reads individual-level data from a local source, cleans, and produces aggregated output saved in `data/public`. 

The public data files include:

- `summary-latest.RDS`
  - a single dataframe for aggregated summary stats across all participants' most recent data
- `summary-timeseries.RDS`
  - a single dataframe for week-by-week aggregations, among any participants presenting each week
- `summary-tables.RDS`
  - tables per organisation for: cross-tabulated BMI, participant demographics, and data quality
  
---

<details>

<summary> Data generation </summary>

Data generation involves: 

- a survey at enrollment for each participant, including: creating a unique participant ID; values for baseline characteristics at study entry (e.g. sex, age, organisation, pre-war weight); and that day's weight measurement. 
- a daily survey, optional for each participant, including: the participant's unique ID; the date; the day's weight measurement.

Data are collected using ODK: see study protocol. Data are held on a secure local server.

This produces two datasets, each updating daily:

- base_data : Participants are identified by a unique `id`. 
   - A single row for each unique participant ID; no duplicated participant IDs; any date from the start of the study to present. 
- fup_data : follow up data for subseqent weight measurements, for participants who opt in to this each day. In this dataframe, the first date for each participant is therefore the first observation after enrollment, i.e. the participant's second measurement. Participant ID is a primary key with base_data.
   - One or more rows for each unique participant ID.
   
</details>
  
---
  
<details>

<summary> Data processing pipeline </summary>

The full pipeline runs on a local VM with the following steps:

- Clones private repo: `wt_monitoring_gaza` on branch `automation`
  - Pulls 2 datasets from ODK
    - Saves to `wt_monitoring_gaza/data/raw`
  - Initial cleaning
  - Saves output to `wt_monitoring_gaza/data/processed`
- Clones public repo: `gaza-response`
   - Runs public pipeline code
      - `gaza-response/R/data-pipeline/run-public-pipeline.R`
        - Reads local files from `wt_monitoring_gaza/data/processed`
        - Cleans
        - Aggregates
        - Saves output to `gaza-response/data/public`
   - Pushes to `gaza-response`
   
</details>

--------

<details>

<summary>Data dictionary</summary>

To view the data dictionary, use:

```r
data_dictionary <- readRDS(here::here("data/data-dictionary.RDS"))
```

Add to the data dictionary with new variable names or factor levels in the data. 
Append the new item as a named character to the [`dictionary` list object](./R/data-pipeline/0-data-dictionary.R), and save as an RDS file.

</details>

--------

<details>

<summary>Working with individual daily data</summary>

To load clean individual level data, using RDS files stored in the `data/processed` directory, use:

``` r
# get functions
source(here::here("R/data-pipeline/0-data-dictionary.R"))
source(here::here("R/data-pipeline/1-data_cleaning.R"))
# load data
baseline_data <- readRDS(here::here("data/processed/df_base.RDS"))
followup_data <- readRDS(here::here("data/processed/df_fup.RDS"))
# clean data
data_dictionary <- set_data_dictionary()
data_id_daily <- clean_data(baseline_data, followup_data, data_dictionary)
```

To get only records for participants with any longitudinal follow up, use:

``` r
data_id_latest <- data_id_daily |>
  filter(participant_in_followup)
```

To get only the latest record for each participant (including those with only one record), use:

``` r
data_id_latest <- data_id_daily |>
  filter(last_measurement)
```

</details>

-----------

<details>

<summary>Testing the website pipeline</summary>

1.  Generate simulated data

```         
source(here::here("R", "sandbox", "gaza_adult_wt_sim.R"))
```

-   This generates two files for baseline and follow up datasets, saved in `data/processed`. These replicate the structure of the confidential data

2.  Run the data processing pipeline

```         
source(here::here("R", "data-pipeline", "run-public-pipeline.R"))
```

-   This cleans and aggregates the data and replicating the code run on the local server. This creates three new RDS files: the pipeline log; summary tables; and the summary data. These are published openly, saved in `data/public`.

3.  Create the dashboard

```         
quarto::quarto_render("index.qmd")
```

</details>
