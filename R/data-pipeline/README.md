**Testing the pipeline**

1. Generate simulated data

```
source(here::here("R", "sandbox", "gaza_adult_wt_sim.R"))
```

- This generates two files for baseline and follow up datasets, saved in `data/processed`. These replicate the structure of the confidential data

2. Run the data processing pipeline 
```
source(here::here("R", "data-pipeline", "run-public-pipeline.R"))
```
- This cleans and aggregates the data and replicating the code run on the local server. This creates two new RDS files, the pipeline log and the summary data that is published openly, saved in `data/public`

3. Create the dashboard
```
quarto::quarto_render("index.qmd")
```
