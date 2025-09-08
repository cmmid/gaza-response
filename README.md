# Monitoring the nutritional status of humanitarian workers in Gaza

This project provides real-time information on the nutritional status of humanitarian staff in Gaza. We are collecting a set of baseline information from each participant, and a daily optional weight measurement. 

We aim to produce a dashboard with aggregated results, stratified by organisation, and by key characteristics (age group, sex, children dependents, staff role, governorate location). See [study protocol](/protocol/protocol.docx) for full analysis objectives. 

The published website is at: [cmmid.github.io/gaza-response](https://cmmid.github.io/gaza-response/)

This repository contains Quarto documents to analyse aggregated data, and create a website using Github Pages.


## Overview

---

<img width="1581" height="733" alt="gaza-response drawio" src="https://github.com/user-attachments/assets/c13df6e1-a4d2-4967-af45-af6f053b8220" />

[Schematic](https://app.diagrams.net/#G1snCTpMD9-Zb3m23dCBqMmGUp-dfgFssn#%7B%22pageId%22%3A%22voqQkcouitH0LyEqQwV9%22%7D)

---

The central file for the website dashboard is:

- [index.qmd](index.qmd)

This draws on code for processing individual data - cleaning and aggregating into summary statistics - and plotting the aggregated data. This code is stored in

- [R/data-pipeline](./R/data-pipeline/README)
- [R/data-analysis](./R/data-analysis/README)

## Data processing

### Data description

The dataset is longitudinal, meaning that multiple weight measures will be collected for the same individuals, with a unique ID linking sequential observations to each individual (staff member). 
For details of data collection, see the study [protocol](./protocol/protocol.docx).

Baseline variables are: 

- "id", "date" , "organisation", "age", "sex", "governorate", "role", "height", "weight_prewar", "weight", "children_feeding"

Follow up variables are:

- "id", "date", "weight"

### Data pipeline

Individual data are confidential. 
Individual level data are anonymised and processed in a contained environment on a virtual machine.
All code to process the individual data is openly available on this public Github repo.

The sequence for updating the data pipeline is:

- The VM receives the latest data
- At hourly intervals:
  - Pulls this repository from Github
  - Accesses the data from a local directory
  - Runs the [data pipeline](./R/data-pipeline/run-data-pipeline.R) to clean and aggregate data
  - Saves the aggregated data in the [data/public](./data/public) directory
  - Pushes changes to this repository back to Github

 See the README for more detail: [R/data-pipeline](./R/data-pipeline/README).

## Website

The published website is at: [cmmid.github.io/gaza-response](https://cmmid.github.io/gaza-response/).
The website is rendered and deployed using [Github Actions](./.github/workflows).

### Website creation

The dashboard relies on a sequence of quarto documents nested within `index.qmd`.
Each nested document is stored in the [`quarto`](./quarto) directory.

The hierarchy of documents is:

- `index.qmd`: overall dashboard frame that includes tabs for each organisation
  - `_organisation.qmd`: a template page for each organisation, that includes the following sub-tabs:
    - `_tab-key-insights.qmd`: an overall summary per organisation
    - `_tab-strata.qmd`: a template for summarising data by demographic stratification, per organisation
    - `_tab-participation.qmd`: an overall summary of participant characteristics per organisation

### Website maintenance

- The website is rendered and deployed using [Github Actions](./.github/workflows)
  - See manually dispatched workflow: [publish.yml](./.github/workflows/publish.yml)]
- Website settings are saved in [`_quarto.yml`](_quarto.yml)
- Analytics are tracked with [goatcounter](https://gaza-response.goatcounter.com/settings/users). 
An admin will need to add your email to access this.
