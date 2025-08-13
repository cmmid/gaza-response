# Monitoring the nutritional status of humanitarian workers in Gaza

This project provides real-time information on the nutritional status of humanitarian staff in Gaza. We are collecting a set of baseline information from each participant, and a daily optional weight measurement. 

We aim to produce a dashboard with aggregated results, stratified by organisation, and by key characteristics (age group, sex, children dependents, staff role, governorate location). See [study protocol](/protocol/protocol.docx) for full analysis objectives. 

## Overview

<img width="958" height="274" alt="image" src="https://github.com/user-attachments/assets/796f3952-cabb-465d-b848-6a3692966596" />


## Data processing

The dataset is longitudinal, meaning that multiple weight measures will be collected for the same individuals, with a unique ID linking sequential observations to each individual (staff member). 
For details of data collection, see the study protocol.

Baseline variables are: 

- "id", "date" , "organisation", "age", "sex", "governorate", "role", "height", "weight_prewar", "weight", "children_feeding"

Follow up variables are:

- "id", "date", "weight"

Data are stored on a local server. 
The local machine pulls from this public Github repo. 
This contains code to clean and aggregate the data ready for visualisation, in [R/data-pipeline](./R/data-pipeline). 

The result is then pushed back to this public Github repository. 

### Data analysis

This repository contains Quarto documents to analyse aggregated data, and create a website using Github Pages.

The central file for the analysis pipeline is: 

- [index.qmd](index.qmd)

This draws on code for processing and plotting the aggregated data, stored in

- [R/data-analysis](./R/data-analysis)

### Website

The website is rendered and deployed using [Github Actions](./.github/workflows).

The published website is at: https://cmmid.github.io/gaza-response/

- Website settings are saved in [`_quarto.yml`](_quarto.yml)
- We have English and Arabic dashboards under development. Both dashboards should keep an identical structure and visual output.
- Source files are:
  - [English quarto](index.qmd)
  - [Arabic quarto](arabic.qmd)
- Analytics are tracked with [goatcounter](https://gaza-response.goatcounter.com/settings/users). An admin will need to add your email to access this.


