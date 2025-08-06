#...............................................................................
### ++ MONITORING THE NUTRITIONAL STATUS OF HUMANITARIAN WORKERS IN GAZA +++ ###
#...............................................................................

#...............................................................................
## ------ R SCRIPT TO ANALYSE DATA AND MAKE TABLES FOR VISUALISATIONS   ----- ##
#...............................................................................

# Install or load packages from CRAN
pacman::p_load(
  ggplot2,       # Visualise data
  ggpubr,        # Arrange multiple plots into a single plot
  ggrepel,       # Improve labelling of plots
  MASS,          # Implement various statistical methods
  mgcv,          # Fit generalised additive models
  scales,        # Scale and format data for visualisation
  tidyverse,     # Tidyverse suite of packages
  viridis,
  lubridate)       # Colour-blind palette

#...................................
## Starting setup

# Set font for Windows or Mac
# suppressWarnings(windowsFonts(Arial = windowsFont("Arial")))
suppressWarnings(par(family = "Arial"))

if(interactive()){
  # Clean up from previous code / runs
  rm(list=ls(all=TRUE) )

  # Set working directory to where this file is stored
  dir_path <- paste(dirname(rstudioapi::getActiveDocumentContext()$path  )
                    , "/", sep = "")
  setwd(dir_path)
} else {
  #do not show summarise message unless in interactive session
  options(dplyr.summarise.inform = FALSE)
}

# suppressWarnings(dir.create(paste0(dir_path, "out")))

# Initialise random numbers
set.seed(123)

# Get todays date
today <- as.Date(Sys.time())

#...............................................................................
### Read in data
#...............................................................................

data_table <- readRDS("./data/cleaned/matched_data.RDS")

organisations <- as.character(unique(data_table$organisation))

#...............................................................................
##### Utility Functions ####
#...............................................................................
special_group_by <- function(df, group_col) {
  if (group_col == "Overall") {
    return(df)  # Ungrouped
  } else {
    return(df %>% group_by(.data[[group_col]]))
  }
}

special_group_by_date <- function(df, group_col) {
  if (group_col == "Overall") {
    return(df %>% group_by(.data[["date"]]))  # Ungrouped
  } else {
    return(df %>% group_by(across(all_of(c("date", group_col)))))
  }
}

special_group_by_bmicat <- function(df, group_col) {
  if (group_col == "Overall") {
    return(df %>% group_by(across(all_of(c("date", "category")))))  # Ungrouped
  } else {
    return(df %>% group_by(across(all_of(c("date", "category", group_col)))))
  }
}

#...............................................................................
##### Table generating functions ####
#...............................................................................

generate_most_recent_stats <- function(data_table, stratification) {
  summary_table <- data_table %>%
    dplyr::filter(date > (today - 7)) %>% # The slide to just the last week so it's recent
    dplyr::group_by(id) %>%
    dplyr::filter(date == max(date)) %>%
    dplyr::ungroup() %>%
    special_group_by(stratification) %>%
    dplyr::summarise(Weight_mean = mean(weight, na.rm = TRUE),
                     Weight_median = median(weight, na.rm = TRUE),
                     Weight_q1 = quantile(weight, probs = 0.25, na.rm = TRUE),
                     Weight_q3 = quantile(weight, probs = 0.75, na.rm = TRUE),

                     BMI_mean = mean(BMI, na.rm = TRUE),
                     BMI_median = median(BMI, na.rm = TRUE),
                     BMI_q1 = quantile(BMI, probs = 0.25, na.rm = TRUE),
                     BMI_q3 = quantile(BMI, probs = 0.75, na.rm = TRUE),

                     PercentChangeFirstMeasurement_mean = mean(percent_change_firstmeasurement, na.rm = TRUE),
                     PercentChangeFirstMeasurement_median = median(percent_change_firstmeasurement, na.rm = TRUE),
                     PercentChangeFirstMeasurement_q1 = quantile(percent_change_firstmeasurement, probs = 0.25, na.rm = TRUE),
                     PercentChangeFirstMeasurement_q3 = quantile(percent_change_firstmeasurement, probs = 0.75, na.rm = TRUE),

                     PercentChangePreWar_mean = mean(percent_change_prewar, na.rm = TRUE),
                     PercentChangePreWar_median = median(percent_change_prewar, na.rm = TRUE),
                     PercentChangePreWar_q1 = quantile(percent_change_prewar, probs = 0.25, na.rm = TRUE),
                     PercentChangePreWar_q3 = quantile(percent_change_prewar, probs = 0.75, na.rm = TRUE)) %>%
    tidyr::pivot_longer(cols = matches("_")) %>%
    tidyr::separate(name, into = c("Variable", "Stat"), sep = "_") %>%
    tidyr::pivot_wider(names_from = Stat, values_from = value) %>%
    dplyr::mutate(Variable = recode(Variable,
                                    "Weight" = "Weight (kg)",
                                    "PercentChangePreWar" = "% Weight Change from Prewar Value",
                                    "PercentChangeFirstMeasurement" = "% Weight Change from First Measurement")) %>%
    dplyr::mutate(Variable = factor(Variable, levels = c("Weight (kg)", "BMI", "% Weight Change from First Measurement", "% Weight Change from Prewar Value")),
                  Stratification = stratification)

  return(summary_table)
}

generate_time_series_table <- function(data_table, stratification) {
  time_series_table <- data_table %>%
    special_group_by_date(stratification) %>%
    dplyr::summarise(Weight_mean = mean(weight, na.rm = TRUE),
                     Weight_median = median(weight, na.rm = TRUE),
                     Weight_q1 = quantile(weight, probs = 0.25, na.rm = TRUE),
                     Weight_q3 = quantile(weight, probs = 0.75, na.rm = TRUE),

                     BMI_mean = mean(BMI, na.rm = TRUE),
                     BMI_median = median(BMI, na.rm = TRUE),
                     BMI_q1 = quantile(BMI, probs = 0.25, na.rm = TRUE),
                     BMI_q3 = quantile(BMI, probs = 0.75, na.rm = TRUE),

                     PercentChangeFirstMeasurement_mean = mean(percent_change_firstmeasurement, na.rm = TRUE),
                     PercentChangeFirstMeasurement_median = median(percent_change_firstmeasurement, na.rm = TRUE),
                     PercentChangeFirstMeasurement_q1 = quantile(percent_change_firstmeasurement, probs = 0.25, na.rm = TRUE),
                     PercentChangeFirstMeasurement_q3 = quantile(percent_change_firstmeasurement, probs = 0.75, na.rm = TRUE),

                     PercentChangePreWar_mean = mean(percent_change_prewar, na.rm = TRUE),
                     PercentChangePreWar_median = median(percent_change_prewar, na.rm = TRUE),
                     PercentChangePreWar_q1 = quantile(percent_change_prewar, probs = 0.25, na.rm = TRUE),
                     PercentChangePreWar_q3 = quantile(percent_change_prewar, probs = 0.75, na.rm = TRUE)) %>%
    tidyr::pivot_longer(cols = matches("_")) %>%
    tidyr::separate(name, into = c("Variable", "Stat"), sep = "_") %>%
    tidyr::pivot_wider(names_from = Stat, values_from = value) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(Variable = recode(Variable,
                                    "Weight" = "Weight (kg)",
                                    "PercentChangePreWar" = "% Weight Change from Prewar Value",
                                    "PercentChangeFirstMeasurement" = "% Weight Change from First Measurement")) %>%
    dplyr::mutate(Variable = factor(Variable, levels = c("Weight (kg)", "BMI", "% Weight Change from First Measurement", "% Weight Change from Prewar Value")),
                  Stratification = stratification)

  return(time_series_table)
}

generate_bmicategory_proportions_table <- function(data_table, stratification) {
  bmicategories_proportions_table <- data_table %>%
    dplyr::mutate(category = case_when(BMI < 18.5 ~ "underweight",
                                       BMI >= 18.5 & BMI < 25 ~ "normal",
                                       BMI >= 25 & BMI < 30 ~ "overweight",
                                       BMI >= 30 ~ "obese",
                                       TRUE ~ NA_character_)) %>%
    special_group_by_bmicat(stratification) %>%
    summarise(n = n_distinct(id), .groups = "drop") %>%
    special_group_by_date(stratification) %>%
    mutate(perc = n/sum(n)*100,
           category = factor(category,
                             levels = c("underweight", "normal", "overweight", "obese"),
                             labels = c("Underweight", "Normal", "Overweight", "Obese")),
           Stratification = stratification)

  return(bmicategories_proportions_table)
}


#...............................................................................
##### Stats from most recent measurements ####
#...............................................................................

current_summary_stats_table <- rbind(dplyr::mutate(generate_most_recent_stats(data_table, "Overall"), Group = NA),
                                     dplyr::rename(generate_most_recent_stats(data_table, "sex"), Group = sex),
                                     dplyr::rename(generate_most_recent_stats(data_table, "agegroup"), Group = agegroup),
                                     dplyr::rename(generate_most_recent_stats(data_table, "governorate"), Group = governorate),
                                     dplyr::rename(generate_most_recent_stats(data_table, "role"), Group = role)) %>%
  dplyr::mutate(Organisation = "Overall")

for (o in organisations) {
  data_table_filtered <- dplyr::filter(data_table, organisation == o)
  tmptab <- rbind(dplyr::mutate(generate_most_recent_stats(data_table_filtered, "Overall"), Group = NA),
                  dplyr::rename(generate_most_recent_stats(data_table_filtered, "sex"), Group = sex),
                  dplyr::rename(generate_most_recent_stats(data_table_filtered, "agegroup"), Group = agegroup),
                  dplyr::rename(generate_most_recent_stats(data_table_filtered, "governorate"), Group = governorate),
                  dplyr::rename(generate_most_recent_stats(data_table_filtered, "role"), Group = role)) %>%
    dplyr::mutate(Organisation = o)
  current_summary_stats_table <- rbind(current_summary_stats_table, tmptab)
}


write.csv(current_summary_stats_table, "./results/current_summary_stats.csv", row.names = F)

#...............................................................................
##### Calculate trends over time ####
#...............................................................................


time_series_table <- rbind(dplyr::mutate(generate_time_series_table(data_table, "Overall"), Group = NA),
                           dplyr::rename(generate_time_series_table(data_table, "sex"), Group = sex),
                           dplyr::rename(generate_time_series_table(data_table, "agegroup"), Group = agegroup),
                           dplyr::rename(generate_time_series_table(data_table, "governorate"), Group = governorate),
                           dplyr::rename(generate_time_series_table(data_table, "role"), Group = role)) %>%
  dplyr::mutate(Organisation = "Overall")

for (o in organisations) {
  data_table_filtered <- dplyr::filter(data_table, organisation == o)
  tmptab <- rbind(dplyr::mutate(generate_time_series_table(data_table_filtered, "Overall"), Group = NA),
                  dplyr::rename(generate_time_series_table(data_table_filtered, "sex"), Group = sex),
                  dplyr::rename(generate_time_series_table(data_table_filtered, "agegroup"), Group = agegroup),
                  dplyr::rename(generate_time_series_table(data_table_filtered, "governorate"), Group = governorate),
                  dplyr::rename(generate_time_series_table(data_table_filtered, "role"), Group = role)) %>%
    dplyr::mutate(Organisation = o)
  time_series_table <- rbind(time_series_table, tmptab)
}


write.csv(time_series_table, "./results/time_series_table.csv", row.names = F)

#...............................................................................
##### Calculate BMI Proportions over time ####
#...............................................................................
bmicategory_proportions <- rbind(dplyr::mutate(generate_bmicategory_proportions_table(data_table, "Overall"), Group = NA),
                                 dplyr::rename(generate_bmicategory_proportions_table(data_table, "sex"), Group = sex),
                                 dplyr::rename(generate_bmicategory_proportions_table(data_table, "agegroup"), Group = agegroup),
                                 dplyr::rename(generate_bmicategory_proportions_table(data_table, "governorate"), Group = governorate),
                                 dplyr::rename(generate_bmicategory_proportions_table(data_table, "role"), Group = role)) %>%
  dplyr::mutate(Organisation = "Overall")

for (o in organisations) {
  data_table_filtered <- dplyr::filter(data_table, organisation == o)
  tmptab <- rbind(dplyr::mutate(generate_bmicategory_proportions_table(data_table_filtered, "Overall"), Group = NA),
                  dplyr::rename(generate_bmicategory_proportions_table(data_table_filtered, "sex"), Group = sex),
                  dplyr::rename(generate_bmicategory_proportions_table(data_table_filtered, "agegroup"), Group = agegroup),
                  dplyr::rename(generate_bmicategory_proportions_table(data_table_filtered, "governorate"), Group = governorate),
                  dplyr::rename(generate_bmicategory_proportions_table(data_table_filtered, "role"), Group = role)) %>%
    dplyr::mutate(Organisation = o)
  bmicategory_proportions <- rbind(bmicategory_proportions, tmptab)
}


write.csv(bmicategory_proportions, "./results/bmicategory_proportions.csv", row.names = F)
