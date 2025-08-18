#...............................................................................
### ++ MONITORING THE NUTRITIONAL STATUS OF HUMANITARIAN WORKERS IN GAZA +++ ###
#...............................................................................

#...............................................................................
## ------ R SCRIPT TO GENERATE PLOTS OF BMI CATEGORY PROPORTIONS TIME SERIES ----- ##
#...............................................................................

#...............................................................................
### Preparatory steps
#...............................................................................

#...................................
## Install or load required R packages
pacman::p_load(
  lubridate,
  ggplot2,       # Visualise data
  tidyverse,     # Tidyverse suite of packages
  viridis)       # Colour-blind palette

#...............................................................................
### Read in bmi category proportions data
#...............................................................................

# bmicategory_proportions_filename <- "bmicategory_proportions.csv"
# bmicategory_proportions <- read.csv(bmicategory_proportions_filename)

#...............................................................................
### Plot
#...............................................................................

plot_bmicategory_proportions_time_series <- function(data,
                                                     strata = "Overall"){

  bmi_categories <- c("underweight", "normal", "overweight", "obese")
  names(bmi_categories) <- stringr::str_to_title(bmi_categories)

  # Filter data for the selected option
  data_bmi <- data[[tolower(strata)]] |>
    # check to exclude dummy date ("latest")
    filter(date <= Sys.Date() &
             !is.na(date)) |>
    # daily data (ie drop prewar)
    filter(grepl("bmi_category_daily", variable)) |>
    mutate(bmi_category = str_remove_all(variable,
                                         "bmi_category_daily_"),
           bmi_category = na_if(bmi_category, "NA"),
           bmi_category = ordered(bmi_category,
                                  levels = bmi_categories,
                                  labels = names(bmi_categories)),
           # add week
           week = lubridate::floor_date(date, "week")
           ) |>
    # only keep counts
    filter(stat == "count") |>
    group_by(organisation, week, group, label, bmi_category) |>
    summarise(
      category_count = sum(value, na.rm = TRUE),
      .groups = "drop") |>
    group_by(organisation, week, group, label) |>
    mutate(category_total = sum(category_count),
           category_proportion = category_count / category_total) |>
    ungroup()

  # ggplot - stacked bar % by week
  plot <- ggplot(data_bmi,
                 aes(x = as.Date(week),
                     fill = bmi_category, col = bmi_category)) +
    geom_col(aes(y = category_proportion),
             position = "stack") +
    geom_text(aes(y = 1,
                  label = paste0("N=",category_total),
                  colour = "black", vjust = -0.5)) +
    scale_y_continuous(limits = c(NA, 1.1), breaks = c(0,0.25,0.5,0.75,1),
                       labels = scales::label_percent(accuracy = 1)) +
    scale_fill_manual(name = "BMI category",
                      values = lshtm_palette$bmi_categories,
                      aesthetics = c("col", "fill")) +
    labs(x = NULL, y = "Weekly participant measurements") +
    facet_wrap(~ label, scales = "free_y")

  return(plot)
}
