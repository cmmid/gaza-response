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
source(here("R", "data-analysis/ggplot_theme.R"))
plot_bmicategory_proportions_time_series <- function(plot_data){

  # get % BMI category per week
  plot_data_bmi <- plot_data |>
    filter(grepl("bmi_category_daily", variable)) |>
    mutate(`BMI category` = factor(bmi_category,
                  levels = c("Underweight", "Normal", "Overweight", "Obese"),
                                 ordered = TRUE)) |>
    filter(stat == "count") |>
    mutate(`BMI %` = round(value / cohort_id_recorded * 100),
           `Total weekly participants` = cut(cohort_id_recorded, c(0,10,30,50,Inf),
                              labels = c("<10", "10-20", "20-50", ">50"),
                              ordered_result = TRUE)) |>
    ungroup()

  # ggplot - stacked bar % by date
  plot <- ggplot(plot_data_bmi,
                 aes(x = as.Date(date), y = `BMI %`,
                     fill = `BMI category`,
                     alpha = `Total weekly participants`)) +
    geom_col(position = "stack") +
    scale_y_continuous(labels = scales::label_percent(scale = 1), n.breaks = 3) +
    scale_fill_manual(name = "BMI category",
                      values = lshtm_palette$bmi_categories) +
    scale_alpha_discrete(range = c(0.1,0.7)) +
    labs(x = NULL, y = "% weekly measurements",
         alpha = NULL) +
    guides(alpha = "none") +
    facet_wrap(~ Stratum, scales = "free_y") +
    theme(lshtm_theme())

  return(plot)
}
