#...............................................................................
### ++ MONITORING THE NUTRITIONAL STATUS OF HUMANITARIAN WORKERS IN GAZA +++ ###
#...............................................................................

#...............................................................................
## ------ R SCRIPT TO GENERATE PLOTS OF CURRENT SUMMARY STATISTICS ----- ##
#...............................................................................
# Protocol  for analysis:
# - [x] Mean, median and inter-quartile range absolute and percent reduction in weight and body mass index (BMI) from pre-war baseline.
# - [ ] Proportion of staff with BMI in different WHO categories: underweight <18.5, normal 18.50 – 24.99, overweight ≥ 25, obese ≥
# - [ ] Trends in the mean, median and inter-quartile range weight, BMI and percent weight change, by date.
#...............................................................................
### Preparatory steps
#...............................................................................

#...................................
## Install or load required R packages
pacman::p_load(
  ggplot2,       # Visualise data
  tidyverse)     # Tidyverse suite of packages

#...............................................................................
### Plot
#...............................................................................
source(here("R", "data-analysis/ggplot_theme.R"))

plot_current_summary_stats <- function(data_latest, strata = "overall"){

  # Filter data for the selected option
  plot_data <- data_latest[[tolower(strata)]]
  # set caption with N participants
  plot_data <- plot_data |>
    mutate(strata_n = paste0(label, ": N=", cohort_id_recorded))
  caption_n <- paste0("Median and 25-75% range. ",
                      paste0(unique(plot_data$strata_n), collapse = "; "))
  # reshape and rename
  plot_data <- plot_data |>
    pivot_wider(names_from = stat, values_from = value) %>%
    filter(!is.na(median)) |>
    filter(grepl("_prewar", variable)
           #& !grepl("bmi_category_", variable)
           ) |>
    # rename "variable" column (= new column: variable_name)
    rename_with_dictionary(data_dictionary, long = TRUE) |>
    # rename columns
    rename_with_dictionary(data_dictionary, long = FALSE)

    plot <- plot_data %>%
      group_by(variable_name) |>
      ggplot(aes(y = Strata, col = Strata)) +
      geom_linerange(aes(xmin = q1, xmax = q3),
                     alpha = 0.3,
                     linewidth = 2,
                     show.legend = F) +
      geom_point(aes(x = median),
                 alpha = 0.8,
                 size = 2, shape = 15,
                 show.legend = F) +
      geom_vline(aes(xintercept = 0), lty = 2,alpha = 0.1) +
      scale_colour_discrete(l = 40) +
      scale_x_continuous(limits = c(NA, 0)) +
      facet_wrap(~variable_name,
                 nrow = 2,
                 scales = "free_x",
                 labeller = label_wrap_gen(width = 25)) +
      labs(x = NULL,
           caption = caption_n) +
      theme(axis.title.y = element_blank()) +
      lshtm_theme()

  return(plot)

}

plot_bmi_categories <- function() {
  bmi_cat <- plot_data |>
    filter(stat == "count") |>
    mutate(bmi_period = as_factor(bmi_period)) |>
    group_by(label) |>
    expand(bmi_period, bmi_category) |>
    drop_na(bmi_period) |>
    left_join(plot_data) |>
    mutate(category_percent = value / cohort_id_recorded,
           category_percent = if_else(is.na(category_percent), 0, category_percent))

  bmi_cat <- plot_data |>
    filter(grepl("bmi_category_", variable)) |>


    pivot_wider(names_from = bmi_period, values_from = value)

  select(label, starts_with("bmi_category_"), value) |>
    right_join(expand(plot_data, label, bmi_period, bmi_category)) |>
    filter(!is.na(bmi_period)) |>
    mutate(value = if_else(is.na(value), 0, value)) |>
    filter(!is.na(bmi_category)) |>
    pivot_wider(names_from = bmi_period, values_from = value)

  bmi_cat |>
    ggplot(aes(x = bmi_category_daily, y = bmi_category_prewar)) +
    geom_tile(aes(fill = Underweight))
}
