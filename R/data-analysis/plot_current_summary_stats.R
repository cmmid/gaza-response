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
### Read in current summary statistics data
#...............................................................................

# current_summary_stats_filename <- "current_summary_stats.csv"
# current_summary_stats <- read.csv(current_summary_stats_filename)

#...............................................................................
### Plot
#...............................................................................
source(here("R", "data-analysis/ggplot_theme.R"))

plot_current_summary_stats <- function(data, strata = "overall"){

  # Filter data for the selected option
  data_filter <- data[[tolower(strata)]]

  data_filter <- data_filter |>
    pivot_wider(names_from = stat, values_from = value) %>%
    dplyr::filter(!is.na(median)) |>
    filter(grepl("_prewar", variable) & !grepl("bmi_category_", variable)) |>
    filter(!grepl("other|prefer no", label))

    fig <- data_filter %>%
      group_by(indicator) |>
      ggplot(aes(y = label)) +
      geom_linerange(aes(xmin = q1, xmax = q3,
                         color = label),
                     alpha = 0.3,
                     linewidth = 2,
                     show.legend = F) +
      geom_point(aes(x = median, col = label),
                 alpha = 0.8,
                 size = 2, shape = 15,
                 show.legend = F) +
      facet_wrap(~variable,
                 nrow = 2,
                 scales = "free_x",
                 labeller = label_wrap_gen(width = 25)) +
      labs(x = NULL,
           caption = "Median and 25-75% range") +
      theme(axis.title.y = element_blank()) +
      lshtm_theme()

  return(fig)

}
