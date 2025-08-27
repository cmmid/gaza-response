#...............................................................................
### ++ MONITORING THE NUTRITIONAL STATUS OF HUMANITARIAN WORKERS IN GAZA +++ ###
#...............................................................................

#...............................................................................
## ------ R SCRIPT TO GENERATE PLOTS OF CURRENT SUMMARY STATISTICS ----- ##
#...............................................................................

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

plot_current_summary_stats <- function(plot_data, stratum = NULL){

  if (!is.null(stratum)) {
    plot_data <- filter(plot_data, stratum == {{ stratum }})
  }

  # set caption with N participants
  plot_data <- plot_data |>
    mutate(stratum_n = paste0(Stratum, ": N=", cohort_id_recorded))
  caption_n <- paste0("Median and 25-75% range. ",
                      paste0(unique(plot_data$stratum_n), collapse = "; "))
  # reshape and rename
  plot_data <- plot_data |>
    pivot_wider(names_from = stat, values_from = value) %>%
    filter(!is.na(median)) |>
    filter(grepl("_prewar", variable)
           #& !grepl("bmi_category_", variable)
           )

    plot <- plot_data |>
      ggplot(aes(y = Stratum)) +
      geom_linerange(aes(xmin = q1, xmax = q3),
                     alpha = 0.3,
                     linewidth = 2,
                     show.legend = F) +
      geom_point(aes(x = median),
                 alpha = 0.8,
                 size = 2,
                 show.legend = F) +
      geom_vline(aes(xintercept = 0), lty = 2,alpha = 0.1) +
      scale_colour_discrete(l = 40) +
      scale_x_continuous(limits = c(NA, 0)) +
      facet_wrap(~Variable,
                 nrow = 2,
                 scales = "free_x") +
      labs(x = NULL,
           caption = caption_n) +
      theme(axis.title.y = element_blank()) +
      lshtm_theme()

  return(plot)

}

# plot_bmi_categories <- function() {
#   bmi_cat <- plot_data |>
#     filter(stat == "count") |>
#     mutate(bmi_period = as_factor(bmi_period)) |>
#     group_by(stratum) |>
#     expand(bmi_period, bmi_category) |>
#     drop_na(bmi_period) |>
#     left_join(plot_data) |>
#     mutate(category_percent = value / cohort_id_recorded,
#            category_percent = if_else(is.na(category_percent), 0, category_percent))
#
#   bmi_cat <- plot_data |>
#     filter(grepl("bmi_category_", variable)) |>
#
#
#     pivot_wider(names_from = bmi_period, values_from = value)
#
#   select(stratum, starts_with("bmi_category_"), value) |>
#     right_join(expand(plot_data, stratum, bmi_period, bmi_category)) |>
#     filter(!is.na(bmi_period)) |>
#     mutate(value = if_else(is.na(value), 0, value)) |>
#     filter(!is.na(bmi_category)) |>
#     pivot_wider(names_from = bmi_period, values_from = value)
#
#   bmi_cat |>
#     ggplot(aes(x = bmi_category_daily, y = bmi_category_prewar)) +
#     geom_tile(aes(fill = Underweight))
# }
