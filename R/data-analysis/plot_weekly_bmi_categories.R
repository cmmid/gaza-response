# Plot BMI categories week by week

# Install or load required R packages
pacman::p_load(lubridate, ggplot2, dplyr, forcats, scales)
source(here("R", "data-analysis/ggplot_theme.R"))

# Plot
plot_weekly_bmi_categories <- function(plot_data){

  # get % BMI category per week
  plot_data <- plot_data |>
    filter(grepl("bmi_category_daily", variable)) |>
    mutate(`BMI category` = factor(bmi_category,
                  levels = c("Underweight", "Normal", "Overweight", "Obese"),
                                 ordered = TRUE)) |>
    filter(stat == "count") |>
    mutate(`BMI %` = round(value / cohort_id_recorded * 100),
           date = as.Date(date)) |>
    ungroup()

  # Adapt data to plot text label N participants:
  # - add a dummy week to hold "N=" before data
  # - expand to explicitly include weeks with 0 counts
  plot_data_bmi <- expand_grid(date = seq.Date(from = min(plot_data$date) - 7,
                                  to = max(plot_data$date),
                                  by = 7),
                       Stratum = unique(plot_data$Stratum)) |>
    left_join(plot_data) |>
    mutate(n_text = ifelse(date == min(date), "N=",
                           ifelse(is.na(cohort_id_recorded), 0,
                                  cohort_id_recorded)))

  # ggplot - stacked bar % by date
  plot <- plot_data_bmi |>
    ggplot(aes(x = date)) +
    geom_col(aes(y = `BMI %`,
                 fill = `BMI category`),
             position = "stack") +
    geom_text(aes(y = 105, label = n_text),
              vjust = 0.5, alpha = 0.6, size = 3, col = "#01454f") +
    scale_y_continuous(labels = scales::label_percent(scale = 1),
                       limits = c(0, 105), breaks = c(0, 50, 100)) +
    scale_x_date(limits = c(min(plot_data$date) - 7,
                            floor_date(Sys.Date(), "week")),
                 date_breaks = "2 week", date_minor_breaks = "1 week",
                 date_labels = "%d %b") +
    scale_fill_manual(name = "BMI category",
                      values = lshtm_palette$bmi_categories) +
    labs(x = NULL, y = "% weekly measurements") +
    facet_wrap(~ Stratum, scales = "free_y") +
    theme(lshtm_theme())

  return(plot)
}
