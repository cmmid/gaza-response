# Plot % change in BMI from pre-war, among participants reporting each week
#...................................
# Install or load packages from CRAN
pacman::p_load(ggplot2, dplyr, scales)

#...............................................................................
source(here("R", "data-analysis/ggplot_theme.R"))
plot_weekly_percentage_change <- function(plot_data) {

  plot_data <- plot_data |>
    filter(variable == "bmi_change_percent_prewar") |>
    pivot_wider(names_from = stat, values_from = value) %>%
    filter(!is.na(median)) |>
    mutate(date = as.Date(date))

  # Adapt data to plot text label N participants
  plot_data_change <- expand_grid(date = seq.Date(from = min(plot_data$date) - 7,
                                               to = max(plot_data$date),
                                               by = 7),
                               Stratum = unique(plot_data$Stratum)) |>
    left_join(plot_data) |>
    mutate(n_text = ifelse(date == min(date), "N=",
                           ifelse(is.na(cohort_id_recorded), 0,
                                  cohort_id_recorded)))

  # plot
    plot <- plot_data_change |>
      ggplot(aes(x = date)) +
      geom_linerange(aes(ymin = q1, ymax = q3),
                     col = lshtm_palette$lshtm_generic,
                     linewidth = 2, alpha = 0.8) +
      geom_point(aes(y = median),
                 col = lshtm_palette$lshtm_generic,
                 size = 2.5, alpha = 0.8) +
      geom_text(aes(y = 2.5, label = n_text),
                vjust = 0.5, alpha = 0.6, size = 3, col = "#01454f") +
      geom_hline(aes(yintercept = 0), lty = 2, alpha = 0.1) +
      labs(x = NULL, y = NULL) +
      scale_y_continuous(labels = scales::label_percent(scale = 1),
                         limits = c(NA, 3),
                         minor_breaks = c(NA, 0, NA)) +
      scale_x_date(limits = c(min(plot_data$date) - 7,
                              floor_date(Sys.Date(), "week")),
                   date_breaks = "2 week", date_minor_breaks = "1 week",
                   date_labels = "%d %b") +
      facet_wrap(~Stratum,
                 scales = "fixed") +
      theme(lshtm_theme())

  return(plot)

}
