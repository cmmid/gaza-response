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
    filter(!is.na(median))

  # plot
    plot <- plot_data |>
      ggplot(aes(x = date)) +
      geom_linerange(aes(ymin = q1, ymax = q3),
                     col = lshtm_palette$lshtm_generic,
                     linewidth = 2) +
      geom_point(aes(y = median),
                 col = lshtm_palette$lshtm_generic,
                 size = 2.5) +
      geom_text(aes(y = 2.5, label = cohort_id_recorded),
                vjust = 0.5, alpha = 0.5, size = 3) +
      geom_hline(aes(yintercept = 0), lty = 2, alpha = 0.1) +
      labs(x = NULL, y = NULL) +
      scale_y_continuous(labels = scales::label_percent(scale = 1),
                         limits = c(NA, 5),
                         minor_breaks = c(NA, 0, NA)) +
      scale_x_date(date_breaks = "2 week", date_minor_breaks = "1 week",
                   date_labels = "%d %b")
      facet_wrap(~Stratum,
                 scales = "fixed") +
      theme(lshtm_theme())

  return(plot)

}
