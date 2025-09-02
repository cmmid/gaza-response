plot_bmi_categories <- function(plot_data) {

  plot_data <- plot_data |>
    filter(stat == "count") |>
    mutate(bmi_period = as_factor(bmi_period)) |>
    group_by(Stratum) |>
    expand(bmi_period, bmi_category) |>
    drop_na(bmi_category) |>
    left_join(plot_data |>
                select(Stratum, bmi_period, bmi_category,
                       value, cohort_id_recorded)) |>
    mutate(`BMI %` = round(value / cohort_id_recorded * 100),
           `BMI %` = if_else(is.na(`BMI %`), 0, `BMI %`),
           Timepoint = suppressWarnings(fct_rev(fct_recode(
             bmi_period, !!!data_dictionary$data_levels)))) |>
    dplyr::select(Stratum, Timepoint, `BMI category` = bmi_category, `BMI %`)

  plot <- plot_data |>
    ggplot(aes(fill = `BMI category`,
               x = Timepoint, y = `BMI %`)) +
    geom_col(position = position_stack()) +
    scale_y_continuous(labels = scales::label_percent(scale = 1)) +
    scale_fill_manual(values = lshtm_palette$bmi_categories) +
    labs(x = NULL, y = NULL) +
    facet_wrap(~Stratum) +
    theme(lshtm_theme())

  return(plot)

}

# Alternative: waterfall plot, +/- % difference in each category, pre-war to current
plot_bmi_category_difference <- function(plot_data) {

  # get full grid of stratum / bmi / timepoint
  bmi_cats <- plot_data_latest |>
    filter(stat == "count") |>
    expand(stratum, bmi_period, bmi_category) |>
    drop_na(bmi_category)

  plot_data <- bmi_cats |>
    filter(!is.na(bmi_category)) |>
    left_join(plot_data_latest,
                by = c("stratum", "bmi_period", "bmi_category")) |>
    mutate(cohort_id_recorded = if_else(is.na(cohort_id_recorded),
                                        0, cohort_id_recorded),
           percent = round(value / cohort_id_recorded * 100),
           percent = if_else(is.na(percent), 0, percent),
           Timepoint = suppressWarnings(fct_rev(
             fct_recode(bmi_period,
                        !!!data_dictionary$data_levels)))) |>
    dplyr::select(Stratum, Timepoint,
                  bmi_category, percent) |>
    drop_na(Stratum) |>
    pivot_wider(names_from = Timepoint, values_from = `BMI %`) |>
    mutate(Change = `Current BMI` - `Pre-war BMI`)

  plot <- plot_data |>
    ggplot(aes(fill = `BMI category`,
               x = `BMI category`, y = Change)) +
    geom_col(position = position_dodge()) +
    scale_y_continuous(labels = scales::label_percent(scale = 1)) +
    scale_fill_manual(values = lshtm_palette$bmi_categories) +
    labs(x = NULL, y = NULL) +
    facet_wrap(~Stratum)

  return(plot)
}
