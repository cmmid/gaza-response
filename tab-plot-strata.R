
plot1 <- plot_current_summary_stats(plot_data_latest) |>
  ggplotly()

plot2 <- plot_time_series_statistics(plot_data = plot_data_timeseries) |>
  ggplotly() |>
  layout(legend = list(orientation = 'h'))

plot3 <- plot_bmicategory_proportions_time_series(plot_data = plot_data_timeseries) |>
  ggplotly(tooltip = c("y", "fill")) |>
  layout(showlegend = FALSE)

