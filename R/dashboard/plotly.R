library(crosstalk)
library(plotly)

shared_current <- SharedData$new(data_filter)
bscols(list(
         filter_checkbox("agegroup", "Age",
                         shared_current, ~ agegroup),
         filter_checkbox("children_feeding", "Dependents",
                         shared_current, ~ children_feeding),
         filter_checkbox("sex", "Sex",
                         shared_current, ~ sex)
       ),
       plotly::ggplotly(
         shared_current |>
         ggplot(aes(y = agegroup)) +
           geom_errorbarh(aes(xmin = q1, xmax = q3, color = agegroup),
                          show.legend = F, height = 0, linewidth = 0.5) +
           geom_point(aes(x = median, col = agegroup),
                      show.legend = F, size = 3) +
           geom_point(aes(x = mean, col = agegroup),
                      show.legend = F, size = 3, shape = 4, stroke = 1) +
           # facet_wrap(~variable, nrow = 1, scales = "free_x",
           #            labeller = label_wrap_gen(width = 25)) +
           labs(x = "Value",
                caption = "X = mean; 0 = median; - = typical range (IQR*)") +
           theme(axis.title.y = element_blank(),
             legend.position = "bottom")
       )
)


# function ----------------------------------------------------------------

plotly_summary <- function(data_filter) {
  data_filter |>
    plotly::ggplotly(
      ggplot(aes(y = factor(label))) +
        geom_errorbarh(aes(xmin = q1, xmax = q3, color = label), show.legend = F, height = 0, linewidth = 0.5) +
        geom_point(aes(x = median, col = label), show.legend = F, size = 3) +
        geom_point(aes(x = mean, col = label), show.legend = F, size = 3, shape = 4, stroke = 1) +
        facet_wrap(~variable, nrow = 1, scales = "free_x", labeller = label_wrap_gen(width = 25)) +
        labs(x = "Value",
             caption = "O = median; X = mean; - = IQR") +
        #theme_bw() +
        theme(#strip.background = element_blank(),
          #strip.placement  = "outside",
          #axis.text.y  = element_blank(),
          #åaxis.ticks.y = element_blank(),
          axis.title.y = element_blank(),
          legend.position = "bottom")
}

# Filter data for the selected option
data_filter <- data[[org]] |>
  list_rbind() |>
  filter(date == max(date, na.rm = TRUE)) |>
  pivot_wider(names_from = stat, values_from = value) |>
  dplyr::filter(!is.na(mean))

data_filter |>

