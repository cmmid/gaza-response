
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
  ggplot2,       # Visualise data
  tidyverse,     # Tidyverse suite of packages
  viridis,
  plotly)       # Colour-blind palette

#...............................................................................
### Read in bmi category proportions data
#...............................................................................

# bmicategory_proportions_filename <- "bmicategory_proportions.csv"
# bmicategory_proportions <- read.csv(bmicategory_proportions_filename)

#...............................................................................
### Plot
#...............................................................................




plot_bmicategory_proportions_time_series <- function(data, strata = "Overall") {
  # Filter and prepare the data
  data <- data %>%
    mutate(Group = factor(Group, levels = datadict[[strata]])) %>%
    filter(Stratification == strata) %>%
    mutate(category = factor(category, levels = c("Underweight", "Normal", "Overweight", "Obese"))) %>%
    group_by(Date, Group, Stratification) %>%
    mutate(perc_norm = perc / sum(perc)) %>%
    ungroup()

  if (strata == "Overall") {
    fig <- plot_ly(data,
                   x = ~Date,
                   y = ~perc,
                   color = ~category,
                   colors = viridis_pal(option = "D")(length(unique(data$category))),
                   type = 'scatter',
                   mode = 'none',
                   stackgroup = 'one',
                   fill = 'tonexty',
                   text = ~paste0(
                     "Date: ", Date, "<br>",
                     "Category: ", category, "<br>",
                     "%: ", round(perc, 2), "%"),
                   hoverinfo = "text",
                   legendgroup = ~category,
                   showlegend = TRUE,
                   name = ~category) %>%
      layout(
        showlegend = TRUE,
        yaxis = list(title = 'Percentage of Survey Participants (%)'),
        xaxis = list(title = '', tickangle = -45, tickformat = "%b %d")
      )
  } else {
    # Split into facets
    facet_data <- split(data, data$Group)
    facet_data <- facet_data[names(facet_data) != "other/prefer not to answer"] # Exclude unwanted group
    # Create one stacked chart per Group
    plots <- lapply(seq_along(facet_data), function(i) {
      df <- facet_data[[i]]
      plot_ly(df,
              x = ~Date,
              y = ~perc,
              color = ~category,
              colors = viridis_pal(option = "D")(length(levels(df$category))),
              type = 'scatter',
              mode = 'none',
              stackgroup = 'one',
              fill = 'tonexty',
              text = ~paste0(
                "Date: ", Date, "<br>",
                "Category: ", category, "<br>",
                "%: ", round(perc, 2), "%"),
              hoverinfo = "text",
              legendgroup = ~category,
              showlegend = (i == 1),
              name = ~category) %>%
        layout(
          showlegend = TRUE,
          yaxis = list(title = 'Percentage of Survey Participants (%)'),
          xaxis = list(title = '', tickangle = -45, tickformat = "%b %d")
        )
    })
    # Create subplot layout
    fig <- subplot(plots,
                   nrows = 1,
                   margin = 0.05,
                   shareY = TRUE,
                   shareX = TRUE,
                   titleX = TRUE,
                   titleY = TRUE)
    # Add group titles as annotations
    annotations <- lapply(seq_along(plots), function(i) {
      list(
        x = (i - 0.5) / length(plots),
        y = 1.05,
        text = names(facet_data)[i],
        xref = "paper",
        yref = "paper",
        showarrow = FALSE,
        font = list(size = 12)
      )
    })
    fig <- fig %>%
      layout(
        annotations = annotations,
        # yaxis = list(title = "Percentage of Survey Participants"),
        xaxis = list(title = "Date", tickangle = -45, tickformat = "%b %d"),
        legend = list(title = list(text = "Category"))
      )
  }

  return(fig)
}
