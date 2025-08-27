#...............................................................................
### ++ MONITORING THE NUTRITIONAL STATUS OF HUMANITARIAN WORKERS IN GAZA +++ ###
#...............................................................................

#...............................................................................
## ------ R SCRIPT TO GENERATE PLOTS OF TIME SERIES SUMMARY STATISTICS ----- ##
#...............................................................................

#...............................................................................
### Preparatory steps
#...............................................................................

#...................................
# Install or load packages from CRAN
pacman::p_load(
  ggplot2,       # Visualise data
  tidyverse)     # Tidyverse suite of packages

### Plot

# set caption with N participants
# caption_n <- plot_data |>
#   group_by(Stratum) |>
#   summarise(strata_n = sum(cohort_id_new)) |>
#   mutate(strata_n = paste0(Stratum, ": N=", strata_n))
# caption <- paste0("Showing percentage change in units of Body Mass Index (BMI) from self-reported weight before the war, to most recent measurement during study period. \n ",
#                     paste0(unique(caption_n$strata_n), collapse = "; "))

#...............................................................................
source(here("R", "data-analysis/ggplot_theme.R"))
plot_time_series_statistics <- function(plot_data) {

  plot_data <- plot_data |>
    filter(variable == "bmi_change_percent_prewar") |>
    pivot_wider(names_from = stat, values_from = value) %>%
    filter(!is.na(median)) |>
    mutate(Participants = cut(cohort_id_recorded, c(0,10,30,50,100),
                   labels = c("<10", "10-20", "20-50", ">50"),
                   ordered_result = TRUE))

  # plot
    plot <- plot_data |>
      ggplot(aes(x = date)) +
      geom_linerange(aes(ymin = q1, ymax = q3,
                     alpha = Participants),
                     position = position_dodge(1),
                     linewidth = 2) +
      geom_point(aes(y = median,
                     alpha = Participants),
                 position = position_dodge(1),
                 size = 2) +
      scale_alpha_discrete(range = c(0.1,0.7)) +
      geom_hline(aes(yintercept = 0), lty = 2, alpha = 0.1) +
      labs(x = NULL, y = NULL) +
      scale_y_continuous(minor_breaks = c(NA, 0, NA)) +
      facet_wrap(~Stratum,
                 scales = "fixed", ncol=1) +
      #gghighlight() +
      theme(lshtm_theme())

  return(plot)

}

plotly_time_series <- function() {

  # Define alpha values for each participant group
  participant_alpha <- c("<10" = 0.1, "10-20" = 0.3, "20-50" = 0.5, ">50" = 0.7)

  plot_data <- plot_data |>
    filter(variable == "bmi_change_percent_prewar") |>
    pivot_wider(names_from = stat, values_from = value) |>
    filter(!is.na(median)) |>
    mutate(
      Participants = cut(cohort_id_recorded, c(0,10,30,50,100),
                         labels = c("<10", "10-20", "20-50", ">50"),
                         ordered_result = TRUE),
      # Add alpha mapping directly to the data
      alpha_value = participant_alpha[as.character(Participants)]
    )

  plot_data_list <- split(plot_data, plot_data$stratum)

  subplot_list <- map(plot_data_list,
                      ~ plot_data_list$Female |>
                        plot_ly() |>
    # Add IQR line ranges
    add_segments(
      x = ~date, xend = ~date,
      y = ~q1, yend = ~q3,
      color = ~Participants,
      colors = c("#0000FF19", "#0000FF4D", "#0000FF80", "#0000FFB3"),
      line = list(width = 4),
      showlegend = FALSE,
      hovertemplate = "Date: %{x}<br>Q1: %{y}<br>Q3: %{yend}<br>Participants: %{customdata}<extra></extra>",
      customdata = ~Participants
    ) |>
    # Add median points
    add_markers(
      x = ~date,
      y = ~median,
      color = I("blue"),
      size = I(8),
      showlegend = FALSE,
      hovertemplate = "Date: %{x}<br>Median: %{y}<br>Participants: %{customdata}<extra></extra>",
      customdata = ~Participants
    ) |>
    layout(
      xaxis = list(title = ""),
      yaxis = list(title = "")
    )
  )

    plot <- subplot(subplot_list, nrows = length(stratum),
                    shareX = TRUE)

  plot

}

create_bmi_plot_simple <- function(plot_data) {

  # Create alpha mapping
  participant_range <- range(plot_data$Participants, na.rm = TRUE)
  plot_data$alpha <- 0.1 + (plot_data$Participants - participant_range[1]) /
    (participant_range[2] - participant_range[1]) * 0.6

  # Create the plot
  fig <- plot_ly(plot_data)

  # Add traces for each stratum
  for(strat in unique(plot_data$Stratum)) {
    data_subset <- plot_data |>
      filter(Stratum == strat)

    # Add error bars
    fig <- fig |>
      add_trace(
        data = data_subset,
        type = 'scatter',
        mode = 'markers',
        x = ~date,
        y = ~median,
        error_y = list(
          type = 'data',
          symmetric = FALSE,
          array = ~(q3 - median),
          arrayminus = ~(median - q1),
          color = 'rgba(0,0,0,0.3)',
          thickness = 2,
          width = 0
        ),
        marker = list(
          size = 8,
          color = ~alpha,
          colorscale = list(
            c(0, 'rgba(0,0,0,0.1)'),
            c(1, 'rgba(0,0,0,0.7)')
          ),
          showscale = FALSE
        ),
        name = strat,
        legendgroup = strat,
        showlegend = TRUE,
        hovertemplate = paste0(
          strat, "<br>",
          "Date: %{x|%Y-%m-%d}<br>",
          "Median: %{y:.2f}<br>",
          "<extra></extra>"
        )
      )
  }

  # Update layout
  fig <- fig |>
    layout(
      xaxis = list(title = ""),
      yaxis = list(
        title = "BMI Change",
        zeroline = TRUE,
        zerolinecolor = "rgba(0,0,0,0.2)"
      ),
      plot_bgcolor = "white",
      paper_bgcolor = "white",
      hovermode = "closest"
    )

  return(fig)
}

create_bmi_plot <- function(plot_data, lshtm_theme = NULL) {

  # Get unique strata for faceting
  strata <- unique(plot_data$Stratum)

  # Create alpha mapping based on Participants
  # Map participant counts to alpha values between 0.1 and 0.7
  participant_range <- range(plot_data$Participants, na.rm = TRUE)
  plot_data$alpha <- 0.1 + (plot_data$Participants - participant_range[1]) /
                            (participant_range[2] - participant_range[1]) * 0.6

  # Initialize plot list for subplots
  plot_list <- list()

  # Create a plot for each stratum
  for(i in seq_along(strata)) {
    stratum_data <- plot_data |>
      filter(Stratum == strata[i])

    # Create base plot for this stratum
    p <- plot_ly()

    # Add horizontal reference line at 0
    p <- p |>
      add_trace(
        type = "scatter",
        mode = "lines",
        x = c(min(stratum_data$date), max(stratum_data$date)),
        y = c(0, 0),
        line = list(
          dash = "dash",
          color = "rgba(0,0,0,0.1)",
          width = 1
        ),
        showlegend = FALSE,
        hoverinfo = "skip"
      )

    # Add error bars (linerange equivalent)
    for(j in 1:nrow(stratum_data)) {
      p <- p |>
        add_trace(
          type = "scatter",
          mode = "lines",
          x = c(stratum_data$date[j], stratum_data$date[j]),
          y = c(stratum_data$q1[j], stratum_data$q3[j]),
          line = list(
            color = paste0("rgba(0,0,0,", stratum_data$alpha[j], ")"),
            width = 6  # Equivalent to linewidth = 2 in ggplot2
          ),
          showlegend = FALSE,
          hovertemplate = paste0(
            "Date: %{x|%Y-%m-%d}<br>",
            "IQR: ", round(stratum_data$q1[j], 2), " to ",
            round(stratum_data$q3[j], 2), "<br>",
            "Participants: ", stratum_data$Participants[j],
            "<extra></extra>"
          ),
          name = ""
        )
    }

    # Add median points
    p <- p |>
      add_trace(
        type = "scatter",
        mode = "markers",
        x = stratum_data$date,
        y = stratum_data$median,
        marker = list(
          size = 8,  # Equivalent to size = 2 in ggplot2
          color = paste0("rgba(0,0,0,", stratum_data$alpha, ")")
        ),
        showlegend = FALSE,
        hovertemplate = paste0(
          "Date: %{x|%Y-%m-%d}<br>",
          "Median: %{y:.2f}<br>",
          "Participants: ", stratum_data$Participants,
          "<extra></extra>"
        ),
        name = ""
      )

    # Add title annotation for facet
    p <- p |>
      layout(
        annotations = list(
          list(
            text = strata[i],
            x = 0.5,
            y = 1,
            xref = "paper",
            yref = "paper",
            xanchor = "center",
            yanchor = "bottom",
            showarrow = FALSE,
            font = list(size = 12, color = "black")
          )
        )
      )

    plot_list[[i]] <- p
  }

  # Combine all plots as subplots
  combined_plot <- subplot(
    plot_list,
    nrows = length(strata),
    shareX = TRUE,
    shareY = TRUE,
    titleY = TRUE,
    margin = 0.05
  )

  # Apply overall layout settings
  combined_plot <- combined_plot |>
    layout(
      xaxis = list(
        title = "",
        showgrid = TRUE,
        gridcolor = "rgba(0,0,0,0.1)"
      ),
      yaxis = list(
        title = "BMI Change",
        showgrid = TRUE,
        gridcolor = "rgba(0,0,0,0.1)",
        zeroline = TRUE,
        zerolinecolor = "rgba(0,0,0,0.2)"
      ),
      plot_bgcolor = "white",
      paper_bgcolor = "white",
      margin = list(l = 50, r = 20, t = 40, b = 40),
      showlegend = FALSE
    )

  return(combined_plot)
}
