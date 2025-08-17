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
### Read in current summary statistics data
#...............................................................................

# current_summary_stats_filename <- "current_summary_stats.csv"
# current_summary_stats <- read.csv(current_summary_stats_filename)

#...............................................................................
### Plot
#...............................................................................

plot_current_summary_stats <- function(data, strata = "overall"){

  # Filter data for the selected option
  data_filter <- filter(data, group == tolower(strata)) |>
    pivot_wider(names_from = stat, values_from = value) %>%
    dplyr::filter(!is.na(mean)) |>
    filter(!grepl("_firstmeasurement", variable))

  data_filter <- recode_data_table(data_filter)

  if (strata == "overall") {
    # Generate plot
    fig <- data_filter %>%
      ggplot(aes(y = 0)) +
      geom_errorbarh(aes(xmin = q1, xmax = q3), height = 0, color = "darkblue", linewidth = 0.5) +
      geom_point(aes(x = median), color = "darkblue", size = 3) +
      geom_point(aes(x = mean), color = "darkred", size = 3, shape = 4, stroke = 1) +
      facet_wrap(~variable, ncol = 2,
                 scales = "free", labeller = label_wrap_gen(width = 25)) +
      labs(x = "Value",
           caption = "Blue point = median; X = mean; blue line = IQR") +
      #theme_bw() +
      theme(#strip.background = element_blank(),
        #strip.text  = "outside",
        axis.text.y  = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank())
  }

  else {
    fig <- data_filter %>%
      ggplot(aes(y = factor(label))) +
      geom_errorbarh(aes(xmin = q1, xmax = q3, color = label), show.legend = F, height = 0, linewidth = 0.5) +
      geom_point(aes(x = median, col = label), show.legend = F, size = 3) +
      geom_point(aes(x = mean, col = label), show.legend = F, size = 3, shape = 4, stroke = 1) +
      facet_wrap(~variable, ncol = 2,
                 scales = "free_x", labeller = label_wrap_gen(width = 25)) +
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


  return(fig)

}
