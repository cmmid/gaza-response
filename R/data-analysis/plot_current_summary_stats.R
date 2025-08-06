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

plot_current_summary_stats <- function(data, strata = "Overall"){

  # Filter data for the selected option
  data <- data %>%
    filter(!Group == "other/prefer not to share") %>% #Manually filtering this out per Francescos advice
    filter(Stratification == strata) %>%
    mutate(Group = factor(Group, levels = datadict[[strata]]),
           Variable = factor(Variable, levels = datadict[["variable"]]))

  if (strata == "Overall") {
    # Generate plot
    fig <- data %>%
      ggplot(aes(y = 0)) +
      geom_errorbarh(aes(xmin = q1, xmax = q3), height = 0, color = "darkblue", linewidth = 0.5) +
      geom_point(aes(x = median), color = "darkblue", size = 3) +
      geom_point(aes(x = mean), color = "darkred", size = 3, shape = 4, stroke = 1) +
      facet_wrap(~Variable, nrow = 1, scales = "free", labeller = label_wrap_gen(width = 25)) +
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
    fig <- data %>%
      ggplot(aes(y = factor(Group))) +
      geom_errorbarh(aes(xmin = q1, xmax = q3, color = Group), show.legend = F, height = 0, linewidth = 0.5) +
      geom_point(aes(x = median, col = Group), show.legend = F, size = 3) +
      geom_point(aes(x = mean, col = Group), show.legend = F, size = 3, shape = 4, stroke = 1) +
      facet_wrap(~Variable, nrow = 1, scales = "free_x", labeller = label_wrap_gen(width = 25)) +
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
