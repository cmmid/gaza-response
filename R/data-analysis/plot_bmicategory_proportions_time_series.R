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
  viridis)       # Colour-blind palette

#...............................................................................
### Read in bmi category proportions data
#...............................................................................

# bmicategory_proportions_filename <- "bmicategory_proportions.csv"
# bmicategory_proportions <- read.csv(bmicategory_proportions_filename)

#...............................................................................
### Plot
#...............................................................................

plot_bmicategory_proportions_time_series <- function(data, strata = "Overall"){

  # Filter data for the selected option
  data <- data %>%
    filter(!Group == "other/prefer not to share") %>% #Manually filtering this out per Francescos advice
    mutate(Group = factor(Group, levels = datadict[[strata]])) %>%
    filter(Stratification == strata) %>%
    mutate(category = factor(category, levels = c("Underweight", "Normal", "Overweight", "Obese")))

  # Generate plot
  fig <- data %>%
    ggplot() +
    geom_area(aes(x = Date, y = perc, fill = category, group = category),
              position = "stack", alpha = 0.8) +
    scale_fill_viridis_d(option = "D") +
    labs(x = "Date",
         y = "Percentage of Survey Participants (%)",
         fill = "Category") +
    #theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
          strip.text.y = element_blank()) +
    facet_grid(cols = vars(Group), row = vars(Stratification), switch = "y", labeller = label_wrap_gen(width = 25), scales = "free")


  if (strata == "Overall") {
    fig <- fig + theme(strip.text.x = element_blank())
  }
  return(fig)

}
