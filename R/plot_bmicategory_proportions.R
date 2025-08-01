#...............................................................................
### ++ MONITORING THE NUTRITIONAL STATUS OF HUMANITARIAN WORKERS IN GAZA +++ ###
#...............................................................................

#...............................................................................
## ------ R SCRIPT TO GENERATE PLOTS OF CURRENT BMI CATEGORY PROPORTIONS ----- ##
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

plot_bmicategory_proportions <- function(data, option = c("Overall", "Sex", "Age Group", "Governorate", "Role")){

  # Filter data for the selected option
  data <- data %>%
    filter(Stratification == option, date == max(date)) %>%
    mutate(category = factor(category, levels = c("Underweight", "Normal", "Overweight", "Obese")))

  # Generate plot
  fig <- data %>%
    ggplot() +
    geom_bar(aes(x = category, y = perc, fill = category), stat = "identity") +
    scale_fill_viridis_d(option = "D") +
    labs(x = "WHO BMI Category",
         y = "Percentage of Survey Participants (%)") +
    theme_bw() +
    theme(legend.position = "none",
          plot.caption = element_text(hjust = 0, face = "italic")) +
    facet_grid(cols = vars(Group), row = vars(Stratification), switch = "y", labeller = label_wrap_gen(width = 25), scales = "free")

  return(fig)

}
