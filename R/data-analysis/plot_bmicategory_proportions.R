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
source(here("R", "data-analysis/ggplot_theme.R"))
plot_bmicategory_proportions <- function(data, strata = "overall"){

  # Filter data for the selected option
  data <- data %>%
    filter(!Group == "other/prefer not to share") %>% #Manually filtering this out per Francescos advice
    filter(Stratification == strata, Date == max(Date)) %>%
    mutate(category = factor(category, levels = c("Underweight", "Normal", "Overweight", "Obese")))

  # Generate plot
  fig <- data %>%
    ggplot() +
    geom_bar(aes(x = category, y = perc, fill = category), stat = "identity") +
    geom_text(aes(x = category, y = perc, label = round(perc)), vjust = -0.5) +
    scale_fill_viridis_d(option = "D") +
    labs(x = "WHO BMI Category",
         y = "Percentage of Survey Participants (%)") +
    theme_bw() +
    theme(legend.position = "none",
          plot.caption = element_text(hjust = 0, face = "italic")) +
    facet_grid(cols = vars(Group), row = vars(Stratification), switch = "y", labeller = label_wrap_gen(width = 25), scales = "free")

  return(fig)

}
