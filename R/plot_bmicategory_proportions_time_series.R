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
if (!"pacman" %in% rownames(installed.packages())){install.packages("pacman")}

# Install or load packages from CRAN
pacman::p_load(
  ggplot2,       # Visualise data
  tidyverse,     # Tidyverse suite of packages
  viridis)       # Colour-blind palette

#...................................      
## Starting setup

# Clean up from previous code / runs
rm(list=ls(all=TRUE) )

# Set working directory to where this file is stored
dir_path <- paste(dirname(rstudioapi::getActiveDocumentContext()$path  )
                  , "/", sep = "")
setwd(dir_path)
print( getwd() )

#...............................................................................
### Read in bmi category proportions data
#...............................................................................

bmicategory_proportions_filename <- "bmicategory_proportions.csv"
bmicategory_proportions <- read.csv(bmicategory_proportions_filename)

#...............................................................................
### Plot
#...............................................................................

plot_bmicategory_proportions_time_series <- function(data, option = c("Overall", "Sex", "Age Group", "Governorate", "Role")){
  
  # Filter data for the selected option
  data <- data %>% 
    filter(Stratification == option) %>% 
    mutate(category = factor(category, levels = c("Underweight", "Normal", "Overweight", "Obese")))
  
  # Generate plot
  fig <- data %>% 
    ggplot() +
    geom_area(aes(x = date, y = perc, fill = category, group = category), 
              position = "fill", alpha = 0.8) +
    scale_fill_viridis_d(option = "D") +
    labs(x = "Date", 
         y = "Percentage of Survey Participants (%)",
         fill = "Category") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
    facet_grid(cols = vars(Group), row = vars(Stratification), switch = "y", labeller = label_wrap_gen(width = 25), scales = "free") 
  
  return(fig)
  
}