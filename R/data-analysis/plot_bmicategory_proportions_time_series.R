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
  data_filter <- filter(data, group == tolower(strata)) |>
    # filter out the duplicate "current" records
    dplyr::filter(date <= Sys.Date()) %>%
    #dplyr::filter(!sex == "other/prefer not to share") %>%
    pivot_wider(names_from = stat, values_from = value) %>%
    dplyr::filter(is.na(mean)) |>
    filter(!grepl("_prewar_", variable)) |>
    filter(!grepl("NA", variable))

  data_filter <- recode_data_table(data_filter)

  if (strata == "Overall") {
    fig <- data_filter %>%
      ggplot() +
      geom_area(aes(x = date, y = percent, fill = variable, group = variable),
                position = "stack", alpha = 0.8) +
      scale_fill_viridis_d(option = "D") +
      labs(x = "Date",
           y = "Percentage of Survey Participants (%)",
           fill = "Category") +
      #theme_bw() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
            strip.text.y = element_blank())
  } else if (length(unlist(str_split(strata, "-"))) == 1) {
    # Generate plot
    fig <- data_filter %>%
      dplyr::rename(facet1 = all_of(strata)) %>%
      ggplot() +
      geom_area(aes(x = date, y = percent, fill = variable, group = variable),
                position = "stack", alpha = 0.8) +
      scale_fill_viridis_d(option = "D") +
      labs(x = "Date",
           y = "Percentage of Survey Participants (%)",
           fill = "Category") +
      #theme_bw() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
            strip.text.y = element_blank()) +
      facet_wrap(~facet1, labeller = label_wrap_gen(width = 25), scales = "free")

  } else {
    fig <- data_filter %>%
      dplyr::rename(facet1 = all_of(unlist(str_split(strata, "-"))[1]),
                    facet2 = all_of(unlist(str_split(strata, "-"))[2])) %>%
      ggplot() +
      geom_area(aes(x = date, y = percent, fill = variable, group = variable),
                position = "stack", alpha = 0.8) +
      scale_fill_viridis_d(option = "D") +
      labs(x = "Date",
           y = "Percentage of Survey Participants (%)",
           fill = "Category") +
      #theme_bw() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
      facet_grid(facet1~facet2, labeller = label_wrap_gen(width = 25), scales = "free")
  }




  return(fig)

}
