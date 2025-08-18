#...............................................................................
### ++ MONITORING THE NUTRITIONAL STATUS OF HUMANITARIAN WORKERS IN GAZA +++ ###
#...............................................................................

#...............................................................................
## ------ R SCRIPT TO GENERATE DUMMY DATASET FOR PIPELINE DEVELOPMENT  ------ ##
#...............................................................................



#...............................................................................
### Preparatory steps
#...............................................................................

  #...................................
  ## Install or load required R packages
  if (!"pacman" %in% rownames(installed.packages())){install.packages("pacman")}

    # Install or load packages from CRAN
    pacman::p_load(
      here,
      ggplot2,       # Visualise data
      ggpubr,        # Arrange multiple plots into a single plot
      ggrepel,       # Improve labelling of plots
      MASS,          # Implement various statistical methods
      mgcv,          # Fit generalised additive models
      scales,        # Scale and format data for visualisation
      tidyverse,     # Tidyverse suite of packages
      viridis)       # Colour-blind palette

  #...................................
  ## Starting setup

    # Clean up from previous code / runs
    rm(list=ls(all=TRUE) )

    # Set font for Windows or Mac
    # suppressWarnings(windowsFonts(Arial = windowsFont("Arial")))
    # suppressWarnings(par(family = "Arial"))

    # Initialise random numbers
    set.seed(123)

    # Colour-blind palette for graphing
      # general palette
      palette_gen <- viridis(16)
      show_col(palette_gen)


#...............................................................................
### Generating dummy dataset
#...............................................................................

  #...................................
  ## Set parameters
    dates <- seq.Date(as.Date("2025-07-28"), as.Date("2025-08-05"), by = 1)
    n_staff <- 200


  #...................................
  ## Generate baseline questionnaire data

    # Initialise dataset
    df_base <- data.frame(id = paste0("id", 1:n_staff))
    vars <- c("date", "organisation", "age", "sex", "governorate", "role",
      "height", "weight_prewar", "weight")
    df_base[, vars] <- NA

    # Fill in baseline characteristics randomly
    df_base$date <- sample(dates, n_staff, replace = T,
      prob = c(0.46, 0.2, 0.1, rep(0.04, 6)))
    df_base$organisation <- c("Save the Children International", "UNRWA")
    df_base$age <- round(runif(n_staff, min = 18, max = 60), 0)
    df_base$sex <- sample(c("male", "female", "other/prefer not to share"),
      n_staff, replace = T, prob = c(0.6, 0.35, 0.05))
    df_base$governorate <- sample(c("North Gaza", "Gaza City", "Deir al Balah",
      "Khan Younis", "Rafah"), n_staff, replace = T, prob = rep(0.2, 5))
    df_base$role <- sample(c("expatriate", "national - contracted",
      "national = casual worker", "other"), n_staff, replace = T,
      prob = c(0.1, 0.5, 0.3, 0.1))
    df_base$children_feeding <- sample(c("1", "2", "3+"), n_staff, replace = T,
                                      prob = c(0.5, 0.3, 0.2))

    # Fill in baseline anthropometry randomly
    x <- which(df_base$sex == "male")
    df_base[x, "height"] <- round(rnorm(length(x), mean = 170, sd = 10), 0)
    df_base[x, "weight_prewar"] <- round(rnorm(length(x), mean = 83, sd = 8), 1)
    x <- which(df_base$sex != "male")
    df_base[x, "height"] <- round(rnorm(length(x), mean = 160, sd = 9), 0)
    df_base[x, "weight_prewar"] <- round(rnorm(length(x), mean = 62, sd = 6), 1)
    df_base$weight <- round(df_base$weight_prewar *
        (1 - rnorm(n_staff, mean = 0.17, sd = 0.05)), 1)

  #...................................
  ## Generate follow-up data

    # Initialise follow-up dataset
    df_fup <- expand.grid(id = unique(df_base$id), date = dates)
    df_fup$weight <- NA
    df_fup <- df_fup[order(df_fup$id, df_fup$date), ]

    # Fill in follow-up weights
    for (i in unique(df_base$id)) {

      # date range of observation series for this staff member
      x <- as.Date(df_base[which(df_base$id == i), "date"])
      x <- dates[which(dates == x) : length(dates)]

      # grab series
      x <- which(df_fup$id == i & df_fup$date %in% x)
      df_i <- df_fup[x, ]

      # grab starting weight
      weight_start_i <- df_base[which(df_base$id == i), "weight"]

      # if there is at least 1 follow-up date available...
      if (nrow(df_i) > 1) {
        for (j in 2:nrow(df_i)) {
          if (j == 2) {
            df_i[j, "weight"] <- weight_start_i *
              (1 - rnorm(1, mean = 0.001, sd = 0.0003))
          }
          if (j > 2) {
            df_i[j, "weight"] <- df_i[j-1, "weight"] *
              (1 - rnorm(1, mean = 0.001, sd = 0.0003))
          }
        }
      }

      # update weights
      df_fup[x, "weight"] <- df_i$weight
    }

    # Round weights and create gaps in data
    df_fup$weight <- round(df_fup$weight, 1)
    for (i in unique(df_fup$id)) {
      x <- which(df_fup$id == i)
      df_fup[x, "weight"] <- df_fup[x, "weight"] * rbinom(length(x), 1, 0.7)
    }
    df_fup[which(df_fup$weight == 0), "weight"] <- NA

  #...................................
  ## Save data

    if (!dir.exists(here("data", "processed"))) {
      dir.create(here("data", "processed"))
    }

    # Baseline dataset
    saveRDS(df_base, here("data", "processed", "df_base.RDS"))
    saveRDS(df_fup, here("data", "processed", "df_fup.RDS"))


#...............................................................................
### ENDS
#...............................................................................
