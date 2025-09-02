# Simple model for individual level weight change over time
# -------------------------------------------------------------------------
# set up ------------------------------------------------------------------
library(here)
library(purrr)
library(dplyr)
library(ggplot2)
library(patchwork)
library(mgcv)
library(gratia)
public_repo <- here::here()
pipeline_functions <- here(public_repo,
                             c("R/data-pipeline/0-data-dictionary.R",
                               "R/data-pipeline/1-data_cleaning.R"
                             ))
walk(pipeline_functions, source)

# Load individual data -------------------------------------------------
if (!file.exists(here(public_repo, "data/processed/df_base.RDS"))) {
  stop("Missing raw data in data/processed directory")
}

base_data <- readRDS(here(public_repo, "data/processed/df_base.RDS"))
fup_data <- readRDS(here(public_repo, "data/processed/df_fup.RDS"))
data_dictionary <- set_data_dictionary()
# merge and clean baseline and follow up data
data_id_daily <- clean_data(base_data, fup_data) |>
  mutate(id = as_factor(id),
         calendar_date_numeric = as.numeric(date),
         across(contains("change"),
                ~ if_else(participant_timepoint == "Study entry", 0, .x)))

# Longitudinal data since baseline at study entry -------------------------------------
participant_trajectory <- data_id_daily |>
  filter(participant_in_followup)

# plot_followup_time <- participant_trajectory |>
#   ggplot(aes(x = date, y = participant_cumulative_days_recorded,
#              group = id)) +
#   geom_point(aes(col = organisation), alpha = 0.3) +
#   geom_line(aes(col = organisation), alpha = 0.3) +
#   geom_hline(yintercept = 1, lty = 2) +
#   scale_y_continuous(breaks = breaks_pretty()) +
#   labs(x = NULL, y = "Participant follow up") +
#   theme(legend.position = "bottom")

# # Change over time since study entry
# plot_trajectory_records <- participant_trajectory |>
#   ggplot(aes(x = participant_cumulative_days_recorded,
#              y = bmi_change_unit_entry)) +
#   geom_point(aes(group = id, col = organisation), alpha = 0.3) +
#   geom_line(aes(group = id, col = organisation), alpha = 0.3) +
#   geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"),
#               col = "grey40") +
#   geom_hline(yintercept = 0, alpha = 0.8, lty = 2) +
#   scale_x_continuous(breaks = breaks_pretty()) +
#   labs(x = "Participant observation",
#        y = "Change in BMI (kg/m2)") +
#   theme(legend.position = "bottom")


plot_followup_freq <- participant_trajectory |>
  ggplot() +
  geom_histogram(aes(participant_cumulative_days_recorded),
                 binwidth = 1, alpha = 0.6) +
  scale_x_binned(limits = c(0, NA)) +
  labs(x = "Follow up measurements", y = "Participants")

plot_trajectory_calendar <- participant_trajectory |>
  ggplot(aes(x = date,
             y = bmi_change_unit_entry)) +
  geom_point(aes(group = id, col = organisation), alpha = 0.3) +
  geom_line(aes(group = id, col = organisation), alpha = 0.3) +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"),
              col = "grey40") +
  geom_hline(yintercept = 0, alpha = 0.8, lty = 2) +
  labs(x = "Date of observation", y = "Change in BMI (kg/m2)") +
  theme(legend.position = "bottom")

plot_followup_freq +
  plot_trajectory_calendar +
  plot_layout(widths = c(1,2), ncol = 2, guides = "collect", axes = "collect") +
  plot_annotation(caption = paste0(length(unique(participant_trajectory$id)), " participants, ", nrow(participant_trajectory), " observations \n", sum(participant_trajectory$participant_cumulative_days_recorded >= 3), " participants with >= 3 observations"))

participant_trajectory |>
  filter(last_measurement) |>
  ggplot(aes(y = bmi_change_unit_entry,
             col = organisation)) +
  geom_density() +
  coord_flip()

# GAM --------------------------------------------------------------
# calendar time smooth and random intercept for participants
model_calendar <- gam(
  bmi_change_unit_entry ~ s(calendar_date_numeric) + s(id, bs = "re"),
  data = participant_trajectory,
  method = "REML"
)
# Model summary
summary(model_calendar)

# Model diagnostics
gam.check(model_calendar)

# Visualization of smooth effects
par(mfrow = c(2, 2))
plot(model_calendar, pages = 1)

# Predictions and residuals plot
participant_trajectory$fitted <- predict(model_calendar)
participant_trajectory$residuals <- residuals(model_calendar)

# Residuals vs fitted
participant_trajectory |>
  ggplot(aes(x = fitted, y = residuals)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "loess", se = FALSE,
              color = "red") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Residuals vs fitted",
    x = "Fitted BMI change",
    y = "Residuals"
  ) +
  theme_minimal()

# Extract smooth effect for interpretation
smooth_calendar <- smooth_estimates(model_calendar, smooth = "s(date_numeric)")

cat("\n=== INTERPRETATION NOTES ===\n")
cat("1. Effective degrees of freedom for calendar time smooth:",
    summary(model_calendar)$s.table[1, "edf"], "\n")
cat("2. Significance of calendar time effect (p-value):",
    summary(model_calendar)$s.table[1, "p-value"], "\n")
cat("3. Proportion of variance explained (R-sq adj):",
    summary(model_calendar)$r.sq, "\n")
cat("4. Random effect variance (participant ID):",
    gam.vcomp(model_calendar)[1], "\n")

# Function to add covariates when ready
add_covariates_model <- function(data) {
  gam(
    bmi_change_unit_prewar ~ s(date_numeric, k = 10) +
      organisation + sex + age + role + governorate +
      children_feeding +
      s(id_factor, bs = "re"),
    data = data,
    method = "REML"
  )
}


# -------------------------------------------------------------------------

gam_data <- participant_trajectory |>
  select(date, id, date_entry, participant_cumulative_days_enrolled,
         organisation, sex, age, children_feeding, role, governorate,
         bmi_entry, bmi_daily,
         bmi_change_unit_entry, bmi_change_percent_entry,
         bmi_change_percent_daily_rate_entry
         )

sample <- gam_data |>
  filter(id %in% sample(id, 10, replace=TRUE))
write_csv(sample, here("sample.csv"))
