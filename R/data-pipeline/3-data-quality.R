# Data quality
# TODO - PLACEHOLDER CODE

get_data_quality <- function() {
  df_participants <- data |>
    group_by(across(all_of(group_cols))) |>
    summarise(participants = length(unique(id)),
              observations = n(),
              weight_prewar_missing = sum(is.na(weight_prewar)),
              bmi_anomaly = sum(bmi_anomaly, na.rm = TRUE) /
                participants*100,
              change_anomaly = sum(change_anomaly, na.rm = TRUE)  /
                observations*100,
              .groups = "drop"
    )
  return(df_participants)
}
