library(dplyr)

# Set Up patient_status dataframe ----
patient_status <- tibble::tribble(
  ~"subject_id", ~"cohort", ~"status", ~"time_start", ~"time_end", ~"alive",
  1, "Cohort A", "On Study", 0, 5, TRUE,
  1, "Cohort A", "Off Study", 5, 12, TRUE,
  2, "Cohort A", "On Study", 0, 5, TRUE,
  2, "Cohort A", "Off Study", 5, 6, TRUE,
  3, "Cohort A", "On Study", 0, 5, FALSE,
  3, "Cohort A", "Off Study", 5, 14, FALSE,
  4, "Cohort B", "On Study", 0, 5, TRUE,
  4, "Cohort B", "Off Study", 5, 7, TRUE,
  5, "Cohort A", "On Study", 0, 5, FALSE,
  5, "Cohort A", "Off Study", 5, 8, FALSE,
  6, "Cohort A", "On Study", 0, 5, FALSE,
  7, "Cohort A", "On Study", 0, 8, TRUE,
  8, "Cohort A", "On Study", 0, 2, TRUE,
  9, "Cohort B", "On Study", 0, 5, TRUE,
  10, "Cohort B", "On Study", 0, 8, TRUE
) |>
  mutate(
    subject_id = factor(subject_id)
  )

# Set up adverse_events dataframe ----
adverse_events <- tibble::tribble(
  ~"subject_id", ~"adverse_event_name", ~"time_of_event",
  1, "Psychiatric Disorder", 5,
  1, "Psychiatric Disorder", 12,
  1, "Infection", 10,
  1, "Infection", 18,
  2, "Cardiac Disorder", 3,
  2, "Cardiac Disorder", 12,
  3, "Psychiatric Disorder", 6,
  3, "Psychiatric Disorder", 12,
  3, "Infection", 8,
  3, "Infection", 22,
  4, "Psychiatric Disorder", 2,
  4, "Cardiac Disorder", 15,
  7, "Psychiatric Disorder", 5,
  9, "Infection", 5
) |>
  mutate(
    subject_id = factor(subject_id)
  )


# Set up medication_administration dataframe ----
medication_administration <- tibble::tribble(
  ~"subject_id", ~"medication", ~"name", ~"time_of_event",
  1, "\U274C", "Medication B", 2,
  1, "\U274C", "Medication B", 21,
  2, "\U2705", "Medication A", 5,
  2, "\U2705", "Medication A", 15,
  4, "\U274C", "Medication B", 7,
  4, "\U274C", "Medication B", 14,
  7, "\U2705", "Medication A", 3,
  9, "\U274C", "Medication B", 3
) |>
  mutate(
    subject_id = factor(subject_id)
  )


# Apply edits to patient_status and join on arrow status ----
# Below added to help with documentation and reduction of repeated code:
patient_status <- patient_status |>
  tidyr::pivot_longer(c(time_start, time_end)) |>
  mutate(
    .by = subject_id,
    time_sorting = sum(value)
  ) |>
  arrange(cohort, time_sorting) |>
  mutate(subject_id = factor(subject_id, levels = unique(subject_id)))

# Make final patient_data list ----
patient_data <- list(
  patient_status = patient_status,
  adverse_events = adverse_events,
  medication_administration = medication_administration
)

usethis::use_data(patient_data, overwrite = TRUE)
usethis::use_data(patient_status, overwrite = TRUE)
usethis::use_data(adverse_events, overwrite = TRUE)
usethis::use_data(medication_administration, overwrite = TRUE)
