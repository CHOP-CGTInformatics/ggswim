## code to prepare `patient_data` dataset goes here
library(dplyr)

set.seed(123)

patient_status <- tibble::tibble(subject_id = rep(1:10, length.out = 15)) |>
  sample_n(15) |>
  arrange(subject_id) |>
  mutate(
    .by = subject_id,
    cohort = rep(sample(c("Cohort A", "Cohort B"), 1), length.out = n(), each = length(unique(subject_id))),
    cohort = factor(cohort, levels = c("Cohort B", "Cohort A")),
    status = ifelse(duplicated(subject_id) | !duplicated(subject_id, fromLast = TRUE), "On Study", "Off Study"),
    status = factor(status, levels = c("On Study", "Off Study"), ordered = TRUE),
    time_end = ifelse(status == "On Study", runif(n(), 1, 10), runif(n(), 11, 20)),
    time_start = ifelse(status == "On Study", 0, NA),
    time_start = ifelse(is.na(time_start), lead(time_end), time_start)
  ) |>
  arrange(subject_id, status) |>
  relocate(time_start, .before = time_end) |>
  mutate(subject_id = factor(subject_id))

set.seed(1)

adverse_events <- tibble::tibble(subject_id = factor(rep(1:10, length.out = 15))) |>
  sample_n(8) |>
  arrange(subject_id) |>
  mutate(
    adverse_event_name = rep(sample(c("Infection", "Cardiac Disorder", "Psychiatric Disorder")), length.out = n())
  ) |>
  left_join(patient_status, by = "subject_id",
            relationship = "many-to-many") |> # Temp join to reference time vals
  mutate(
    .by = subject_id,
    time_of_event = runif(n = n(), min = time_start, max = time_end)
  ) |>
  select(-c(cohort, status, time_start, time_end)) |>
  unique()

set.seed(1)

medication_administration <- tibble::tibble(subject_id = factor(rep(1:10, length.out = 15))) |>
  sample_n(5) |>
  arrange(subject_id) |>
  mutate(
    medication = rep(sample(c("❌", "✅")), length.out = n()),
    name = ifelse(medication == "✅", "Medication A", "Medication B"),
  ) |>
  left_join(patient_status, by = "subject_id",
            relationship = "many-to-many") |> # Temp join to reference time vals
  mutate(
    .by = subject_id,
    time_of_event = runif(n = n(), min = time_start, max = time_end)
  ) |>
  select(-c(cohort, status, time_start, time_end)) |>
  unique()

patient_data <- list(patient_status = patient_status,
                     adverse_events = adverse_events,
                     medication_administration = medication_administration)

usethis::use_data(patient_data, overwrite = TRUE)
