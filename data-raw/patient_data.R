## code to prepare `patient_data` dataset goes here

set.seed(12345)

patient_status <- tibble::tibble(subject_id = rep(1:10, length.out = 15)) |>
  sample_n(15) |>
  dplyr::arrange(subject_id) |>
  dplyr::mutate(
    .by = subject_id,
    cohort = rep(sample(c("Cohort A", "Cohort B"), 1), length.out = n(), each = length(unique(subject_id))),
    status = ifelse(duplicated(subject_id) | !duplicated(subject_id, fromLast = TRUE), "On Study", "Off Study"),
    status = factor(status, levels = c("On Study", "Off Study"), ordered = T),
    time_end = ifelse(status == "On Study", runif(n(), 1, 10), runif(n(), 11, 20)),
    time_start = ifelse(status == "On Study", 0, NA),
    time_start = ifelse(is.na(time_start), lead(time_end), time_start)
  ) |>
  dplyr::arrange(subject_id, status) |>
  dplyr::relocate(time_start, .before = time_end) |>
  dplyr::mutate(subject_id = factor(subject_id))

set.seed(12345)

adverse_events <- tibble::tibble(subject_id = factor(rep(1:10, length.out = 15))) |>
  sample_n(8) |>
  dplyr::arrange(subject_id) |>
  dplyr::mutate(
    adverse_event_name = rep(sample(c("Infection", "Cardiac Disorder", "Psychiatric Disorder")), length.out = n())
  ) |>
  dplyr::left_join(patient_status, by = "subject_id",
                   relationship = "many-to-many") |> # Temp join to reference time vals
  dplyr::mutate(
    .by = subject_id,
    time_of_event = ave(time_end, adverse_event_name, FUN = function(x) runif(1, 0, x[1]))
  ) |>
  dplyr::select(-c(cohort, status, time_start, time_end)) |>
  unique()

set.seed(12345)

medication_administration <- tibble::tibble(subject_id = factor(rep(1:10, length.out = 15))) |>
  sample_n(5) |>
  dplyr::arrange(subject_id) |>
  dplyr::mutate(
    medication = rep(sample(c("❌", "✅")), length.out = n()),
    name = ifelse(medication == "✅", "Medication A", "Medication B"),
  ) |>
  dplyr::left_join(patient_status, by = "subject_id",
                   relationship = "many-to-many") |> # Temp join to reference time vals
  dplyr::mutate(
    .by = subject_id,
    time_of_event = ave(time_end, medication, FUN = function(x) runif(1, 0, x[1]))
  ) |>
  dplyr::select(-c(cohort, status, time_start, time_end)) |>
  unique()

patient_data <- list(patient_status = patient_status,
                     adverse_events = adverse_events,
                     medication_administration= medication_administration)

usethis::use_data(patient_data, overwrite = TRUE)
