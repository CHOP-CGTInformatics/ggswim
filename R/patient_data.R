#' @title Patient data
#'
#' @description
#' A sample list dataset containing patient status, adverse event, and
#' medication distribution information. Suitable for a `ggswim()` display.
#'
#' @format A list with the following tibbles:
#' \describe{
#' \item{patient_status}{A 5 column tibble with patient IDs, cohorts, status, and start and end times}
#' \item{adverse_events}{A 4 column tibble with patient IDs, AE names, and time of event}
#' \item{medication_administration}{A 5 column tibble with patient IDs, labels for
#' medications, names of medications, and time of event}
#' }
#'
#' @examples
#' patient_data

"patient_data"

#' @describeIn patient_data A tibble subset from the `patient_data` list
#' @examples
#' patient_status

"patient_status"

#' @describeIn patient_data A tibble subset from the `patient_data` list
#' @examples
#' adverse_events

"adverse_events"

#' @describeIn patient_data A tibble subset from the `patient_data` list
#' @examples
#' medication_administration

"medication_administration"
