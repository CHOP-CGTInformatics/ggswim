#' @title Patient Dataset
#'
#' @description
#' This dataset provides information about patients, their status, adverse events,
#' and medication administration.
#'
#' @format A list containing three tibbles:
#'   \describe{
#'     \item{patient_status}{A tibble containing information about patient status
#'     including subject ID, cohort, status, name, value, and time sorting.}
#'     \item{adverse_events}{A tibble containing information about adverse
#'     events including subject ID, adverse event name, and time of event.}
#'     \item{medication_administration}{A tibble containing information about
#'     medication administration including subject ID, medication name, name,
#'     and time of event.}
#'   }
#'
#' @usage data(patient_data)
#' @docType data
#' @source This dataset is for demonstration purposes only.
#' @examples
#' patient_data
#'
#' patient_data$patient_status
#'
#' patient_status
#'
#' adverse_events
#'
#' medication_administration
"patient_data"

#' @rdname patient_data
"patient_status"

#' @rdname patient_data
"adverse_events"

#' @rdname patient_data
"medication_administration"
