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
#' \item{medication administration}{A 5 column tibble with patient IDs, labels for
#' medications, names of medications, and time of event}
#' }
#'
#' @examples
#' patient_data

"patient_data"
