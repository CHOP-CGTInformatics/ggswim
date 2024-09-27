#' @title Patient Data dataset
#'
#' @description
#' These datasets provide information about patients, infusion events, and end of
#' study events. Trial data has been de-identified and randomized for general use.
#'
#' @details
#' `patient_data` contains pre-formatted time series data related to disease status
#' markers and status markers that can support arrows. This dataset is most
#' applicable for use with `geom_swim_lane()`.
#'
#' @usage data(patient_data)
#' @docType data
#' @source This dataset is for demonstration purposes only.
#' @examples
#' patient_data
"patient_data"


#' @title Infusion Events dataset
#'
#' @description
#' These datasets provide information about patients, infusion events, and end of
#' study events. Trial data has been de-identified and randomized for general use.
#'
#' @details
#' `infusion_events` contains pre-formatted time series data related to infusions
#' and reinfusions for patients in `patient_data`. This dataset is most applicable
#' for use with `geom_swim_marker()`.
#'
#' @usage data(infusion_events)
#' @docType data
#' @source This dataset is for demonstration purposes only.
#' @examples
#' infusion_events
"infusion_events"

#' @title End of Study Events dataset
#'
#' @description
#' These datasets provide information about patients, infusion events, and end of
#' study events. Trial data has been de-identified and randomized for general use.
#'
#' @details
#' `end_study_events` contains pre-formatted time series data related to end of
#' study events where patients left the trial for varying reasons. This dataset
#' is most applicable for use with `geom_swim_marker()`.
#'
#' @usage data(end_study_events)
#' @docType data
#' @source This dataset is for demonstration purposes only.
#' @examples
#' end_study_events
"end_study_events"
