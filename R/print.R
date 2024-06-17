#' @title Print ggswim object
#'
#' @param x a ggswim object
#' @param ... not used
#'
#' @name print.ggswim_obj
#' @return a ggplot
#' @keywords internal
#'
#' @examples
#' print(
#'   ggplot2::ggplot(data = patient_data) +
#'     geom_swim_lane(
#'       mapping = aes(
#'         x = start_time,
#'         y = pt_id,
#'         xend = end_time,
#'         color = disease_assessment
#'       )
#'     )
#' )
NULL

#' @export
#' @rdname print.ggswim_obj
print.ggswim_obj <- function(x, ...) {
  build_ggswim(x) |> print()
}

#' @title Print ggswim layer object
#'
#' @param x a ggswim layer object
#' @param ... not used
#'
#' @name print.ggswim_layer
#' @return a layer object
#' @keywords internal
#'
#' @examples
#' print(
#'   geom_swim_lane(
#'     data = patient_data,
#'     mapping = aes(
#'       x = start_time,
#'       y = pt_id,
#'       xend = end_time,
#'       color = disease_assessment
#'     )
#'   )
#' )
NULL
