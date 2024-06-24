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
  try_ggswim(build_ggswim(x) |> print())
  invisible(x) # Used for pkgdown example rendering
}
