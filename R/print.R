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
#' ggswim(
#'   data = patient_data,
#'   mapping = aes(
#'     x = start_time,
#'     xend = end_time,
#'     y = pt_id,
#'     color = disease_assessment
#'   )
#' ) |>
#'   print()
NULL

#' @export
#' @rdname print.ggswim_obj
print.ggswim_obj <- function(x, ...) {
  try_ggswim(build_ggswim(x) |> print())
  invisible(x) # Used for pkgdown example rendering
}
