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
#'     x = delta_t0_months,
#'     y = pt_id,
#'     fill = disease_assessment_status
#'   )
#' ) |>
#'   print()
NULL

#' @export
#' @rdname print.ggswim_obj
print.ggswim_obj <- function(x, ...) {
  build_ggswim(x) |> print()
}
