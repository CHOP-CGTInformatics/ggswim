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
#'   ggswim(data = patient_status,
#'          mapping = aes(x = value,
#'                        y = subject_id,
#'                        fill = cohort)) |>
#'   print()
NULL

#' @export
#' @rdname print.ggswim_obj
print.ggswim_obj <- function(x, ...) {
  build_ggswim(x) |> print()
}
