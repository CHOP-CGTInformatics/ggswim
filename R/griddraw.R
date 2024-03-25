#' @title Draw ggswim object
#'
#' @description
#' `grid::grid.draw()` methods for objects of classes 'ggswim_obj' and 'ggcuminc'.
#' These are implemented to allow users to directly call `ggplot2::ggsave()`
#' on 'ggswim' figures.
#'
#' @param x an object of class 'ggswim_obj'
#' @inheritParams grid::grid.draw
#'
#' @return None
#' @keywords internal
#' @name grid.draw.ggswim_obj
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
#'   grid.draw()
NULL


#' @export
#' @rdname grid.draw.ggswim_obj
grid.draw.ggswim_obj <- function(x, recording = TRUE) { # nolint: object_name_linter
  build_ggswim(x) |>
    grid::grid.draw(recording = recording)
}
