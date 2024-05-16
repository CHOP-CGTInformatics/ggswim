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
#' p <- ggplot2::ggplot(data = patient_data) +
#'  geom_swim_lane(
#'   mapping = aes(x = start_time,
#'                 y = pt_id,
#'                 xend = end_time,
#'                 color = disease_assessment)
#'                 )
#' grid.draw(p)
NULL


#' @export
#' @rdname grid.draw.ggswim_obj
grid.draw.ggswim_obj <- function(x, recording = TRUE) { # nolint: object_name_linter
  build_ggswim(x) |>
    grid::grid.draw(recording = recording)
}
