#' Draw ggswim object
#'
#' @param x an object of class 'ggswim_obj'
#' @inheritParams grid::grid.draw
#'
#' @return None
#' @export
#'
#' @examples
#' # TODO: add example
grid.draw.ggswim_obj <- function(x, recording = TRUE) {
  build_ggswim(x) |>
    grid::grid.draw(recording = recording)
}

