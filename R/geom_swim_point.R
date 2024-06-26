#' @title Add point markers to swimmer plots
#'
#' @description
#' Markers are specific symbols or indicators placed on the lanes of a swimmer plot
#' to denote particular events, milestones, or statuses. They provide additional
#' contextual information about significant occurrences during the timeline, such
#' as treatment responses or adverse events.
#'
#' @inheritParams ggplot2::geom_point
#'
#' @section Aesthetics:
#' [geom_swim_point()] understands the following aesthetics (required aesthetics are in bold):
#'
#' - **`x`**
#' - **`y`**
#' - `alpha`
#' - `colour`
#' - `group`
#' - `shape`
#' - `size`
#' - `stroke`
#'
#' [geom_swim_point()] is a wrapper for [geom_point()] and can support much of the same
#' functionality.
#'
#' **Notes**:
#'
#' - [geom_swim_point()] **does not** support mapping using `fill`.
#'
#' @export
#'
#' @examples
#' infusion_events |>
#'   ggplot2::ggplot() +
#'   geom_swim_point(
#'     mapping = aes(
#'       x = time_from_initial_infusion, y = pt_id,
#'       color = infusion_type
#'     ),
#'     size = 5
#'   )
geom_swim_point <- function(mapping = NULL, data = NULL,
                            stat = "identity", position = "identity",
                            ...,
                            na.rm = FALSE,
                            show.legend = NA,
                            inherit.aes = TRUE) {
  layer_obj <- layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomSwimPoint,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list2(
      na.rm = na.rm,
      ...
    )
  )

  # Add custom attribute and modify class
  attr(layer_obj, "swim_class") <- "swim_point"
  class(layer_obj) <- c("swim_point", class(layer_obj))

  layer_obj
}

#' @export
ggplot_add.swim_point <- function(object, plot, object_name) {
  # Unpack vars ----
  mapping <- object$mapping

  # Enforce checks ----
  check_supported_mapping_aes(
    mapping = mapping,
    unsupported_aes = "fill",
    parent_func = "geom_swim_point()"
  )

  object$mapping <- mapping

  plot$layers <- append(plot$layers, object)

  # Return
  if (!"ggswim_obj" %in% class(plot)) {
    class(plot) <- c("ggswim_obj", class(plot))
  }

  plot
}

#' @rdname geom_swim_point
#' @format NULL
#' @usage NULL
#' @export
GeomSwimPoint <- ggproto("GeomSwimPoint", GeomPoint,
  required_aes = c("x", "y"),
  non_missing_aes = c("size", "shape", "colour"),
  default_aes = aes(
    shape = 19, colour = "black", size = 1.5, fill = NA,
    alpha = NA, stroke = 0.5
  ),
  draw_panel = function(self, data, panel_params, coord, ...) {
    # Return all components
    grid::gList(
      GeomPoint$draw_panel(data, panel_params, coord, ...)
    )
  }
)
