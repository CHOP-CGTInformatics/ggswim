#' @title Add lanes to swimmer plots
#'
#' @description
#' Lanes in swimmer plots represent individual subjects or entities being tracked
#' over time. Each lane is a horizontal track that displays the duration and
#' sequence of events or activities for the corresponding subject.
#'
#' @param data a dataframe prepared for use with [geom_swim_lane()]
#' @inheritParams ggplot2::geom_segment
#' @param position Position adjustment. ggswim accepts either "stack", or "identity"
#' depending on the use case. Default "identity".
#'
#' @section Aesthetics:
#' [geom_swim_lane()] understands the following aesthetics (required aesthetics are in bold):
#'
#' - **`x`**
#' - **`y`**
#' - **xend**
#' - `alpha`
#' - `colour`
#' - `group`
#' - `linetype`
#' - `linewidth`
#'
#' [geom_swim_lane()] is a wrapper for [geom_segment()] and can support much of the same
#' functionality.
#'
#' **Notes**:
#'
#' - [geom_swim_lane()] **does not** support mapping using `fill`.
#'
#' @section Arrows:
#' Arrows can be added to the ends of swimmer plot lanes as specified in
#' [geom_swim_arrow()].
#'
#' @export
#'
#' @examples
#' patient_data |>
#'   ggplot2::ggplot() +
#'   geom_swim_lane(mapping = aes(
#'     x = start_time, y = pt_id, xend = end_time,
#'     color = disease_assessment
#'   ))
geom_swim_lane <- function(mapping = NULL, data = NULL,
                           stat = "identity", position = "identity",
                           ...,
                           arrow = NULL,
                           arrow.fill = NULL,
                           lineend = "butt",
                           linejoin = "round",
                           na.rm = FALSE,
                           show.legend = NA,
                           inherit.aes = TRUE) {
  layer_obj <- layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomSwimLane,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list2(
      arrow = arrow,
      arrow.fill = arrow.fill,
      lineend = lineend,
      linejoin = linejoin,
      na.rm = na.rm,
      ...
    )
  )

  # Add custom attribute and modify class
  attr(layer_obj, "swim_class") <- "swim_lane"
  class(layer_obj) <- c("swim_lane", class(layer_obj))

  layer_obj
}

#' @export
ggplot_add.swim_lane <- function(object, plot, object_name) {
  # Unpack vars ----
  mapping <- object$mapping

  # Enforce checks ----
  # TODO: Determine if custom error is better than standard ignore warning
  check_supported_mapping_aes(
    mapping = mapping,
    unsupported_aes = "fill",
    parent_func = "geom_swim_lane()"
  )

  # TODO: Determine if below better than just:   plot <- plot + new_layer
  plot$layers <- append(plot$layers, object)

  # Return
  if (!"ggswim_obj" %in% class(plot)) {
    class(plot) <- c("ggswim_obj", class(plot))
  }

  plot
}

#' @rdname geom_swim_lane
#' @format NULL
#' @usage NULL
#' @export
GeomSwimLane <- ggproto("GeomSwimLane", GeomSegment,
  required_aes = c("x", "y", "xend"),
  non_missing_aes = c("linetype", "linewidth"),
  default_aes = aes(
    colour = "black",
    linewidth = 2,
    size = 2,
    linetype = 1,
    shape = 19,
    fill = NA,
    alpha = NA,
    stroke = 1
  ),
  draw_panel = function(self, data, panel_params, coord, arrow = NULL, arrow.fill = NULL,
                        lineend = "butt", linejoin = "round", na.rm = FALSE) {
    # Return all components
    grid::gList(
      GeomSegment$draw_panel(data, panel_params, coord,
        arrow = NULL, arrow.fill = NULL,
        lineend = lineend, linejoin = linejoin, na.rm = FALSE
      )
    )
  }
)
