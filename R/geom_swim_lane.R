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
  # Set default mapping if not provided and inherit.aes is TRUE
  if (is.null(mapping) && inherit.aes) {
    mapping <- aes()
  }

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

  layer_obj
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
