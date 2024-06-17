#' @title Add arrows to swimmer plot lanes
#'
#' @description
#' Arrows attached to the end of swimmer plot lanes can be used to denote the
#' continuation of events such as ongoing treatment, implying that the activity
#' or status extends beyond the plotted period.
#'
#' @details
#' Please note that [geom_swim_arrow()] requires a `data` argument and does not
#' inherit data like other functions.
#'
#' @param data A dataframe prepared for use with [geom_swim_arrow()]. Required.
#' @inheritParams ggplot2::geom_segment
#' @param position Position adjustment. ggswim accepts either "stack", or "identity"
#' depending on the use case. Default "identity".
#' @param arrow_colour The color of the arrow head
#' @param arrow_fill The fill color of the arrow head
#' @param arrow_head_length A unit specifying the length of the arrow head
#' (from tip to base).
#' @param arrow_neck_length Value specifying neck length from end of segment
#' to arrow head base
#' @param arrow_type One of "open" or "closed" indicating whether the arrow head
#' should be a closed triangle.
#'
#' @section Aesthetics:
#' [geom_swim_arrow()] understands the following aesthetics (required aesthetics are in bold):
#'
#' - **`y`**
#' - **xend**
#' - `alpha`
#' - `colour`
#' - `group`
#' - `linetype`
#' - `linewidth`
#'
#' [geom_swim_arrow()] is a wrapper for [geom_segment()] and can support much of the same
#' functionality.
#'
#' @examples
#' # Set up data for arrows
#' arrow_data <- patient_data |>
#'   dplyr::left_join(
#'     end_study_events |>
#'       dplyr::select(pt_id, end_study_name),
#'     by = "pt_id"
#'   ) |>
#'   dplyr::select(pt_id, end_time, end_study_name) |>
#'   dplyr::filter(.by = pt_id, end_time == max(end_time)) |>
#'   unique()
#'
#' geom_swim_arrow(
#'   data = arrow_data,
#'   mapping = aes(xend = end_time, y = pt_id),
#'   linewidth = .1,
#'   arrow_neck_length = 5,
#'   arrow_head_length = grid::unit(0.25, "inches"),
#'   arrow_colour = "slateblue",
#'   arrow_fill = "cyan"
#' )
#'
#' @export

geom_swim_arrow <- function(mapping = NULL, data,
                            stat = "identity", position = "identity",
                            ...,
                            arrow_colour = "black",
                            arrow_head_length = unit(0.25, "inches"),
                            arrow_neck_length = NULL,
                            arrow_fill = NULL,
                            arrow_type = "closed",
                            lineend = "butt",
                            linejoin = "round",
                            na.rm = FALSE,
                            show.legend = FALSE) {
  # Set proportional default for arrow_neck_length
  x_val <- retrieve_original_aes(data = data, aes_mapping = mapping, aes_var = "xend")

  if (is.null(arrow_neck_length)) {
    arrow_neck_length <- max(data[[x_val]]) * 0.15
  }

  layer_obj <- layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomSwimArrow,
    position = position,
    show.legend = show.legend,
    params = list2(
      arrow = arrow,
      arrow.fill = arrow_fill,
      arrow_colour = arrow_colour,
      arrow_head_length = arrow_head_length,
      arrow_neck_length = arrow_neck_length,
      arrow_type = arrow_type,
      lineend = lineend,
      linejoin = linejoin,
      na.rm = na.rm,
      ...
    )
  )

  # Add custom attribute and modify class
  attr(layer_obj, "swim_class") <- "swim_arrow"
  class(layer_obj) <- c("swim_arrow", class(layer_obj))

  layer_obj
}

#' @export
ggplot_add.swim_arrow <- function(object, plot, object_name) {
  # TODO: Determine if below better than just:   plot <- plot + new_layer
  plot$layers <- append(plot$layers, object)

  # Return
  if (!"ggswim_obj" %in% class(plot)) {
    class(plot) <- c("ggswim_obj", class(plot))
  }

  plot
}

#' @rdname geom_swim_arrow
#' @format NULL
#' @usage NULL
#' @export
GeomSwimArrow <- ggproto("GeomSwimArrow", GeomSegment,
  required_aes = c("y", "xend"),
  non_missing_aes = c("linetype", "linewidth"),
  optional_aes = c("arrow_colour", "arrow_head_length", "arrow_type", "arrow_neck_length"),
  default_aes = aes(
    colour = "black",
    linewidth = 0.5,
    size = 2,
    linetype = 1,
    alpha = NA
  ),
  setup_data = function(data, params) {
    arrow_neck_length <- params$arrow_neck_length

    # If NULL, neck length to be a 0.15 proportion
    if (is.null(params$arrow_neck_length)) {
      arrow_neck_length <- max(data$xend) * 0.15
    }

    data <- data |>
      mutate(
        x = xend,
        xend = arrow_neck_length + xend
      )

    data
  },
  draw_panel = function(self, data, panel_params, coord, arrow = NULL, arrow.fill = NULL,
                        lineend = "butt", linejoin = "round", na.rm = FALSE) {
    arrow <- arrow(type = data$arrow_type, length = data$arrow_head_length) # Change arrow type and head length
    data$colour <- data$arrow_colour # Change arrow neck and outline color

    # Return all components
    grid::gList(
      GeomSegment$draw_panel(data, panel_params, coord,
        arrow = arrow, arrow.fill = arrow.fill,
        lineend = lineend, linejoin = linejoin, na.rm = na.rm
      )
    )
  }
)
