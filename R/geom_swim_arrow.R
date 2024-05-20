#' @title Add arrows to a swimmer plot
#'
#' @description
#' Add arrows to the ends of swimmer plot lanes for continuation indication.
#'
#' @param data A dataframe prepared for use with [geom_swim_arrow()]
#' @inheritParams ggplot2::geom_segment
#' @param position Position adjustment. ggswim accepts either "stack", or "identity"
#' depending on the use case. Default "identity".
#' @param arrow_colour The color of the arrow head
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
#' - **`x`**
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
#' dplyr::left_join(
#'   end_study_events |>
#'     dplyr::select(pt_id, end_study_name),
#'   by = "pt_id"
#' ) |>
#'   dplyr::select(pt_id, end_time, end_study_name) |>
#'   dplyr::filter(.by = pt_id, end_time == max(end_time)) |>
#'   unique()
#'
#' geom_swim_arrow(
#'   data = arrow_data,
#'   mapping = aes(xend = end_time, y = pt_id),
#'   linewidth = .1,
#'   arrow_neck_length = 5,
#'   arrow_head_length = unit(0.25, "inches"),
#'   arrow_colour = "slateblue",
#'   arrow_fill = "cyan"
#' )
#'
#' @export

geom_swim_arrow <- function(mapping = NULL, data = NULL,
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
                           show.legend = FALSE,
                           inherit.aes = TRUE) {
  structure(
    "A geom_swim_arrow layer.",
    class = "swim_arrow",
    stat = stat,
    position = position,
    mapping = mapping,
    data = data,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    arrow_colour = arrow_colour,
    arrow_head_length = arrow_head_length,
    arrow_neck_length = arrow_neck_length,
    arrow_type = arrow_type,
    params = list(
      na.rm = na.rm,
      lineend = lineend,
      linejoin = linejoin,
      arrow = NULL,
      arrow.fill = arrow_fill,
      ... = ...
    )
  )
}

#' @export
ggplot_add.swim_arrow <- function(object, plot, object_name) {
  # Unpack vars ----
  data <- attr(object, "data")
  mapping <- attr(object, "mapping")
  position <- attr(object, "position")
  arrow_neck_length <- attr(object, "arrow_neck_length")
  arrow_fill <-attr(object, "params")$arrow.fill
  arrow_type <- attr(object, "arrow_type")
  arrow_head_length <- attr(object, "arrow_head_length")

  attr(object, "params")$arrow <- arrow(
    type = arrow_type,
    length = arrow_head_length
  )

  # Copied over add_arrows stuff ==============================================

  # Implement UI checks ----
  # Give warning supplied if `arrow_fill` !NULL and `arrow_type` "open"
  check_arrow_fill_type(arrow_type, arrow_fill)
  # Give error if arrow_neck_length not a name or numeric val
  check_arrow_neck_length(arrow_neck_length)

  x_val <- retrieve_original_aes(data, aes_mapping = unlist(mapping), aes_var = "xend") # nolint: object_usage_linter
  y_val <- retrieve_original_aes(data, aes_mapping = unlist(mapping), aes_var = "y")

  xend <- NULL # define to avoid global variable note

  new_arrow_data <- data |>
    mutate(
      .by = all_of(y_val),
      xend = case_when(
        position == "identity" ~ max(.data[[x_val]], na.rm = TRUE),
        position == "stack" ~ sum(.data[[x_val]], na.rm = TRUE),
        TRUE ~ NA
      )
    )

  # If NULL, neck length to be a 0.15 proportion
  if (is.null(arrow_neck_length)) {
    arrow_neck_length <- max(true_arrow_data$xend) * 0.15
  }

  # Change mapping vals
  new_arrow_mapping <- aes(
    x = xend,
    y = .data[[y_val]],
    xend = arrow_neck_length + xend
  )

  new_layer <- layer(
    data = new_arrow_data,
    mapping = new_arrow_mapping,
    stat = attr(object, "stat"),
    geom = GeomSwimArrow,
    position = attr(object, "position"),
    show.legend = attr(object, "show.legend"),
    inherit.aes = attr(object, "inherit.aes"),
    params = attr(object, "params")
  )

  # Add a reference class to the layer attributes
  new_layer$swim_class <- "swim_arrow"

  # TODO: Determine if below better than just:   plot <- plot + new_layer
  plot$layers <- append(plot$layers, new_layer)

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
GeomSwimArrow <- ggproto("GeomSwimArrow", Geom,
                        required_aes = c("x", "y", "xend"),
                        non_missing_aes = c("linetype", "linewidth"),
                        default_aes = aes(
                          colour = "black",
                          linewidth = 0.5,
                          size = 2,
                          linetype = 1,
                          alpha = NA
                        ),
                        draw_panel = function(data, panel_params, coord, arrow = NULL, arrow.fill = NULL,
                                              lineend = "butt", linejoin = "round", na.rm = FALSE) {
                          # Return all components
                          grid::gList(
                            GeomSegment$draw_panel(data, panel_params, coord,
                                                   arrow = arrow, arrow.fill = arrow.fill,
                                                   lineend = lineend, linejoin = linejoin, na.rm = na.rm
                            )
                          )
                        }
)
