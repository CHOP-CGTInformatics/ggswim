#' @title Create swimmer survival plots
#'
#' @description
#' Use ggplot2 architecture to create a swimmer plot showing subject survival
#' timelines.
#'
#' @details
#' A swimmer plot is a data visualization used to display individual
#' subject data over time. It shows events or outcomes as points along a
#' horizontal line for each subject, allowing easy comparison and pattern
#' identification.
#'
#' @param data a dataframe prepared for use with [geom_swim_lane()]
#' @inheritParams ggplot2::geom_segment
#' @param position Position adjustment. ggswim accepts either "stack", or "identity"
#' depending on the use case. Default "identity".
#'
#' @section Aesthetics:
#' `ggswim()` understands the following aesthetics (required aesthetics are in bold):
#'
#' - **`x`**
#' - **`y`**
#' - **xend _or_ yend**
#' - `alpha`
#' - `colour`
#' - `group`
#' - `linetype`
#' - `linewidth`
#'
#' `geom_swim_lane()` is a wrapper for [geom_segment()] and can support much of the same
#' functionality.
#'
#' **Notes**:
#'
#' - `geom_swim_lane()` **does not** support mapping using `fill`.
#'
#' @section Arrows:
#' Arrows can be added to the ends of swimmer plot lanes as specified in
#' [add_arrows()].
#'
#' @export

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

  structure(
    "A geom_swim_lane layer.",
    class = "swim_lane",
    stat = stat,
    position = position,
    mapping = mapping,
    data = data,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      lineend = lineend,
      linejoin = linejoin,
      arrow = arrow,
      arrow.fill = arrow.fill,
      ... = ...
    )
  )
}

#' @export
ggplot_add.swim_lane <- function(object, plot, object_name) {

  # Enforce checks ----
  mapping <- attr(object, "mapping")

  check_supported_mapping_aes(
    mapping = mapping,
    unsupported_aes = "fill",
    parent_func = "geom_swim_lane()"
  )

  new_layer <- layer(
    data = attr(object, "data"),
    mapping = mapping,
    stat = attr(object, "stat"),
    geom = GeomSwimLane,
    position = attr(object, "position"),
    show.legend = attr(object, "show.legend"),
    key_glyph = "path",
    inherit.aes = attr(object, "inherit.aes"),
    params = attr(object, "params")
  )

  # Add a reference class to the layer attributes
  new_layer$swim_class <- "swim_lane"

  plot$layers <- append(plot$layers, new_layer)

  # Return
  if (!"ggswim_obj" %in% class(plot)) {
    class(plot) <- c("ggswim_obj", class(plot))
  }

  plot
}

#' #' @rdname geom_swim_lane
#' #' @format NULL
#' #' @usage NULL
#' #' @export
GeomSwimLane <- ggproto("GeomSwimLane", Geom,
                    required_aes = c("x", "y", "xend"),
                    non_missing_aes = c("linetype", "linewidth"),
                    default_aes = aes(
                      colour = "black",
                      linewidth = 2,
                      size = 2,
                      linetype = 1,
                      alpha = NA
                    ),
                    draw_panel = function(data, panel_params, coord, arrow = NULL, arrow.fill = NULL,
                                          lineend = "butt", linejoin = "round", na.rm = FALSE) {

                      # Return all components
                      grid::gList(
                        GeomSegment$draw_panel(data, panel_params, coord, arrow = NULL, arrow.fill = NULL,
                                               lineend = "butt", linejoin = "round", na.rm = FALSE)
                      )
                    }
)
