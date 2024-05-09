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
#'
#' @examples
#' ggswim(
#'   data = patient_data,
#'   mapping = aes(
#'     x = start_time,
#'     xend = end_time,
#'     y = pt_id,
#'     color = disease_assessment
#'   ),
#'   linewidth = 5
#' )

geom_swim_lane <- function(mapping = NULL, data = NULL,
                      stat = "identity", position = "identity",
                      ..., na.rm = FALSE, show.legend = NA,
                      inherit.aes = TRUE) {

  check_supported_mapping_aes(
    mapping = mapping,
    unsupported_aes = "fill",
    parent_func = "ggswim()"
  )

  out <- layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomSwim,
    position = position,
    show.legend = show.legend,
    key_glyph = "path",
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )

  # Define new class 'ggswim_obj' (after new color scale)
  class(out) <- c("ggswim_obj", class(out))

  # Add a reference class to the layer attributes
  out$swim_class <- "ggswim"

  # Return
  out
}

#' @rdname geom_swim_lane
#' @format NULL
#' @usage NULL
#' @export
GeomSwim <- ggproto("GeomSwim", Geom,
                    required_aes = c("x", "y", "xend"),
                    non_missing_aes = c("linetype", "linewidth"),
                    default_aes = aes(
                      colour = "black",
                      linewidth = 2,
                      size = 2,
                      linetype = 1,
                      alpha = NA
                    ),
                    draw_panel = function(data, panel_params, coord, ...) {

                      # Return all components
                      grid::gList(
                        GeomSegment$draw_panel(data, panel_params, coord, ...)
                      )
                    }
)
